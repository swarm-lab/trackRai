# Display
.shades <- col2rgb(pals::alphabet())[3:1, ]
mode(.shades) <- "integer"

.drawBoxes <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    box <- array(c(SD$x1, SD$x2, SD$x3, SD$x4, SD$y1, SD$y2, SD$y3, SD$y4), dim = c(nrow(SD), 4, 2))
    mode(box) <- "integer"
    cv2$drawContours(
      img, list(reticulate::r_to_py(box)), 0L,
      .shades[, (BY$id %% ncol(.shades)) + 1],
      as.integer(linewidth)
    )
  }
  NULL
}

.drawWhiteBox <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    box <- array(c(SD$x1, SD$x2, SD$x3, SD$x4, SD$y1, SD$y2, SD$y3, SD$y4), dim = c(nrow(SD), 4, 2))
    mode(box) <- "integer"
    cv2$drawContours(
      img, list(reticulate::r_to_py(box)), 0L,
      c(255L, 255L, 255L),
      as.integer(linewidth)
    )
  }
  NULL
}

shiny::observeEvent(refresh_display(), {
  if (input$main == "2") {
    if (trackRai::trackRcv::is_image(the_image)) {
      to_display <<- the_image$copy()
      r <- 0.005 * min(c(trackRai::trackRcv::n_row(to_display), trackRai::trackRcv::n_col(to_display)))
      sc <- max(c(trackRai::trackRcv::n_row(to_display), trackRai::trackRcv::n_col(to_display)) / 720)

      if (!is.null(objects_obb)) {
        void <- objects_obb[frame == the_frame() & id == selected(),
          .drawWhiteBox(to_display, .SD, .BY, as.integer(max(1, 3 * sc))),
          by = .(id), .SDcols = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")
        ]
        void <- objects_obb[frame == the_frame(),
          .drawBoxes(to_display, .SD, .BY, as.integer(max(0.5, sc))),
          by = .(id), .SDcols = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")
        ]
      }

      if (collect_object() > 0) {
        if (nrow(object_coords) > 0) {
          for (i in seq_len(nrow(object_coords))) {
            cv2$circle(
              to_display, as.integer(object_coords[i, ]), as.integer(r * 1.5),
              c(255L, 255L, 255L), -1L
            )
            cv2$circle(
              to_display, as.integer(object_coords[i, ]), as.integer(r),
              c(0L, 0L, 255L), -1L
            )
          }
        }
      }
    } else {
      to_display <<- black_screen$copy()
    }

    print_display(print_display() + 1)
  }
})


# Collect/select objects
shiny::observeEvent(input$new_object_x, {
  if (trackRai::trackRcv::is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1, "OFF")
    shinyjs::addClass("display", "active_display")

    shiny::showNotification("First, click at the head and tail of an object.
                              Then, click on the left and right of the object.
                              Esc to cancel.",
      id = "object_notif",
      duration = NULL, type = "message"
    )

    collect_object(1)
  }
})

.point_in_rectangle <- function(p, rectangle) {
  x <- p[1]
  y <- p[2]
  xp <- rectangle[1:4]
  yp <- rectangle[5:8]
  pracma::inpolygon(x, y, xp, yp, TRUE)
}

shinyjs::onevent("click", "display_img", function(props) {
  if (collect_object() > 0) {
    x <- trackRai::trackRcv::n_col(to_display) * (props$offsetX / input$display_img_width)
    y <- trackRai::trackRcv::n_row(to_display) * (props$offsetY / input$display_img_height)
    object_coords <<- rbind(object_coords, c(x, y))

    if (nrow(object_coords) >= 4) {
      stop_object_collection(stop_object_collection() + 1)
    } else {
      refresh_display(refresh_display() + 1)
    }
  } else {
    if (!is.null(objects_obb)) {
      if (nrow(objects_obb[frame == the_frame(), ]) > 0) {
        x <- trackRai::trackRcv::n_col(to_display) * (props$offsetX / input$display_img_width)
        y <- trackRai::trackRcv::n_row(to_display) * (props$offsetY / input$display_img_height)

        in_poly <- objects_obb[frame == the_frame(),
          .(test = .point_in_rectangle(c(x, y), unlist(.SD))),
          by = .(id), .SDcols = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")
        ]

        if (any(in_poly$test)) {
          selected(in_poly$id[in_poly$test][1])
        }
      }
    }

    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$escKey, {
  if (collect_object() > 0) {
    object_coords <<- NULL
    stop_object_collection(stop_object_collection() + 1)
  }
})

# .oriented_rectangle <- function(p1, p2, width, height) {
#   com <- (p1 + p2) / 2
#   dx <- p2[1] - p1[1]
#   dy <- p2[2] - p1[2]
#   length <- sqrt(dx^2 + dy^2)
#   ux <- dx / length
#   uy <- dy / length
#   vx <- -uy
#   vy <- ux
#   half_width_vec <- (width / 2) * c(vx, vy)
#   half_height_vec <- (height / 2) * c(ux, uy)
#   corner1 <- com - half_width_vec - half_height_vec
#   corner2 <- com + half_width_vec - half_height_vec
#   corner3 <- com + half_width_vec + half_height_vec
#   corner4 <- com - half_width_vec + half_height_vec
#   c(corner1, corner2, corner3, corner4)
# }

.oriented_rectangle <- function(p) {
  u <- p[2, ] - p[1, ]
  height <- sqrt(u[1]^2 + u[2]^2)
  v_norm <- u[2:1] * c(-1, 1) / height

  v1 <- p[3, ] - p[2, ]
  m1 <- cbind(u, v1)
  d1 <- abs(det(m1)) / sqrt(sum(u * u))

  v2 <- p[4, ] - p[2, ]
  m2 <- cbind(u, v2)
  d2 <- abs(det(m2)) / sqrt(sum(u * u))

  corner1 <- p[1, ] + v_norm * d1
  corner2 <- p[1, ] - v_norm * d2
  corner3 <- p[2, ] - v_norm * d2
  corner4 <- p[2, ] + v_norm * d1
  c(corner1, corner2, corner3, corner4)
}


dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
 } 

shiny::observeEvent(stop_object_collection(), {
  if (collect_object() > 0) {
    if (!is.null(object_coords)) {
      if (nrow(object_coords) > 0) {
        h <- sqrt(diff(object_coords[, 1])^2 + diff(object_coords[, 2])^2)

        obb <- data.table::as.data.table(t(c(
          the_frame(),
          id_counter,
          object_coords[1, ],
          object_coords[2, ],
          # .oriented_rectangle(object_coords[1, ], object_coords[2, ], round(h / input$hw_ratio_x), round(h))
          .oriented_rectangle(object_coords)
        )))
        names(obb) <- c("frame", "id", "p1_x", "p1_y", "p2_x", "p2_y", "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")
        selected(id_counter)
        id_counter <<- id_counter + 1

        objects_obb <<- data.table::rbindlist(
          list(objects_obb, obb)
        )

        shiny::updateSelectInput(
          session, "tagged_frame_x",
          choices = sort(unique(objects_obb$frame)), selected = the_frame()
        )
      }
    }

    shiny::removeNotification(id = "object_notif")
    .toggleInputs(input, "ON")
    .toggleTabs(1, "ON")
    shinyjs::removeClass("display", "active_display")
    collect_object(0)
    object_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(selected(), {
  if (!is.null(objects_obb)) {
    selected_obb <- objects_obb[frame == the_frame() & id == selected(), ]

    if (nrow(selected_obb) > 0) {
      h <- sqrt((selected_obb$x1 - selected_obb$x4)^2 + (selected_obb$y1 - selected_obb$y4)^2)
      w <- sqrt((selected_obb$x1 - selected_obb$x2)^2 + (selected_obb$y1 - selected_obb$y2)^2)
      shiny::updateNumericInput(session, "height_x", value = round(h))
      shiny::updateNumericInput(session, "width_x", value = round(w))
    }
  }
})

# shiny::observeEvent(input$height_x, {
#   if (!is.null(objects_obb)) {
#     ix <- which(objects_obb$frame == the_frame() & objects_obb$id == selected())

#     if (length(ix) > 0) {
#       objects_obb[ix, names(objects_obb) := as.list(
#         c(
#           the_frame(),
#           objects_obb[ix, ]$id,
#           objects_obb[ix, ]$p1_x,
#           objects_obb[ix, ]$p1_y,
#           objects_obb[ix, ]$p2_x,
#           objects_obb[ix, ]$p2_y,
#           .oriented_rectangle(
#             c(objects_obb[ix, ]$p1_x, objects_obb[ix, ]$p1_y),
#             c(objects_obb[ix, ]$p2_x, objects_obb[ix, ]$p2_y),
#             input$width_x, input$height_x
#           )
#         )
#       )]

#       refresh_display(refresh_display() + 1)
#     }
#   }
# })

# shiny::observeEvent(input$width_x, {
#   if (!is.null(objects_obb)) {
#     ix <- which(objects_obb$frame == the_frame() & objects_obb$id == selected())

#     if (length(ix) > 0) {
#       objects_obb[ix, names(objects_obb) := as.list(
#         c(
#           the_frame(),
#           objects_obb[ix, ]$id,
#           objects_obb[ix, ]$p1_x,
#           objects_obb[ix, ]$p1_y,
#           objects_obb[ix, ]$p2_x,
#           objects_obb[ix, ]$p2_y,
#           .oriented_rectangle(
#             c(objects_obb[ix, ]$p1_x, objects_obb[ix, ]$p1_y),
#             c(objects_obb[ix, ]$p2_x, objects_obb[ix, ]$p2_y),
#             input$width_x, input$height_x
#           )
#         )
#       )]

#       refresh_display(refresh_display() + 1)
#     }
#   }
# })

shiny::observeEvent(input$remove_object_x, {
  if (!is.null(objects_obb)) {
    ix <- which(objects_obb$frame == the_frame() & objects_obb$id == selected())

    if (length(ix) > 0) {
      objects_obb <<- objects_obb[-ix, ]
    }

    refresh_display(refresh_display() + 1)
  }
})

output$frame_count <- shiny::renderText({
  if (refresh_display() > 0) {
    if (!is.null(objects_obb)) {
      as.character(length(unique(objects_obb$frame)))
    } else {
      "0"
    }
  } else {
    "0"
  }
})

output$object_count <- shiny::renderText({
  if (refresh_display() > 0) {
    if (!is.null(objects_obb)) {
      as.character(nrow(objects_obb))
    } else {
      "0"
    }
  } else {
    "0"
  }
})

shiny::observeEvent(input$newKey, {
  shinyjs::click("new_object_x")
})

shiny::observeEvent(input$remKey, {
  shinyjs::click("remove_object_x")
})

shiny::observeEvent(input$tagged_frame_x, {
  if (!is.null(the_frame())) {
    if (input$tagged_frame_x != the_frame()) {
      new_value <- input$video_controls_x
      new_value[2] <- input$tagged_frame_x
      shinyWidgets::updateNoUiSliderInput(
        session, "video_controls_x",
        value = new_value
      )
    }
  }
})
