# Globals and reactives
object_coords <- NULL
obb <- NULL

collect_object <- shiny::reactiveVal(0)
stop_object_collection <- shiny::reactiveVal(0)
remove_object <- shiny::reactiveVal(0)
tags <- shiny::reactiveVal(data.frame(
  label = character(0),
  value = character(0),
  order = numeric(0)
))


# UI
shiny::observeEvent(refresh_display(), {
  if (!trackRcv::is_image(the_image) | is.null(obb)) {
    .toggleTabs(3, "OFF")
    toggled_tabs$toggled[3] <<- FALSE
  } else if (toggled_tabs$toggled[3] == FALSE) {
    .toggleTabs(3, "ON")
    toggled_tabs$toggled[3] <<- TRUE
  }
})


# Display
shiny::observeEvent(input$show_tag, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$show_box, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main %in% c("2", "3")) {
    if (trackRcv::is_image(the_image)) {
      to_display <<- the_image$copy()
      sc <- max(
        c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)) / 720
      )
      r <- sc * 1.5

      if (!is.null(object_coords)) {
        void <- apply(object_coords, 1, function(coords) {
          .drawCircle(
            to_display,
            coords[1],
            coords[2],
            radius = r,
            color = .shades[3:1, which(tags()$label == input$object_tag_x)],
            contrast = c(255, 255, 255),
            thickness = as.integer(max(1, round(r / 2)))
          )
        })
      }

      if (!is.null(obb)) {
        choices <- sort(unique(obb$frame))
        selected <- if (is.null(input$tagged_frame)) {
          1
        } else {
          which(choices == input$video_controls_x)
        }

        shiny::updateSelectInput(
          session,
          "tagged_frame",
          choices = choices,
          selected = choices[selected]
        )

        dt <- obb[frame == input$video_controls_x]

        if (nrow(dt) > 0) {
          if (input$show_box) {
            dt[,
              .drawBox(
                to_display,
                .SD$x,
                .SD$y,
                .SD$width,
                .SD$height,
                .SD$angle,
                color = .shades[3:1, which(tags()$label == .SD$tag)],
                contrast = c(255, 255, 255),
                thickness = 2L, # as.integer(max(1, round(sc))),
                outline = as.integer(max(1, round(sc)))
              ),
              by = seq_len(nrow(dt))
            ]
          } else {
            dt[,
              .drawCircle(
                to_display,
                .SD$x,
                .SD$y,
                radius = r,
                color = .shades[3:1, which(tags()$label == .SD$tag)],
                contrast = c(255, 255, 255),
                thickness = as.integer(max(1, round(r / 2)))
              ),
              by = seq_len(nrow(dt))
            ]
          }

          if (input$show_tag) {
            dt[,
              .drawTag(
                to_display,
                .SD$tag,
                .SD$x,
                .SD$y,
                scale = 0.75,
                color = .shades[3:1, which(tags()$label == .SD$tag)],
                contrast = c(255, 255, 255),
                thickness = 1L, # as.integer(max(1, round(sc))),
                outline = as.integer(max(1, round(sc)))
              ),
              by = seq_len(nrow(dt))
            ]
          }
        }
      }
    } else {
      to_display <<- black_screen$copy()
    }

    print_display(print_display() + 1)
  }
})


# Tag handling
shiny::observeEvent(input$tag_added, {
  tmp <- tags()
  n <- nrow(tmp) + 1
  tmp[n, ]$label <- input$tag_added$label
  tmp[n, ]$value <- input$tag_added$value
  tmp[n, ]$order <- as.numeric(input$tag_added$`$order`)
  tags(tmp)
})

output$tag_list <- shiny::renderTable(
  {
    if (nrow(tags()) > 0) {
      m <- matrix("", nrow = 3, ncol = ceiling(nrow(tags()) / 3))
      colors_id <- ((tags()$order - 1) %% length(pals::alphabet())) + 1
      colors <- pals::alphabet()[colors_id]
      h <- sapply(seq_along(colors), function(i) {
        as.character(
          shiny::span(
            shiny::icon(
              "square",
              class = "fa-solid",
              style = paste0("color: ", colors[i], ";")
            ),
            tags()$label[i]
          )
        )
      })
      m[1:length(h)] <- h
      as.data.frame(t(m))
    }
  },
  colnames = FALSE,
  sanitize.text.function = function(x) x,
  width = "100%"
)


# Add/remove object
shiny::observeEvent(input$qKey, {
  if (input$main == "2" & input$focused != "object_tag_x-selectized") {
    shinyjs::click("add_object", asis = FALSE)
  }
})

shiny::observeEvent(input$add_object, {
  if (trackRcv::is_image(the_image)) {
    if (nchar(input$object_tag_x) > 0) {
      .toggleInputs(input, "OFF")
      .toggleTabs(1:3, "OFF")

      shiny::showNotification(
        "Click at the head and tail of the object to select. Esc to cancel.",
        id = "object_notif",
        duration = NULL,
        type = "message"
      )

      shinyjs::addClass("display", "active_display")
      collect_object(1)
    } else {
      shiny::showNotification(
        "No tag selected. Please create and select at least one tag.",
        id = "object_notif",
        duration = 5,
        type = "error"
      )
    }
  }
})

shiny::observeEvent(input$wKey, {
  if (input$main == "2" & input$focused != "object_tag_x-selectized") {
    shinyjs::click("remove_object", asis = FALSE)
  }
})

shiny::observeEvent(input$remove_object, {
  if (
    trackRcv::is_image(the_image) &
      nrow(obb[frame == input$video_controls_x]) > 0
  ) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1:3, "OFF")

    shiny::showNotification(
      "Click on an object to remove it. Esc to cancel.",
      id = "object_notif",
      duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    remove_object(1)
  }
})

shinyjs::onevent("click", "display_img", function(props) {
  x <- trackRcv::n_col(to_display) * (props$offsetX / input$display_img_width)
  y <- trackRcv::n_row(to_display) * (props$offsetY / input$display_img_height)

  if (collect_object() > 0) {
    object_coords <<- rbind(object_coords, c(round(x), round(y)))
    if (nrow(object_coords) > 1) {
      stop_object_collection(stop_object_collection() + 1)
    }
  } else if (remove_object() > 0) {
    object_coords <<- rbind(object_coords, c(round(x), round(y)))
    if (nrow(object_coords) > 0) {
      stop_object_collection(stop_object_collection() + 1)
    }
  }

  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$escKey, {
  if (collect_object() > 0) {
    object_coords <<- NULL
    stop_object_collection(stop_object_collection() + 1)
  }
})

shiny::observeEvent(stop_object_collection(), {
  if (collect_object() > 0) {
    if (!is.null(object_coords)) {
      w <- round(sqrt(sum(apply(object_coords, 2, diff)^2)))
      w <- if ((w %% 2) == 0) w + 1 else w
      angle <- atan2(
        object_coords[2, 2] - object_coords[1, 2],
        object_coords[2, 1] - object_coords[1, 1]
      ) *
        180 /
        pi
      coords <- apply(object_coords, 2, mean)
      src_pts <- cv2$boxPoints(list(coords, c(w, w), angle))
      dst_pts <- rbind(c(0, 0), c(w, 0), c(w, w), c(0, w))
      affine <- cv2$getAffineTransform(
        np$float32(src_pts[[0:2]]),
        np$float32(dst_pts[1:3, ])
      )
      m <- reticulate::py_to_r(cv2$warpAffine(
        the_image,
        affine,
        dsize = as.integer(c(w, w))
      ))
      dif <- apply(m, 1, function(x) sum((x - m[floor(w / 2) + 1, , ])^2))
      r <- abs(range(which(dif <= median(dif))) - (floor(w / 2) + 1))
      h <- mean(r) * 2
      obb <<- rbind(
        obb,
        data.table::data.table(
          frame = input$video_controls_x,
          tag = input$object_tag_x,
          x = coords[1],
          y = coords[2],
          width = w,
          height = h,
          angle = angle
        )
      )
    }
  } else if (remove_object() > 0) {
    if (!is.null(object_coords)) {
      in_rect <- obb[
        frame == input$video_controls_x,
        .(
          test = .point_in_rectangle(
            object_coords[1],
            object_coords[2],
            unlist(.SD)
          )
        ),
        by = .I,
        .SDcols = c("x", "y", "width", "height", "angle")
      ]

      if (any(in_rect$test)) {
        obb <<- obb[-in_rect[test == TRUE]$I]
      }
    }
  }

  shiny::removeNotification(id = "object_notif")
  .toggleInputs(input, "ON")
  .toggleTabs(1:3, "ON")
  shinyjs::removeClass("display", "active_display")
  collect_object(0)
  remove_object(0)
  object_coords <<- NULL
  refresh_display(refresh_display() + 1)
})


# Random frame selection
shiny::observeEvent(input$rKey, {
  if (input$main == "2" & input$focused != "object_tag_x-selectized") {
    shinyjs::click("random_frame", asis = FALSE)
  }
})

shiny::observeEvent(input$random_frame, {
  if (is.null(obb)) {
    random_frame <- sample(1:trackRcv::n_frames(the_video), 1)
  } else {
    a <- 1:trackRcv::n_frames(the_video)
    b <- unique(obb$frame)
    d <- apply(outer(a, b, function(x, y) (x - y)^2), 1, min)
    random_frame <- sample(1:trackRcv::n_frames(the_video), 1, prob = d)
  }

  shinyWidgets::updateNoUiSliderInput(
    session,
    "video_controls_x",
    value = random_frame
  )
})

# Stats
output$stats <- shiny::renderTable(
  {
    refresh_display()
    dat <- data.frame(
      name = c(
        "<b>Tagged frames: </b>",
        "<b>Tagged objects: </b>",
        "<b>Tagged objects in this frame: </b>"
      )
    )
    if (is.null(obb)) {
      dat$stat <- rep(0L, 3)
    } else {
      dat$stat <- c(
        length(unique(obb$frame)),
        nrow(obb),
        nrow(obb[frame == input$video_controls_x])
      )
    }
    dat
  },
  striped = TRUE,
  colnames = FALSE,
  sanitize.text.function = function(x) x,
  width = "100%"
)
