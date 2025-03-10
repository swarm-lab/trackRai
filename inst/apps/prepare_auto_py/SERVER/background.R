# Status
output$background_status <- shiny::renderUI({
  if ((refresh_display() > -1 | input$computeBackground_x > -1) &
    !trackRai::is_image(the_background)) {
    toggleTabs(3:6, "OFF")
    toggled_tabs$toggled[3:6] <<- FALSE
    p("Background missing (and required).", class = "bad")
  } else {
    if (toggled_tabs$toggled[3] == FALSE) {
      toggleTabs(3, "ON")
      toggled_tabs$toggled[3] <<- TRUE
      NULL
    }
  }
})


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "2") {
    if (trackRai::is_image(the_background)) {
      to_display <<- the_background$copy()
      sc <- max(c(trackRai::n_row(to_display), trackRai::n_col(to_display)) / 720)
      r <- 0.01 * min(trackRai::n_row(to_display), trackRai::n_col(to_display))

      if (collect_ghost() > 0) {
        if (nrow(ghost_coords) > 1) {
          cv2$polylines(
            to_display, array(as.integer(ghost_coords), c(1, dim(ghost_coords))),
            TRUE, c(255, 255, 255), as.integer(max(1, 1.5 * sc))
          )
        }

        if (nrow(ghost_coords) > 0) {
          for (i in seq_len(nrow(ghost_coords))) {
            cv2$circle(
              to_display, as.integer(ghost_coords[i, ]), as.integer(r * 1.5),
              c(255L, 255L, 255L), -1L
            )
            cv2$circle(
              to_display, as.integer(ghost_coords[i, ]), as.integer(r),
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


# Load existing background
shinyFiles::shinyFileChoose(input, "backgroundFile_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$backgroundFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$backgroundFile_x)
  if (nrow(path) > 0) {
    background_path(normalizePath(path$datapath, mustWork = FALSE))
    refresh_background(refresh_background() + 1)
  }
})

shiny::observeEvent(refresh_background(), {
  if (refresh_background() > 0) {
    to_check <- cv2$imread(background_path())

    if (trackRai::is_image(to_check)) {
      if (!all(unlist(reticulate::py_to_r(to_check$shape)) == unlist(reticulate::py_to_r(the_image$shape)))) {
        shinyalert::shinyalert("Error:",
          "The video and background do not have the same shape.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        the_background <<- NULL
      } else {
        the_background <<- to_check$copy()
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(background_path(), volumes),
          function(l) {
            if (nrow(l) > 0) {
              diff(l[1, ])
            } else {
              NA
            }
          }
        )
      )

      volume <- volumes[ix]
      dir <- dirname(background_path())
      default_root(names(volumes)[ix])
      default_path(gsub(paste0(".*", volume), "", dir))

      refresh_display(refresh_display() + 1)
    }
  }
})


# Compute background estimate
shiny::observeEvent(input$computeBackground_x, {
  if (trackRai::is_video_capture(the_video)) {
    showElement("curtain")
    the_background <<- np$uint8(
      trackRai::backgrounder(the_video,
        n = input$backroundImages_x,
        method = input$backgroundType_x,
        start = input$video_controls[1],
        end = input$video_controls[3]
      )
    )

    hideElement("curtain")
    refresh_display(refresh_display() + 1)
  }
})


# Remove ghosts
shiny::observeEvent(input$ghostButton_x, {
  if (trackRai::is_image(the_background)) {
    toggleInputs(input, "OFF")
    toggleTabs(1, "OFF")

    shiny::showNotification("Click to draw a polygon around the object to remove from
                     the image. Enter to stop. Esc to cancel.",
      id = "ghost_notif",
      duration = NULL, type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_ghost(1)
  }
})

.point_in_rectangle <- function(x, y, rect) {
  l <- list(c(rect[1], rect[2]), c(rect[3], rect[4]), rect[5])
  box <- reticulate::py_to_r(cv2$boxPoints(reticulate::r_to_py(l)))
  pracma::inpolygon(x, y, box[, 1], box[, 2], TRUE)
}

shinyjs::onevent("click", "display_img", function(props) {
  if (collect_ghost() > 0) {
    x <- trackRai::n_col(to_display) * (props$offsetX / input$display_img_width)
    y <- trackRai::n_row(to_display) * (props$offsetY / input$display_img_height)
    ghost_coords <<- rbind(ghost_coords, c(x, y))
    refresh_display(refresh_display() + 1)
  } else if (collect_mask() > 0) {
    x <- trackRai::n_col(to_display) * (props$offsetX / input$display_img_width)
    y <- trackRai::n_row(to_display) * (props$offsetY / input$display_img_height)
    mask_coords <<- rbind(mask_coords, c(x, y))

    if (collect_mask() == 2 & nrow(mask_coords) >= 5) {
      stop_mask_collection(stop_mask_collection() + 1)
    }

    refresh_display(refresh_display() + 1)
  } else if (input$main == "5" & !is.null(the_stats())) {
    px <- trackRai::n_col(to_display) * (props$offsetX / input$display_img_width)
    py <- trackRai::n_row(to_display) * (props$offsetY / input$display_img_height)

    dt <- the_stats()
    in_rect <- dt[frame == the_frame(),
      .(test = .point_in_rectangle(px, py, unlist(.SD))),
      by = .I, .SDcols = c("x", "y", "width", "height", "angle")
    ]

    if (any(in_rect$test)) {
      ix <- which(in_rect$test)

      for (i in ix) {
        mod <- dt[frame == the_frame()]$mod[i]
        if (mod == 0) {
          if (dt[frame == the_frame()]$select_w[i] & dt[frame == the_frame()]$select_h[i]) {
            dt[frame == the_frame()]$mod[i] <- 2
          } else {
            dt[frame == the_frame()]$mod[i] <- 1
          }
        } else {
          if (mod == 1) {
            dt[frame == the_frame()]$mod[i] <- 2
          } else {
            dt[frame == the_frame()]$mod[i] <- 1
          }
        }
      }
    }

    the_stats(dt)
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$retKey, {
  if (collect_ghost() > 0) {
    stop_ghost_collection(stop_ghost_collection() + 1)
  }
})

shiny::observeEvent(input$escKey, {
  if (collect_ghost() > 0) {
    ghost_coords <<- NULL
    stop_ghost_collection(stop_ghost_collection() + 1)
  }
})

shiny::observeEvent(stop_ghost_collection(), {
  if (collect_ghost() > 0) {
    if (nrow(ghost_coords) > 0) {
      roi <- reticulate::np_array(
        array(0L, c(trackRai::n_row(the_background), trackRai::n_col(the_background), 1)),
        dtype = "uint8"
      )
      cv2$fillPoly(
        roi,
        pts = array(as.integer(ghost_coords), c(1, dim(ghost_coords))),
        color = c(255, 255, 255)
      )
      the_background <<- cv2$inpaint(the_background, roi, 5, cv2$INPAINT_TELEA)
    }

    shiny::removeNotification(id = "ghost_notif")
    toggleInputs(input, "ON")
    toggleTabs(1, "ON")
    shinyjs::removeClass("display", "active_display")
    collect_ghost(0)
    ghost_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})


# Save background
shinyFiles::shinyFileSave(input, "save_background_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$save_background_x, {
  path <- shinyFiles::parseSavePath(volumes, input$save_background_x)

  if (trackRai::is_image(the_background) & nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    cv2$imwrite(path, the_background)
    background_path(path)
  }
})
