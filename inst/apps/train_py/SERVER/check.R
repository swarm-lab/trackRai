# Display
output$displayFrame <- shiny::renderUI({
  if (refresh_frame() > 0) {
    if (trackRai::is_image(the_frame)) {
      shiny::showNotification("Computing check image", id = "check", duration = NULL)

      pred <- the_model()(the_frame, verbose = FALSE)
      obb <- pred[0]$obb$xyxyxyxy$cpu()$numpy()
      obb <- np$int_(obb)
      sc <- max(c(trackRai::n_row(the_frame), trackRai::n_col(the_frame)) / 720)

      for (i in seq_len(py_to_r(obb$shape[0]))) {
        the_frame <<- cv2$drawContours(
          the_frame, list(obb[i-1]), 0L, c(255L, 255L, 255),
          as.integer(max(0.5, 4 * sc))
        )
        the_frame <<- cv2$drawContours(
          the_frame, list(obb[i-1]), 0L, c(0L, 224L, 0L),
          as.integer(max(0.5, 2 * sc))
        )
      }

      shiny::removeNotification("check")

      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", the_frame)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    } else {
      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", black_screen)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    }
  }
})


# UI
shiny::observeEvent(the_video(), {
  if (trackRai::is_video_capture(the_video())) {
    shiny::updateSliderInput(session, "frame_x", min = 1, max = trackRai::n_frames(the_video()), value = 1)
  } else {
    shiny::updateSliderInput(session, "frame_x", min = 0, max = 1, value = 0)
  }
})


# Predict
shiny::observeEvent(the_model_folder(), {
  if (is.null(the_model_folder())) {
    the_video(NULL)
  } else {
    the_model(ultralytics$YOLO(normalizePath(paste0(the_model_folder(), "/weights/best.pt"), mustWork = FALSE)))
    the_video(cv2$VideoCapture(normalizePath(paste0(yolo_path(), "/video.mp4"), mustWork = FALSE)))
  }
})

shiny::observeEvent(input$frame_x, {
  if (trackRai::is_video_capture(the_video())) {
    the_frame <<- trackRai::read_frame(the_video(), input$frame_x)
    refresh_frame(refresh_frame() + 1)
  }
})
