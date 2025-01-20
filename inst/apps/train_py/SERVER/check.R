# UI
shiny::observeEvent(theVideo(), {
  if (trackRai::is_video_capture(theVideo())) {
    shiny::updateSliderInput(session, "frame_x", min = 1, max = trackRai::n_frames(theVideo()), value = 1)
  } else {
    shiny::updateSliderInput(session, "frame_x", min = 0, max = 1, value = 0)
  }
})


# Events
shiny::observeEvent(theModelFolder(), {
  if (is.null(theModelFolder())) {
    theVideo(NULL)
  } else {
    theModel(ultralytics$YOLO(normalizePath(paste0(theModelFolder(), "/weights/best.pt"), mustWork = FALSE)))
    theVideo(cv2$VideoCapture(normalizePath(paste0(theYOLOPath(), "/video.mp4"), mustWork = FALSE)))
  }
})

shiny::observeEvent(input$frame_x, {
  if (trackRai::is_video_capture(theVideo())) {
    theFrame <<- trackRai::read_frame(theVideo(), input$frame_x)
    refreshFrame(refreshFrame() + 1)
  }
})

# Plotting
output$displayFrame <- shiny::renderUI({
  if (refreshFrame() > 0) {
    if (trackRai::is_image(theFrame)) {
      showNotification("Computing check image", id = "check", duration = NULL)

      pred <- theModel()(theFrame)
      obb <- pred[0]$obb$xyxyxyxy$cpu()$numpy()
      obb <- np$int_(obb)
      sc <- max(c(trackRai::n_row(theFrame), trackRai::n_col(theFrame)) / 720)

      for (i in seq_len(py_to_r(obb$shape[0]))) {
        theFrame <<- cv2$drawContours(
          theFrame, list(obb[i-1]), 0L, c(255L, 255L, 255),
          as.integer(max(0.5, 4 * sc))
        )
        theFrame <<- cv2$drawContours(
          theFrame, list(obb[i-1]), 0L, c(0L, 224L, 0L),
          as.integer(max(0.5, 2 * sc))
        )
      }

      removeNotification("check")

      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", theFrame)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "displayImg",
        draggable = "false"
      )
    } else {
      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", black_screen)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "displayImg",
        draggable = "false"
      )
    }
  }
})
