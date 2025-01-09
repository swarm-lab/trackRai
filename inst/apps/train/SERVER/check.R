# UI
observeEvent(theVideo(), {
  if (isVideo(theVideo())) {
    updateSliderInput(session, "frame_x", min = 1, max = nframes(theVideo()), value = 1)
  } else {
    updateSliderInput(session, "frame_x", min = 0, max = 1, value = 0)
  }
})


# Events
observeEvent(theModel(), {
  if (is.null(theModel())) {
    theVideo(NULL)
  } else {
    theTempFrame <<- tempfile(fileext = ".jpg")
    theTempPredict <<- tempdir()
    print(theTempPredict)
    theVideo(video(paste0(theYOLOPath(), "/video.mp4")))
  }
})

observeEvent(input$frame_x, {
  if (isVideo(theVideo())) {
    if (isImage(theFrame)) {
      readFrame(theVideo(), input$frame_x, theFrame)
    } else {
      theFrame <<- readFrame(theVideo(), input$frame_x)
    }
    suppressMessages(write.Image(theFrame, theTempFrame, TRUE))
    refreshFrame(refreshFrame() + 1)
  }
})

# Plotting
output$displayImg <- renderImage(
  {
    if (refreshFrame() > 0) {
      showNotification("Computing check image", id = "check", duration = NULL)

      unlink(list.files(theTempPredict, "predict", full.names = TRUE), recursive = TRUE)

      dump <- system2(
        .yolo_path(),
        c(
          "obb",
          "predict",
          paste0("model=", theModel(), "/weights/best.pt"),
          paste0("imgsz=", ncol(theFrame)),
          paste0("max_det="), input$maxObjects_x,
          paste0("source=", theTempFrame),
          paste0("project=", theTempPredict),
          "show_labels=False",
          "show_conf=False",
          "save=False",
          "save_txt=True"
        )
      )

      pred <- fread(
        list.files(paste0(theTempPredict, "/predict/labels"), full.names = TRUE),
        col.names = c("class", "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")
      )
      box <- pred[, rectPoints(x1, y1, x2, y2, x3, y3, x4, y4)]
      drawRotatedRectangle(
        theFrame, box$x, box$y, box$width, box$height, box$angle, "blue", 2
      )
      suppressMessages(write.Image(theFrame, theTempFrame, TRUE))

      iw <- ncol(theFrame)
      ih <- nrow(theFrame)
      ww <- session$clientData[["output_displayImg_width"]] - 20
      wh <- (session$clientData[["output_displayImg_width"]] - 20) * ih / iw

      removeNotification("check")

      list(
        src = theTempFrame,
        width = ww,
        height = wh
      )
    }
  },
  deleteFile = FALSE
)
