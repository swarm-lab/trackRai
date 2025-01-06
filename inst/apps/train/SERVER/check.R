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
    theTempFrame <<- tempfile(fileext = ".bmp")
    theTempPredict <<- tempdir()
    print(theTempPredict)
    theVideo(video(paste0(theYOLOPath(), "/video.mp4")))
  }
})

# observeEvent(input$main, {
#   if (input$main == "2") {
#     theTempFrame <<- tempfile(fileext = ".bmp")
#     theVideo(video(paste0(theYOLOPath(), "/video.mp4")))
#   }
# })

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
      unlink(list.files(theTempPredict, "predict", full.names = TRUE), recursive = TRUE)

      dump <- system2(
        .yolo_path(), 
        c(
          "obb",
          "predict",
          paste0("model=", theModel(), "/weights/best.pt"),
          paste0("imgsz=", ncol(theFrame)),
          paste0("source=", theTempFrame),
          paste0("project=", theTempPredict)
        )
      )
      
      pred <- paste0(theTempPredict, "/predict/", list.files(paste0(theTempPredict, "/predict/")))

      iw <- ncol(pred)
      ih <- nrow(pred)
      ww <- session$clientData[["output_displayImg_width"]] - 20
      wh <- (session$clientData[["output_displayImg_width"]] - 20) * ih / iw

      list(
        src = pred,
        width = ww,
        height = wh
      )
    }
  },
  deleteFile = FALSE
)
