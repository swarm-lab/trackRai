# UI
shiny::observeEvent(theVideo(), {
  if (trackRai::is_video_capture(theVideo())) {
    shiny::updateSliderInput(session, "frame_x", min = 1, max = trackRai::n_frames(theVideo()), value = 1)
  } else {
    shiny::updateSliderInput(session, "frame_x", min = 0, max = 1, value = 0)
  }
})


# Events
shiny::observeEvent(theModel(), {
  if (is.null(theModel())) {
    theVideo(NULL)
  } else {
    theTempFrame <<- tempfile(fileext = ".jpg")
    theTempPredict <<- tempdir()
    theVideo(cv2$VideoCapture(normalizePath(paste0(theYOLOPath(), "/video.mp4"))))
  }
})

shiny::observeEvent(input$frame_x, {
  if (trackRai::is_video_capture(theVideo())) {
    theFrame <<- trackRai::read_frame(theVideo(), input$frame_x)
    cv2$imwrite(normalizePath(theTempFrame, mustWork = FALSE), theFrame)
    refreshFrame(refreshFrame() + 1)
  }
})

# Plotting
output$displayFrame <- shiny::renderUI({
  if (refreshFrame() > 0) {
    if (trackRai::is_image(theFrame)) {
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

      sc <- max(c(trackRai::n_row(theFrame), trackRai::n_col(theFrame)) / 720)

      pred <- fread(
        list.files(paste0(theTempPredict, "/predict/labels"), full.names = TRUE),
        col.names = c("class", "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")
      )

      for (i in seq_len(nrow(pred))) {
        arr <- array(NA, c(4, 1, 2))
        arr[, , 1] <- pred[i, c(2, 4, 6, 8)]
        arr[, , 2] <- pred[i, c(3, 5, 7, 9)]
        theFrame <<- cv2$drawContours(
          theFrame, list(np_array(arr)), 0L, c(255L, 255L, 255),
          as.integer(max(0.5, 4 * sc))
        )
        theFrame <<- cv2$drawContours(
          theFrame, list(np_array(arr)), 0L, c(0L, 224L, 0L),
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

# output$displayImg <- renderImage(
#   {
#     if (refreshFrame() > 0) {
#       showNotification("Computing check image", id = "check", duration = NULL)

#       unlink(list.files(theTempPredict, "predict", full.names = TRUE), recursive = TRUE)

#       dump <- system2(
#         .yolo_path(),
#         c(
#           "obb",
#           "predict",
#           paste0("model=", theModel(), "/weights/best.pt"),
#           paste0("imgsz=", ncol(theFrame)),
#           paste0("max_det="), input$maxObjects_x,
#           paste0("source=", theTempFrame),
#           paste0("project=", theTempPredict),
#           "show_labels=False",
#           "show_conf=False",
#           "save=False",
#           "save_txt=True"
#         )
#       )

#       pred <- fread(
#         list.files(paste0(theTempPredict, "/predict/labels"), full.names = TRUE),
#         col.names = c("class", "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")
#       )
#       box <- pred[, rectPoints(x1, y1, x2, y2, x3, y3, x4, y4)]
#       drawRotatedRectangle(
#         theFrame, box$x, box$y, box$width, box$height, box$angle, "blue", 2
#       )
#       suppressMessages(write.Image(theFrame, theTempFrame, TRUE))

#       iw <- ncol(theFrame)
#       ih <- nrow(theFrame)
#       ww <- session$clientData[["output_displayImg_width"]] - 20
#       wh <- (session$clientData[["output_displayImg_width"]] - 20) * ih / iw

#       removeNotification("check")

#       list(
#         src = theTempFrame,
#         width = ww,
#         height = wh
#       )
#     }
#   },
#   deleteFile = FALSE
# )
