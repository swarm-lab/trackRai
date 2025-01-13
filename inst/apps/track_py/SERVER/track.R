# Status
output$yoloStatus <- shiny::renderUI({
  if (!yolo_installed) {
    p(
      "No YOLO installation was detected.",
      tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  }
})


# UI
output$modelSelect <- shiny::renderUI({
  if (!is.null(theModelFolder())) {
    models <- list.files(paste0(theModelFolder(), "/runs/obb/"))
    shiny::tagList(
      hr(),
      shiny::selectInput("model_x", "Select trained model:",
      paste0(models, "/weights/best.pt"),
      width = "100%"
    )
    )
  }
})

output$rangeSlider <- shiny::renderUI({
  if (refreshVideo() > 0 & trackRai::is_video_capture(theVideo)) {
    shiny::sliderInput("rangePos_x", "Video range",
      width = "100%", min = 1,
      max = n_frames(theVideo),
      value = c(1, n_frames(theVideo)), step = 1
    )
  }
})

output$videoSlider <- shiny::renderUI({
  if (refreshVideo() > 0 & !is.null(input$rangePos_x) & trackRai::is_video_capture(theVideo)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      shiny::sliderInput("videoPos_x", "Frame",
        width = "100%", step = 1,
        value = input$rangePos_x[2],
        min = input$rangePos_x[1],
        max = input$rangePos_x[2]
      )
    } else {
      shiny::sliderInput("videoPos_x", "Frame",
        width = "100%", step = 1,
        value = input$rangePos_x[1],
        min = input$rangePos_x[1],
        max = input$rangePos_x[2]
      )
    }
  }
})


# Events
shinyFiles::shinyDirChoose(input, "dataset_x",
  roots = volumes, session = session
)

shiny::observeEvent(input$dataset_x, {
  path <- shinyFiles::parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    check <- any(grepl("train", list.files(paste0(path, "/runs/obb/"))))

    if (check) {
      theModelFolder(path)
    } else {
      shiny::showNotification(
        "No trained model was found in this dataset. Choose another one.",
        id = "yolo", type = "error"
      )
    }
  }
})

shinyFiles::shinyFileChoose(input, "videoFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$videoFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$videoFile_x)
  if (nrow(path) > 0) {
    theVideoPath(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(theVideoPath(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(theVideoPath(), volumes),
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

  if (length(volume) > 0) {
    dir <- dirname(theVideoPath())
    defaultRoot(names(volumes)[ix])
    defaultPath(gsub(volume, "", dir))
  }
})

shiny::observeEvent(theVideoPath(), {
  toCheck <- cv2$VideoCapture(theVideoPath())

  if (reticulate::py_to_r(toCheck$isOpened())) {
    if (!is.na(trackRai::n_frames(toCheck))) {
      theVideo <<- toCheck
      theImage <<- theVideo$read()[1]
      refreshVideo(refreshVideo() + 1)
      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

shiny::observeEvent(theFrame(), {
  if (!is.null(theFrame())) {
    theImage <<- trackRai::read_frame(theVideo, theFrame())
    refreshDisplay(refreshDisplay() + 1)
  }
})


# shiny::observeEvent(theYOLOPath(), {
#   if (!is.null(theYOLOPath())) {
#     shinyjs::enable("startTrack_x")
#   } else {
#     shinyjs::disable("startstartTrack_xTrain_x")
#   }
# })


# Display
shiny::observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (trackRai::is_image(theImage)) {
      toDisplay <<- theImage$copy()
    } else {
      toDisplay <<- black_screen$copy()
    }

    printDisplay(printDisplay() + 1)
  }
})

output$display <- shiny::renderUI({
  if (printDisplay() > 0) {
    if (trackRai::is_image(toDisplay)) {
      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", toDisplay)[1])$decode("utf-8")
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

session$onFlushed(function() {
  js$uishape("displayImg")
}, once = TRUE)

shiny::observeEvent(input$winResize, {
  js$uishape("displayImg")
})