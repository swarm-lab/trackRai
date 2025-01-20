# Status
output$yoloStatus <- shiny::renderUI({
  if (!yolo_installed) {
    p(
      "No YOLO installation was detected.",
      tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  } else if (is.null(theModelFolder())) {
    p("Dataset missing (and required).", class = "bad")
  }
})

output$videoStatus <- shiny::renderUI({
  if (refreshDisplay() > -1 & !trackRai::is_video_capture(theVideo)) {
    p("Video missing (and required).", class = "bad")
  } else if (!trackRai::is_video_capture(theVideo)) {
    p("Incompatible videos.", class = "bad")
  }
})

shiny::observe({
  if (!is.null(theModelFolder()) & !is.null(theVideoPath()) & trackRai::is_video_capture(theVideo)) {
    if (toggledTabs$toggled[2] == FALSE) {
      toggleTabs(2, "ON")
      toggledTabs$toggled[2] <<- TRUE
    }
  } else {
    toggleTabs(2, "OFF")
    toggledTabs$toggled[2] <<- FALSE
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
      theModelFolder(NULL)
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

      if (!all(unlist(reticulate::py_to_r(theMask$shape)) == unlist(reticulate::py_to_r(theImage$shape)))) {
        theMask <<- reticulate::np_array(
          array(1L, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 3)),
          dtype = "uint8"
        )
      }

      refreshVideo(refreshVideo() + 1)
      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

shinyFiles::shinyFileChoose(input, "maskFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$maskFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$maskFile_x)
  if (nrow(path) > 0) {
    theMaskPath(normalizePath(path$datapath, mustWork = FALSE))
    refreshMask(refreshMask() + 1)
  }
})

shiny::observeEvent(refreshMask(), {
  if (refreshMask() > 0) {
    toCheck <- cv2$imread(theMaskPath())

    if (trackRai::is_image(toCheck)) {
      if (!all(unlist(reticulate::py_to_r(toCheck$shape)) == unlist(reticulate::py_to_r(theImage$shape)))) {
        shinyalert::shinyalert("Error:",
          "The video and mask do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        theMask <<- reticulate::np_array(
          array(1L, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 3)),
          dtype = "uint8"
        )
      } else {
        theMask <<- cv2$divide(cv2$compare(toCheck, 0, 1L), 255L)
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(theMaskPath(), volumes),
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
      dir <- dirname(theMaskPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))

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


# Displays
shiny::observeEvent(input$videoControls[2], {
  if (trackRai::is_video_capture(theVideo)) {
    theFrame(input$videoControls[2])
  }
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (trackRai::is_image(theImage)) {
      toDisplay <<- theImage$copy()

      if (trackRai::is_image(theMask)) {
        toDisplay <<- cv2$multiply(toDisplay, cv2$divide(cv2$compare(theMask, 0, 1L), 255L))
      }
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
