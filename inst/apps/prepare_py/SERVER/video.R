# Status
output$videoStatus <- shiny::renderUI({
  if (refreshDisplay() > -1 & !trackRai::is_video_capture(theVideo)) {
    toggleTabs(2:6, "OFF")
    toggledTabs$toggled[2:6] <<- FALSE
    p("Video missing (and required).", class = "bad")
  } else if (!trackRai::is_video_capture(theVideo)) {
    toggleTabs(2:6, "OFF")
    toggledTabs$toggled[2:6] <<- FALSE
    p("Incompatible videos.", class = "bad")
  } else {
    if (toggledTabs$toggled[2] == FALSE) {
      toggleTabs(2, "ON")
      toggledTabs$toggled[2] <<- TRUE
      NULL
    }
  }
})


# Events
shiny::observeEvent(input$main, {
  refreshDisplay(refreshDisplay() + 1)
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


# Display
shiny::observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (trackRai::is_image(theImage)) {
      toDisplay <<- theImage$copy()
    } else {
      toDisplay <<- black_screen$copy()
    }

    js$uishape("displayImg")
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
})

shiny::observeEvent(input$winResize, {
  js$uishape("displayImg")
})

shiny::observeEvent(input$videoControls[2], {
  if (trackRai::is_video_capture(theVideo)) {
    theFrame(input$videoControls[2])
  }
})
