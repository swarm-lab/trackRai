# Status
output$videoStatus <- shiny::renderUI({
  if (refreshDisplay() > -1 & !trackRai::is_video_capture(theVideo)) {
    shiny::p("Video missing (and required).", class = "bad")
  } else if (!trackRai::is_video_capture(theVideo)) {
    shiny::p("Incompatible videos.", class = "bad")
  }
})

output$trackStatus <- shiny::renderUI({
  if (refreshDisplay() > -1 & !data.table::is.data.table(theTracks)) {
    shiny::p("Tracks missing (and required).", class = "bad")
  } else if (!(all(names(theTracks) == track_names))) {
    shiny::p("Incompatible tracks.", class = "bad")
  }
})


# UI
output$exportControls <- shiny::renderUI({
  if (refreshDisplay() > -1 & trackRai::is_video_capture(theVideo) &
    data.table::is.data.table(theTracks)) {
    shiny::tagList(
      shinyFiles::shinySaveButton(
        "export_x", "Export video with tracks", "Save video as...",
        filename = "video_tracks.mp4",
        filetype = list(video = c("mp4")),
        class = "fullWidth"
      ),
      shiny::hr()
    )
  }
})


# Events
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

shinyFiles::shinyFileChoose(input, "trackFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$trackFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$trackFile_x)
  if (nrow(path) > 0) {
    theTrackPath(normalizePath(path$datapath, mustWork = FALSE))
    refreshTracks(refreshTracks() + 1)
  }
})

shiny::observeEvent(refreshTracks(), {
  if (refreshTracks() > 0) {
    toCheck <- data.table::fread(theTrackPath())

    if (all(names(toCheck) == track_names)) {
      theTracks <<- toCheck
      ix <- which.max(
        sapply(
          stringr::str_locate_all(theTrackPath(), volumes),
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
      dir <- dirname(theTrackPath())
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

shinyFiles::shinyFileSave(input, "export_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$export_x, {
  path <- shinyFiles::parseSavePath(volumes, input$export_x)

  if (nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    theExportPath(path)
  }
})


# Displays
shiny::observeEvent(input$videoControls[2], {
  if (trackRai::is_video_capture(theVideo)) {
    theFrame(input$videoControls[2])
  }
})

shiny::observeEvent(input$lineWidth_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(input$trackLength_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (trackRai::is_image(theImage)) {
      toDisplay <<- theImage$copy()

      if (data.table::is.data.table(theTracks)) {
        displayTable <- theTracks[frame <= theFrame() & frame > (theFrame() - input$trackLength_x)]
        last <- displayTable[frame == max(frame)]
        box <- reticulate::r_to_py(
          simplify2array(
            list(
              as.matrix(last[, c("x1", "x2", "x3", "x4")]),
              as.matrix(last[, c("y1", "y2", "y3", "y4")])
            )
          )
        )
        box <- np$int_(box)
        shades <- col[(last$id %% length(col)) + 1]

        for (i in seq_len(py_to_r(box$shape[0]))) {
          toDisplay <<- cv2$drawContours(
            toDisplay, list(box[i - 1]), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
            as.integer(input$lineWidth_x)
          )
          trace <- reticulate::r_to_py(as.matrix(displayTable[id == last$id[i], c("x", "y")]))
          toDisplay <<- cv2$polylines(
            toDisplay, list(np$int_(trace)), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
            as.integer(input$lineWidth_x)
          )
        }
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


# Export
shiny::observeEvent(theExportPath(), {
  shiny::showNotification("Export in progress.", id = "export", duration = NULL)
  toggleTabs(1, "OFF")
  toggledTabs$toggled[1] <<- FALSE
  toggleInputs(input, state = "OFF")

  theVideoWriter <<- cv2$VideoWriter(
    normalizePath(theExportPath(), mustWork = FALSE),
    codec,
    theVideo$get(cv2$CAP_PROP_FPS),
    as.integer(c(trackRai::n_col(theImage), trackRai::n_row(theImage)))
  )
  theVideo$set(cv2$CAP_PROP_POS_FRAMES, input$videoControls[1] - 1)
  pb <<- Progress$new()
  pb$set(message = "Computing: ", value = 0, detail = "0%")
  n <<- input$videoControls[3] - input$videoControls[1] + 1
  old_check <<- 0
  old_frame <<- 1
  old_time <<- Sys.time()
  theLoop(0)
  inProgress(TRUE)
})

shiny::observeEvent(theDebounce(), {
  if (!is.null(theLoop())) {
    if (theLoop() < n) {
      frame <<- theVideo$read()[1]

      trackTable <- theTracks[frame <= (input$videoControls[1] + theLoop()) &
        frame > (input$videoControls[1] + theLoop() - input$trackLength_x)]
      last <- trackTable[frame == max(frame)]
      box <- reticulate::r_to_py(
        simplify2array(
          list(
            as.matrix(last[, c("x1", "x2", "x3", "x4")]),
            as.matrix(last[, c("y1", "y2", "y3", "y4")])
          )
        )
      )
      box <- np$int_(box)
      shades <- col[(last$id %% length(col)) + 1]

      for (i in seq_len(py_to_r(box$shape[0]))) {
        frame <<- cv2$drawContours(
          frame, list(box[i - 1]), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
          as.integer(input$lineWidth_x)
        )
        trace <- reticulate::r_to_py(as.matrix(trackTable[id == last$id[i], c("x", "y")]))
        frame <<- cv2$polylines(
          frame, list(np$int_(trace)), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
          as.integer(input$lineWidth_x)
        )
      }

      theVideoWriter$write(frame)

      new_check <- floor(100 * theLoop() / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (theLoop() - old_frame + 1) /
          as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <<- new_check
        old_frame <<- theLoop()
        old_time <<- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps")
        )
      }

      theLoop(theLoop() + 1)
    } else {
      theVideoWriter$release()
      theLoop(NULL)
      theExportPath(NULL)
      inProgress(FALSE)
      shiny::removeNotification(id = "export")
      pb$close()
      toggleTabs(1, "ON")
      toggledTabs$toggled[1] <<- TRUE
      toggleInputs(input, state = "ON")
    }
  }
})
