# Status
output$backgroundStatus <- shiny::renderUI({
  if ((refreshDisplay() > -1 | input$computeBackground_x > -1) &
    !trackRai::is_image(theBackground)) {
    toggleTabs(3:6, "OFF")
    toggledTabs$toggled[3:6] <<- FALSE
    p("Background missing (and required).", class = "bad")
  } else {
    if (toggledTabs$toggled[3] == FALSE) {
      toggleTabs(3, "ON")
      toggledTabs$toggled[3] <<- TRUE
      NULL
    }
  }
})

# Events
shinyFiles::shinyFileChoose(input, "backgroundFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$backgroundFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$backgroundFile_x)
  if (nrow(path) > 0) {
    theBackgroundPath(normalizePath(path$datapath, mustWork = FALSE))
    refreshBackground(refreshBackground() + 1)
  }
})

shiny::observeEvent(refreshBackground(), {
  if (refreshBackground() > 0) {
    toCheck <- cv2$imread(theBackgroundPath())

    if (trackRai::is_image(toCheck)) {
      if (!all(unlist(reticulate::py_to_r(toCheck$shape)) == unlist(reticulate::py_to_r(theImage$shape)))) {
        shinyalert::shinyalert("Error:",
          "The video and background do not have the same shape.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        theBackground <<- NULL
      } else {
        theBackground <<- toCheck$copy()
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(theBackgroundPath(), volumes),
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
      dir <- dirname(theBackgroundPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))

      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

shiny::observeEvent(input$computeBackground_x, {
  if (trackRai::is_video_capture(theVideo)) {
    showElement("curtain")
    theBackground <<- trackRai::backgrounder(theVideo,
      n = input$backroundImages_x,
      method = input$backgroundType_x,
      start = input$rangePos_x[1],
      end = input$rangePos_x[2]
    )
    hideElement("curtain")
    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "2") {
    if (trackRai::is_image(theBackground)) {
      toDisplay <<- theBackground$copy()
      sc <- max(c(trackRai::n_row(toDisplay), trackRai::n_col(toDisplay)) / 720)
      r <- 0.01 * min(trackRai::n_row(toDisplay), trackRai::n_col(toDisplay))

      if (collectGhost() > 0) {
        if (nrow(ghostCoords) > 1) {
          cv2$polylines(
            toDisplay, array(as.integer(ghostCoords), c(1, dim(ghostCoords))),
            TRUE, c(255, 255, 255), as.integer(max(1, 1.5 * sc))
          )
        }

        if (nrow(ghostCoords) > 0) {
          for (i in seq_len(nrow(ghostCoords))) {
            cv2$circle(
              toDisplay, as.integer(ghostCoords[i, ]), as.integer(r * 1.5),
              c(255L, 255L, 255L), -1L
            )
            cv2$circle(
              toDisplay, as.integer(ghostCoords[i, ]), as.integer(r),
              c(0L, 0L, 255L), -1L
            )
          }
        }
      }
    } else {
      toDisplay <<- black_screen$copy()
    }

    printDisplay(printDisplay() + 1)
  }
})

shiny::observeEvent(input$ghostButton_x, {
  if (trackRai::is_image(theBackground)) {
    toggleInputs(input, "OFF")
    toggleTabs(1, "OFF")

    shiny::showNotification("Click to draw a polygon around the object to remove from
                     the image. Enter to stop. Esc to cancel.",
      id = "ghost_notif",
      duration = NULL, type = "message"
    )

    collectGhost(1)
  }
})

shinyjs::onevent("click", "displayImg", function(props) {
  if (collectGhost() > 0) {
    x <- trackRai::n_col(toDisplay) * (props$offsetX / input$displayImg_width)
    y <- trackRai::n_row(toDisplay) * (props$offsetY / input$displayImg_height)
    ghostCoords <<- rbind(ghostCoords, c(x, y))
    refreshDisplay(refreshDisplay() + 1)
  } else if (collectMask() > 0) {
    x <- trackRai::n_col(toDisplay) * (props$offsetX / input$displayImg_width)
    y <- trackRai::n_row(toDisplay) * (props$offsetY / input$displayImg_height)
    maskCoords <<- rbind(maskCoords, c(x, y))

    if (collectMask() == 2 & nrow(maskCoords) >= 5) {
      stopMaskCollection(stopMaskCollection() + 1)
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(input$retKey, {
  if (collectGhost() > 0) {
    stopGhostCollection(stopGhostCollection() + 1)
  }
})

shiny::observeEvent(input$escKey, {
  if (collectGhost() > 0) {
    ghostCoords <<- NULL
    stopGhostCollection(stopGhostCollection() + 1)
  }
})

shiny::observeEvent(stopGhostCollection(), {
  if (collectGhost() > 0) {
    if (nrow(ghostCoords) > 0) {
      roi <- reticulate::np_array(
        array(0L, c(trackRai::n_row(theBackground), trackRai::n_col(theBackground), 1)),
        dtype = "uint8"
      )
      cv2$fillPoly(
        roi,
        pts = array(as.integer(ghostCoords), c(1, dim(ghostCoords))), 
        color = c(255, 255, 255)
      )
      theBackground <<- cv2$inpaint(theBackground, roi, 5, cv2$INPAINT_TELEA)
    }

    shiny::removeNotification(id = "ghost_notif")
    toggleInputs(input, "ON")
    toggleTabs(1, "ON")
    collectGhost(0)
    ghostCoords <<- NULL
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFiles::shinyFileSave(input, "saveBackground_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$saveBackground_x, {
  path <- shinyFiles::parseSavePath(volumes, input$saveBackground_x)

  if (trackRai::is_image(theBackground) & nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    cv2$imwrite(path, theBackground)
    theBackgroundPath(path)
  }
})
