# Status
output$backgroundStatus <- renderUI({
  if ((refreshBackground() > -1 | input$computeBackground_x > -1) &
    !isImage(theBackground)) {
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
shinyFileChoose(input, "backgroundFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

observeEvent(input$backgroundFile_x, {
  path <- parseFilePaths(volumes, input$backgroundFile_x)
  if (nrow(path) > 0) {
    theBackgroundPath(path$datapath)
    refreshBackground(refreshBackground() + 1)
  }
})

observeEvent(refreshBackground(), {
  if (refreshBackground() > 0) {
    toCheck <- tryCatch(image(theBackgroundPath()), error = function(e) NA)

    if (isImage(toCheck)) {
      if (colorspace(toCheck) != "BGR") {
        changeColorSpace(toCheck, "BGR", "self")
      }

      if (!all(dim(toCheck) == dim(theImage))) {
        shinyalert("Error:",
          "The video and background do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        theBackground <<- NULL
      } else {
        theBackground <<- cloneImage(toCheck)
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

observeEvent(input$computeBackground_x, {
  if (isVideoStack(theVideo)) {
    showElement("curtain")
    theBackground <<- backgrounder(theVideo,
      n = input$backroundImages_x,
      method = input$backgroundType_x,
      start = input$rangePos_x[1],
      end = input$rangePos_x[2]
    )
    changeBitDepth(theBackground, "8U", target = "self")
    hideElement("curtain")
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "2") {
    if (isImage(theBackground)) {
      cloneImage(theBackground, toDisplay)
      sc <- max(dim(toDisplay) / 720)
      r <- 0.01 * min(nrow(toDisplay), ncol(toDisplay))

      if (collectGhost() > 0) {
        if (nrow(ghostCoords) > 1) {
          drawPolyline(toDisplay, ghostCoords,
            closed = TRUE, color = "white",
            thickness = max(1, 1.5 * sc)
          )
        }

        if (nrow(ghostCoords) > 0) {
          drawCircle(toDisplay,
            x = ghostCoords[, 1], y = ghostCoords[, 2],
            radius = r * 1.5, thickness = -1, color = "white"
          )
          drawCircle(toDisplay,
            x = ghostCoords[, 1], y = ghostCoords[, 2],
            radius = r, thickness = -1, color = "red"
          )
        }
      }

      suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.bmp"), TRUE))
    } else if (isImage(theImage)) {
      suppressMessages(write.Image(
        zeros(nrow(theImage), ncol(theImage), 3),
        paste0(tmpDir, "/display.bmp"), TRUE
      ))
    } else {
      suppressMessages(write.Image(
        zeros(1080, 1920, 3),
        paste0(tmpDir, "/display.bmp"), TRUE
      ))
    }

    printDisplay(printDisplay() + 1)
  }
})

observeEvent(input$ghostButton_x, {
  if (isImage(theBackground)) {
    toggleInputs(input, "OFF")
    toggleTabs(1, "OFF")

    showNotification("Click to draw a polygon around the object to remove from
                     the image. Double-click to stop.",
      id = "ghost_notif",
      duration = NULL, type = "message"
    )

    collectGhost(1)
  }
})

observeEvent(input$plot_click, {
  if (collectGhost() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(theBackground) + 1
    ghostCoords <<- rbind(ghostCoords, unlist(clck))
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$plot_dblclick, {
  if (collectGhost() > 0) {
    stopGhostCollection(stopGhostCollection() + 1)
  }
})

observeEvent(stopGhostCollection(), {
  if (collectGhost() > 0) {
    if (nrow(ghostCoords) > 0) {
      roi <- zeros(nrow(theBackground), ncol(theBackground), 1)
      fillPoly(roi, ghostCoords, "white")
      inpaint(theBackground, roi, method = "Telea", target = "self")
    }

    removeNotification(id = "ghost_notif")
    toggleInputs(input, "ON")
    toggleTabs(1, "ON")
    collectGhost(0)
    ghostCoords <<- NULL
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFileSave(input, "saveBackground_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

observeEvent(input$saveBackground_x, {
  path <- parseSavePath(volumes, input$saveBackground_x)

  if (isImage(theBackground) & nrow(path) > 0) {
    write.Image(theBackground, path$datapath, TRUE)
    theBackgroundPath(path$datapath)
  }
})


# Bookmark
setBookmarkExclude(c(
  session$getBookmarkExclude(), "backgroundFile_x", "refreshBackground",
  "computeBackground_x", "ghostButton_x", "saveBackground_x"
))

onBookmark(function(state) {
  state$values$theBackgroundPath <- theBackgroundPath()
})

onRestore(function(state) {
  theBackgroundPath(state$values$theBackgroundPath[[1]])
  refreshBackground(refreshBackground() + 1)
})
