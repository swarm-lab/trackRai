# UI
observe({
  if (is.null(input$videoPos2_x)) {
    toggleTabs(5:6, "OFF")
    toggledTabs$toggled[5:6] <<- FALSE
  } else {
    if (toggledTabs$toggled[5] == FALSE) {
      toggleTabs(5, "ON")
      toggledTabs$toggled[5] <<- TRUE
      updateCheckboxInput(session, "autoSelect_x", value = TRUE)
    }
  }
})

output$videoSlider2 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos2_x", "Frame",
      width = "100%", step = 1,
      value = frameMem,
      min = input$rangePos_x[1],
      max = input$rangePos_x[2]
    )
  }
})


# Events 
observeEvent(input$videoPos2_x, {
  if (input$main == "4") {
    if (!is.null(input$videoPos2_x)) {
      updateSliderInput(session, "videoPos_x", value = input$videoPos2_x)

      if (!is.null(input$videoPos3_x)) {
        updateSliderInput(session, "videoPos3_x", value = input$videoPos2_x)
      }
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$optimizeThresholds_x, {
  if (isVideoStack(theVideo) & isImage(theBackground) &
    nrow(theImage) == nrow(theBackground) &
    ncol(theImage) == ncol(theBackground)) {
    showElement("curtain")
    showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2],
      length.out = 20
    ))

    background <- cloneImage(theBackground)
    if (input$darkButton_x == "Darker") {
      not(background, "self")
    }

    if (!isImage(theMask) | nrow(theImage) != nrow(theMask) |
      ncol(theImage) != ncol(theMask)) {
      mask <- ones(nrow(theBackground), ncol(theBackground), 3)
    } else {
      mask <- cloneImage(theMask)
      compare(mask, 0, ">", mask)
      divide(mask, 255, mask)
    }

    frames <- lapply(frame_pos, function(i) {
      frame <- readFrame(theVideo, i)

      if (input$darkButton_x == "Darker") {
        not(frame, target = "self")
      }

      if (input$darkButton_x == "A bit of both") {
        absdiff(frame, background, "self")
      } else {
        subtract(frame, background, frame)
      }

      multiply(frame, mask, frame)
      changeColorSpace(frame, "GRAY", "self")
      frame
    })

    removeNotification(id = "load")
    showNotification("Optimizing threshold. Please wait.",
      id = "optim", duration = NULL
    )

    th <- as.integer(
      mean(
        sapply(frames, function(f) {
          autothreshold(f, method = input$thresholdMethod_x)
        })
      )
    )

    removeNotification(id = "optim")
    hideElement("curtain")

    updateSliderInput(session, "threshold_x", value = th[1])
  }
})

observeEvent(input$threshold_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(refreshDisplay(), {
  if (input$main == "4") {
    if (!isImage(theImage) & !isImage(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(1080, 1920, 3),
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else if (!isImage(theImage)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theBackground), ncol(theBackground), 3),
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else if (!isImage(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theImage), ncol(theImage), 3),
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else if (nrow(theImage) != nrow(theBackground) |
      ncol(theImage) != ncol(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theImage), ncol(theImage), 3),
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else {
      background <- changeColorSpace(theBackground, "GRAY")
      if (input$darkButton_x == "Darker") {
        not(background, target = "self")
      }

      if (!isImage(theMask) | nrow(theImage) != nrow(theMask) |
        ncol(theImage) != ncol(theMask)) {
        mask <- ones(nrow(theBackground), ncol(theBackground), 1)
      } else {
        mask <- changeColorSpace(theMask, "GRAY")
      }

      image <- changeColorSpace(theImage, "GRAY")
      if (input$darkButton_x == "Darker") {
        not(image, target = "self")
      }

      if (input$darkButton_x == "A bit of both") {
        absdiff(image, background, "self")
      } else {
        image %i-% background
      }

      compare(mask, 0, ">", mask)
      divide(mask, 255, mask)
      multiply(image, mask, image)

      to_display <- image * (255 / max(max(image)))
      changeColorSpace(to_display, "BGR", "self")
      bw <- image >= input$threshold_x
      boxFilter(bw, 1, 1, target = "self")
      bw %i>% 63
      ct <- findContours(bw, method = "none")

      sc <- max(dim(image) / 720)
      drawCircle(
        to_display, ct$contours[, 2], ct$contours[, 3],
        max(0.5, sc), "#00e000", -1
      )
      suppressMessages(
        write.Image(to_display, paste0(tmpDir, "/display.bmp"), TRUE)
      )
    }

    printDisplay(printDisplay() + 1)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeThresholds_x"))
