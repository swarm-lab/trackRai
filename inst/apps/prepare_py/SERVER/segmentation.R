# UI
shiny::observe({
  if (is.null(input$videoPos2_x)) {
    toggleTabs(5:6, "OFF")
    toggledTabs$toggled[5:6] <<- FALSE
  } else {
    if (toggledTabs$toggled[5] == FALSE) {
      toggleTabs(5, "ON")
      toggledTabs$toggled[5] <<- TRUE
      shiny::updateCheckboxInput(session, "autoSelect_x", value = TRUE)
    }
  }
})

output$videoSlider2 <- shiny::renderUI({
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
shiny::observeEvent(input$videoPos2_x, {
  if (input$main == "4") {
    if (!is.null(input$videoPos2_x)) {
      shiny::updateSliderInput(session, "videoPos_x", value = input$videoPos2_x)

      if (!is.null(input$videoPos3_x)) {
        shiny::updateSliderInput(session, "videoPos3_x", value = input$videoPos2_x)
      }
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(input$optimizeThresholds_x, {
  if (trackRai::is_video_capture(theVideo) & trackRai::is_image(theBackground) &
    trackRai::n_row(theImage) == trackRai::n_row(theBackground) &
    trackRai::n_col(theImage) == trackRai::n_col(theBackground)) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2],
      length.out = 20
    ))

    background <- theBackground$copy()
    if (input$darkButton_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    if (!trackRai::is_image(theMask) | trackRai::n_row(theImage) != trackRai::n_row(theMask) |
      trackRai::n_col(theImage) != trackRai::n_col(theMask)) {
      mask <- reticulate::np_array(
        array(1L, c(trackRai::n_row(theBackground), trackRai::n_col(theBackground), 3)),
        dtype = "uint8"
      )
    } else {
      mask <- cv2$compare(theMask, 0, 1L)
      mask <- cv2$divide(mask, 255)    
    }

    frames <- lapply(frame_pos, function(i) {
      theVideo$set(cv2$CAP_PROP_POS_FRAMES, i - 1)
      frame <- theVideo$read()[1]

      if (input$darkButton_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$darkButton_x == "A bit of both") {
        frame <- cv2$absdiff(frame, background)
      } else {
        frame <- cv2$subtract(frame, background)
      }

      frame <- cv2$multiply(frame, mask)
      cv2$cvtColor(frame, cv2$COLOR_BGR2GRAY)
    })

    shiny::removeNotification(id = "load")
    shiny::showNotification("Optimizing threshold. Please wait.",
      id = "optim", duration = NULL
    )

    th <- as.integer(
      mean(
        sapply(frames, function(f) {
          as.integer(
            autothresholdr::auto_thresh(
              reticulate::py_to_r(f),
              method = input$thresholdMethod_x
            )
          )
        })
      )
    )

    shiny::removeNotification(id = "optim")
    shinyjs::hideElement("curtain")

    shiny::updateSliderInput(session, "threshold_x", value = th[1])
  }
})

shiny::observeEvent(input$threshold_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(input$darkButton_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "4") {
    if (trackRai::n_row(theImage) != trackRai::n_row(theBackground) |
      trackRai::n_col(theImage) != trackRai::n_col(theBackground)) {
      toDisplay <<- black_screen$copy()
    } else {
      background <- theBackground$copy()
      if (input$darkButton_x == "Darker") {
        background <- cv2$bitwise_not(background)
      }

      if (!trackRai::is_image(theMask) | trackRai::n_row(theImage) != trackRai::n_row(theMask) |
        trackRai::n_col(theImage) != trackRai::n_col(theMask)) {
        mask <- reticulate::np_array(
          array(1L, c(trackRai::n_row(theBackground), trackRai::n_col(theBackground), 3)),
          dtype = "uint8"
        )
      } else {
        mask <- cv2$compare(theMask, 0, 1L)
        mask <- cv2$divide(mask, 255)          
      }

      frame <- theImage$copy()
      if (input$darkButton_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$darkButton_x == "A bit of both") {
        frame <- cv2$absdiff(frame, background)
      } else {
        frame <- cv2$subtract(frame, background)
      }

      frame <- cv2$multiply(frame, mask)
      gray <- cv2$cvtColor(frame, cv2$COLOR_BGR2GRAY)

      toDisplay <<- cv2$multiply(gray, 255L / gray$max())
      toDisplay <<- cv2$cvtColor(toDisplay, cv2$COLOR_GRAY2BGR)

      bw <- cv2$compare(gray, input$threshold_x, 2L)
      ct <- cv2$findContours(bw, cv2$RETR_EXTERNAL, cv2$CHAIN_APPROX_NONE)[0]

      sc <- max(c(trackRai::n_row(frame), trackRai::n_col(frame)) / 720)
      cv2$drawContours(toDisplay, ct, -1L, c(0L, 224L, 0L), as.integer(max(1, 1.5 * sc)))
    }

    printDisplay(printDisplay() + 1)
  }
})
