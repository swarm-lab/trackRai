# UI
observe({
  if (is.null(theStats())) {
    toggleTabs(6, "OFF")
    toggledTabs$toggled[6] <<- FALSE
  } else {
    if (toggledTabs$toggled[6] == FALSE) {
      toggleTabs(6, "ON")
      toggledTabs$toggled[6] <<- TRUE
    }
  }
})

output$videoSlider3 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos3_x", "Frame",
      width = "100%", step = 1,
      value = frameMem,
      min = input$rangePos_x[1],
      max = input$rangePos_x[2]
    )
  }
})


# Events
observeEvent(input$videoPos3_x, {
  if (input$main == "5") {
    if (!is.null(input$videoPos3_x)) {
      updateSliderInput(session, "videoPos_x", value = input$videoPos3_x)

      if (!is.null(input$videoPos2_x)) {
        updateSliderInput(session, "videoPos2_x", value = input$videoPos3_x)
      }
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$computeStats_x, {
  if (isVideoStack(theVideo) & isImage(theBackground) & isImage(theMask)) {
    showElement("curtain")
    showNotification("Computing object statistics.",
      id = "stats",
      duration = NULL
    )

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2],
      length.out = input$nIDFrames_x
    ))

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- cloneImage(theBackground)
    if (input$darkButton_x == "Darker") {
      not(background, "self")
    }

    mask <- cloneImage(theMask)
    compare(mask, 0, ">", mask)
    divide(mask, 255, mask)

    frame <- zeros(nrow(theVideo), ncol(theVideo))
    dif <- zeros(nrow(theVideo), ncol(theVideo), 3)
    dif_gray <- zeros(nrow(theVideo), ncol(theVideo), 1)
    morphed <- zeros(nrow(theVideo), ncol(theVideo), 1)
    bw <- zeros(nrow(theVideo), ncol(theVideo), 1)
    cc_dump <- zeros(nrow(theVideo), ncol(theVideo), 1, "16U")

    res <- list()

    for (i in 1:n) {
      readFrame(theVideo, frame_pos[i], frame)
      if (input$darkButton_x == "Darker") {
        not(frame, target = "self")
      }
      subtract(frame, background, dif)
      multiply(dif, mask, dif)
      changeColorSpace(dif, "GRAY", dif_gray)
      morph(dif_gray, "dilate",
        k_height = input$shapeBuffer_x,
        k_width = input$shapeBuffer_x, 
        target = morphed
      )
      threshold(morphed, input$threshold_x, target = bw)
      cc <- connectedComponents(bw, target = cc_dump)

      dt <- as.data.table(cc$table)
      res[[i]] <- dt[, fitEllipse(x, y)[1:3], by = label]

      for (j in 2:nrow(cc$stats)) {
        r <- cc$stats[j, ]
        submasks <<- c(submasks, subImage(cc_dump, r[4], r[5] - r[7] + 1, r[6], r[7]) == r[1])
        subs <<- c(subs, subImage(dif, r[4], r[5] - r[7] + 1, r[6], r[7]) *
          changeColorSpace(submasks[[length(submasks)]] / 255, "BGR"))
      }

      new_check <- floor(100 * i / n)
      if (new_check > (old_check + 5)) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time,
          units = "secs"
        ))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(
            new_check, "% - ",
            round(fps, digits = 2), "fps"
          )
        )
      }
    }

    dt <- data.table::rbindlist(res)
    theStats(dt)
    refreshStats(refreshStats() + 1)

    pb$close()

    removeNotification(id = "stats")
    hideElement("curtain")
  }
})

observeEvent(refreshStats(), {
  if (!is.null(theStats())) {
    if (input$autoSelect_x == TRUE) {
      dt <- theStats()
      d <- sqrt((dt$height - median(dt$height))^2 +
        (dt$width - median(dt$width))^2)
      k <- kmeans(log(d + 1), 2)
      ix <- which.min(k$centers)
      updateNumericRangeInput(session, "rangeWidth_x",
        value = round(range(dt$width[k$cluster == ix], na.rm = TRUE))
      )
      updateNumericRangeInput(session, "rangeHeight_x",
        value = round(range(dt$height[k$cluster == ix], na.rm = TRUE))
      )
    }
  }
})

observeEvent(input$autoSelect_x, {
  if (input$autoSelect_x) {
    disable("rangeWidth_x")
    disable("rangeHeight_x")
    refreshStats(refreshStats() + 1)
  } else {
    enable("rangeWidth_x")
    enable("rangeHeight_x")
  }
})

output$stats <- renderPlotly(
  if (!is.null(theStats())) {
    dt <- theStats()
    rw <- input$rangeWidth_x
    rh <- input$rangeHeight_x
    dt[, select := (width >= rw[1]) & (width <= rw[2]) &
      (height >= rh[1]) & (height <= rh[2])]
    plot_ly(dt,
      x = ~height, y = ~width, color = ~select,
      type = "scatter", mode = "markers",
      colors = c("#FF5005", "#00e000")
    ) %>%
      layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height"),
        yaxis = list(title = "Width"),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  } else {
    plot_ly(type = "scatter", mode = "markers") %>%
      layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height", range = c(0, 1), dtick = 0.25),
        yaxis = list(title = "Width", range = c(0, 1), dtick = 0.25)
      ) %>%
      config(displayModeBar = FALSE)
  }
)

observeEvent(input$rangeWidth_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$rangeHeight_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$shapeBuffer_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(refreshDisplay(), {
  if (input$main == "5") {
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
    } else {
      background <- cloneImage(theBackground)
      changeColorSpace(background, "GRAY", "self")
      if (input$darkButton_x == "Darker") {
        not(background, "self")
      }

      mask <- cloneImage(theMask)
      changeColorSpace(mask, "GRAY", "self")
      compare(mask, 0, ">", mask)
      divide(mask, 255, mask)

      gray <- changeColorSpace(theImage, "GRAY")
      if (input$darkButton_x == "Darker") {
        not(gray, target = "self")
      }
      dif <- subtract(gray, background)
      multiply(dif, mask, dif)
      morphed <- morph(dif, "dilate",
        k_height = input$shapeBuffer_x,
        k_width = input$shapeBuffer_x
      )
      bw <- threshold(morphed, input$threshold_x)
      cc <- connectedComponents(bw)

      toDisplay <- cloneImage(theImage)
      sc <- max(dim(toDisplay) / 720)

      for (i in cc$stats[-1, 1]) {
        ix <- cc$table[, 3] == i
        ell <- fitEllipse(cc$table[ix, 1], cc$table[ix, 2])
        good <- (ell$width >= input$rangeWidth_x[1]) &
          (ell$width <= input$rangeWidth_x[2]) &
          (ell$height >= input$rangeHeight_x[1]) &
          (ell$height <= input$rangeHeight_x[2])
        drawRotatedRectangle(toDisplay, ell$center[1], ell$center[2],
          ell$width, ell$height, ell$angle,
          color = "white", thickness = max(0.5, 4 * sc)
        )
        drawRotatedRectangle(toDisplay, ell$center[1], ell$center[2],
          ell$width, ell$height, ell$angle,
          color = if (good) "#00e000" else "#FF5005",
          thickness = max(0.5, 2 * sc)
        )
      }

      suppressMessages(
        write.Image(toDisplay, paste0(tmpDir, "/display.bmp"), TRUE)
      )
    }

    printDisplay(printDisplay() + 1)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeBlobs_x"))
