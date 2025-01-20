# UI
shiny::observe({
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


# Events
shiny::observeEvent(input$videoControls[2], {
  if (input$main == "5") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(input$computeStats_x, {
  if (trackRai::is_video_capture(theVideo) & trackRai::is_image(theBackground) & trackRai::is_image(theMask)) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Computing object statistics.",
      id = "stats",
      duration = NULL
    )

    frame_pos <- round(seq.int(input$videoControls[1], input$videoControls[3],
      length.out = input$nIDFrames_x
    ))

    pb <- shiny::Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- theBackground$copy()
    if (input$darkButton_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    mask <- cv2$compare(theMask, 0, 1L)
    mask <- cv2$divide(mask, 255)

    obb <- list()
    subs <<- list()
    submasks <<- list()

    for (i in 1:n) {
      theVideo$set(cv2$CAP_PROP_POS_FRAMES, frame_pos[i] - 1)
      frame <- theVideo$read()[1]

      if (input$darkButton_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$darkButton_x == "A bit of both") {
        dif <- cv2$absdiff(frame, background)
      } else {
        dif <- cv2$subtract(frame, background)
      }

      dif <- cv2$multiply(dif, mask)
      dif_gray <- cv2$cvtColor(dif, cv2$COLOR_BGR2GRAY)

      k <- cv2$getStructuringElement(
        cv2$MORPH_RECT,
        as.integer(c(
          input$shapeBuffer_x * 2,
          input$shapeBuffer_x * 2
        )) + 1L
      )

      bw <- cv2$compare(dif_gray, input$threshold_x, 2L)
      bw <- cv2$dilate(bw, k)

      cc <- cv2$connectedComponentsWithStats(bw)
      nz <- cv2$findNonZero(cc[1])
      labs <- cc[1][cc[1]$nonzero()]
      ulabs <- np$unique(labs)

      for (j in seq_along(ulabs)) {
        bb <- reticulate::py_to_r(cc[2][j])
        valid <- bb[5] > 4

        if (valid) {
          ell <- cv2$fitEllipse(nz[labs == ulabs[j - 1]])
          tmp <- reticulate::py_to_r(ell[1])
          obb[[length(obb) + 1]] <- data.table::as.data.table(tmp)

          sub <- dif[bb[2]:(bb[2] + bb[4]), bb[1]:(bb[1] + bb[3])]
          submask <- cv2$cvtColor(
            cv2$compare(cc[1][bb[2]:(bb[2] + bb[4]), bb[1]:(bb[1] + bb[3])], 0, 1L), cv2$COLOR_GRAY2BGR
          )
          submasks <<- c(submasks, submask)
          subs <<- c(subs, cv2$multiply(sub, cv2$divide(cv2$compare(submask, 0, 1L), 255)))
        }
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

    dt <- data.table::rbindlist(obb)
    names(dt) <- c("width", "height")
    theStats(dt)
    refreshStats(refreshStats() + 1)

    pb$close()

    shiny::removeNotification(id = "stats")
    shinyjs::hideElement("curtain")
  }
})

shiny::observeEvent(refreshStats(), {
  if (!is.null(theStats())) {
    if (input$autoSelect_x == TRUE) {
      dt <- theStats()
      d <- sqrt((dt$height - median(dt$height))^2 +
        (dt$width - median(dt$width))^2)
      k <- kmeans(log(d + 1), 2)
      ix <- which.min(k$centers)
      shinyWidgets::updateNumericRangeInput(session, "rangeWidth_x",
        value = round(range(dt$width[k$cluster == ix], na.rm = TRUE))
      )
      shinyWidgets::updateNumericRangeInput(session, "rangeHeight_x",
        value = round(range(dt$height[k$cluster == ix], na.rm = TRUE))
      )
    }
  }
})

shiny::observeEvent(input$autoSelect_x, {
  if (input$autoSelect_x) {
    shinyjs::disable("rangeWidth_x")
    shinyjs::disable("rangeHeight_x")
    refreshStats(refreshStats() + 1)
  } else {
    shinyjs::enable("rangeWidth_x")
    shinyjs::enable("rangeHeight_x")
  }
})

output$stats <- plotly::renderPlotly(
  if (!is.null(theStats())) {
    dt <- theStats()
    rw <- input$rangeWidth_x
    rh <- input$rangeHeight_x
    dt[, select := (width >= rw[1]) & (width <= rw[2]) &
      (height >= rh[1]) & (height <= rh[2])]
    plotly::plot_ly(dt,
      x = ~height, y = ~width, color = ~select,
      type = "scatter", mode = "markers",
      colors = c("#FF5005", "#00e000")
    ) %>%
      plotly::layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height"),
        yaxis = list(title = "Width"),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = FALSE)
  } else {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height", range = c(0, 1), dtick = 0.25),
        yaxis = list(title = "Width", range = c(0, 1), dtick = 0.25)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
)

shiny::observeEvent(input$rangeWidth_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(input$rangeHeight_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(input$shapeBuffer_x, {
  refreshDisplay(refreshDisplay() + 1)
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "5") {
    background <- theBackground$copy()
    if (input$darkButton_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    mask <- cv2$compare(theMask, 0, 1L)
    mask <- cv2$divide(mask, 255)

    frame <- theImage$copy()

    if (input$darkButton_x == "Darker") {
      frame <- cv2$bitwise_not(frame)
    }

    if (input$darkButton_x == "A bit of both") {
      dif <- cv2$absdiff(frame, background)
    } else {
      dif <- cv2$subtract(frame, background)
    }

    dif <- cv2$multiply(dif, mask)
    dif_gray <- cv2$cvtColor(dif, cv2$COLOR_BGR2GRAY)

    k <- cv2$getStructuringElement(
      cv2$MORPH_RECT,
      as.integer(c(
        input$shapeBuffer_x * 2,
        input$shapeBuffer_x * 2
      )) + 1L
    )

    bw <- cv2$compare(dif_gray, input$threshold_x, 2L)
    bw <- cv2$dilate(bw, k)

    cc <- cv2$connectedComponentsWithStats(bw)
    nz <- cv2$findNonZero(cc[1])
    labs <- cc[1][cc[1]$nonzero()]
    ulabs <- np$unique(labs)

    toDisplay <<- theImage$copy()
    sc <- max(c(trackRai::n_row(toDisplay), trackRai::n_col(toDisplay)) / 720)

    for (j in seq_along(ulabs)) {
      bb <- reticulate::py_to_r(cc[2][j])
      valid <- bb[5] > 4

      if (valid) {
        ell_py <- cv2$fitEllipse(nz[labs == ulabs[j - 1]])
        good <- (ell_py[1][0] >= input$rangeWidth_x[1]) &
          (ell_py[1][0] <= input$rangeWidth_x[2]) &
          (ell_py[1][1] >= input$rangeHeight_x[1]) &
          (ell_py[1][1] <= input$rangeHeight_x[2])
        box <- cv2$boxPoints(ell_py)
        box <- np$int_(box)
        cv2$drawContours(
          toDisplay, list(box), 0L, c(255L, 255L, 255),
          as.integer(max(0.5, 4 * sc))
        )
        cv2$drawContours(
          toDisplay, list(box), 0L,
          if (reticulate::py_to_r(good)) c(0L, 224L, 0L) else c(5L, 80L, 255L),
          as.integer(max(0.5, 2 * sc))
        )
      }
    }

    printDisplay(printDisplay() + 1)
  }
})
