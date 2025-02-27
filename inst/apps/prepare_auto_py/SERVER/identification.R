# Display
shiny::observeEvent(input$video_controls, {
  if (input$main == "5") {
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$rangeWidth_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$rangeHeight_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$shapeBuffer_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "5") {
    background <- the_background$copy()
    if (input$dark_button_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    mask <- cv2$compare(the_mask, 0, 1L)
    mask <- cv2$divide(mask, 255)

    frame <- the_image$copy()

    if (input$dark_button_x == "Darker") {
      frame <- cv2$bitwise_not(frame)
    }

    if (input$dark_button_x == "A bit of both") {
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
    bw <- cv2$medianBlur(bw, 3L)
    bw <- cv2$dilate(bw, k)

    cc <- cv2$connectedComponentsWithStats(bw)
    nz <- cv2$findNonZero(cc[1])
    labs <- cc[1][cc[1]$nonzero()]
    ulabs <- np$unique(labs)

    to_display <<- the_image$copy()
    sc <- max(c(trackRai::n_row(to_display), trackRai::n_col(to_display)) / 720)

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
          to_display, list(box), 0L, c(255L, 255L, 255),
          as.integer(max(1, round(2 * sc)))
        )
        cv2$drawContours(
          to_display, list(box), 0L,
          if (reticulate::py_to_r(good)) c(0L, 224L, 0L) else c(5L, 80L, 255L),
          as.integer(max(1, round(sc)))
        )
      }
    }

    print_display(print_display() + 1)
  }
})


# UI
shiny::observe({
  if (is.null(the_stats())) {
    toggleTabs(6, "OFF")
    toggled_tabs$toggled[6] <<- FALSE
  } else {
    if (toggled_tabs$toggled[6] == FALSE) {
      toggleTabs(6, "ON")
      toggled_tabs$toggled[6] <<- TRUE
    }
  }
})

shiny::observeEvent(input$autoSelect_x, {
  if (input$autoSelect_x) {
    shinyjs::disable("rangeWidth_x")
    shinyjs::disable("rangeHeight_x")
    refresh_stats(refresh_stats() + 1)
  } else {
    shinyjs::enable("rangeWidth_x")
    shinyjs::enable("rangeHeight_x")
  }
})


# Compute object statistics
shiny::observeEvent(input$computeStats_x, {
  if (trackRai::is_video_capture(the_video) & trackRai::is_image(the_background) & trackRai::is_image(the_mask)) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Computing object statistics.",
      id = "stats",
      duration = NULL
    )

    frame_pos <- round(seq.int(input$video_controls[1], input$video_controls[3],
      length.out = input$nIDFrames_x
    ))

    pb <- shiny::Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- the_background$copy()
    if (input$dark_button_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    mask <- cv2$compare(the_mask, 0, 1L)
    mask <- cv2$divide(mask, 255)

    obb <- list()
    the_subs <<- list()
    the_submasks <<- list()

    for (i in 1:n) {
      the_video$set(cv2$CAP_PROP_POS_FRAMES, frame_pos[i] - 1)
      frame <- the_video$read()[1]

      if (input$dark_button_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$dark_button_x == "A bit of both") {
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
      bw <- cv2$medianBlur(bw, 3L)
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
          the_submasks <<- c(the_submasks, submask)
          the_subs <<- c(the_subs, cv2$multiply(sub, cv2$divide(cv2$compare(submask, 0, 1L), 255)))
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
    the_stats(dt)
    refresh_stats(refresh_stats() + 1)

    pb$close()

    shiny::removeNotification(id = "stats")
    shinyjs::hideElement("curtain")
  }
})

shiny::observeEvent(refresh_stats(), {
  if (!is.null(the_stats())) {
    if (input$autoSelect_x == TRUE) {
      dt <- the_stats()
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


# Display statistics
output$stats <- plotly::renderPlotly(
  if (!is.null(the_stats())) {
    dt <- the_stats()
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
