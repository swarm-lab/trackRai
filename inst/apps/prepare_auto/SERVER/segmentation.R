# UI
shiny::observeEvent(refresh_display(), {
  if (
    !is_image(the_background) | !is_image(the_mask) | is.null(input$threshold_x)
  ) {
    .toggleTabs(5:6, "OFF")
    toggled_tabs$toggled[5:6] <<- FALSE
  } else if (toggled_tabs$toggled[5] == FALSE) {
    .toggleTabs(5, "ON")
    toggled_tabs$toggled[5] <<- TRUE
  }
})


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "4") {
    if (
      trackRcv::n_row(the_image) != trackRcv::n_row(the_background) |
        trackRcv::n_col(the_image) != trackRcv::n_col(the_background)
    ) {
      to_display <<- black_screen$copy()
    } else {
      background <- the_background$copy()
      if (input$dark_button_x == "Darker") {
        background <- cv2$bitwise_not(background)
      }

      if (
        !trackRcv::is_image(the_mask) |
          trackRcv::n_row(the_image) != trackRcv::n_row(the_mask) |
          trackRcv::n_col(the_image) != trackRcv::n_col(the_mask)
      ) {
        mask <- reticulate::np_array(
          array(
            1L,
            c(
              trackRcv::n_row(the_background),
              trackRcv::n_col(the_background),
              3
            )
          ),
          dtype = "uint8"
        )
      } else {
        mask <- cv2$compare(the_mask, 0, 1L)
        mask <- cv2$divide(mask, 255)
      }

      frame <- the_image$copy()
      if (input$dark_button_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$dark_button_x == "A bit of both") {
        dif <- cv2$absdiff(frame, background)
      } else {
        dif <- cv2$subtract(frame, background)
      }

      if (input$smooth_x > 0) {
        k_size <- ceiling((input$smooth_x - 0.35) / 0.5)
        k_size <- as.integer(k_size + ((k_size %% 2) == 0))
        dif <- cv2$GaussianBlur(dif, c(k_size, k_size), input$smooth_x)
      }

      dif <- cv2$multiply(dif, mask)
      dif_gray <- cv2$cvtColor(dif, cv2$COLOR_BGR2GRAY)

      to_display <<- cv2$multiply(dif_gray, 255L / dif_gray$max())
      to_display <<- cv2$cvtColor(to_display, cv2$COLOR_GRAY2BGR)

      bw <- cv2$compare(dif_gray, input$threshold_x, 2L)
      ct <- cv2$findContours(bw, cv2$RETR_EXTERNAL, cv2$CHAIN_APPROX_NONE)[0]

      sc <- max(c(trackRcv::n_row(frame), trackRcv::n_col(frame)) / 720)
      .drawContour(
        to_display,
        ct,
        color = c(0, 224, 0),
        contrast = c(255, 255, 255),
        thickness = as.integer(max(1.5, round(sc))),
        outline = as.integer(max(1.5, round(sc)))
      )
    }

    print_display(print_display() + 1)
  }
})


# Optimize segmentation threshold
shiny::observeEvent(input$optimize_thresholds, {
  if (
    trackRcv::is_video_capture(the_video) &
      trackRcv::is_image(the_background) &
      trackRcv::n_row(the_image) == trackRcv::n_row(the_background) &
      trackRcv::n_col(the_image) == trackRcv::n_col(the_background)
  ) {
    shinyjs::showElement("curtain")
    shiny::showNotification(
      "Loading images in memory.",
      id = "load",
      duration = NULL
    )

    frame_pos <- round(seq.int(
      input$video_controls_x[1],
      input$video_controls_x[3],
      length.out = 20
    ))

    background <- the_background$copy()
    if (input$dark_button_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    if (
      !trackRcv::is_image(the_mask) |
        trackRcv::n_row(the_image) != trackRcv::n_row(the_mask) |
        trackRcv::n_col(the_image) != trackRcv::n_col(the_mask)
    ) {
      mask <- reticulate::np_array(
        array(
          1L,
          c(trackRcv::n_row(the_background), trackRcv::n_col(the_background), 3)
        ),
        dtype = "uint8"
      )
    } else {
      mask <- cv2$compare(the_mask, 0, 1L)
      mask <- cv2$divide(mask, 255)
    }

    frames <- lapply(frame_pos, function(i) {
      the_video$set(cv2$CAP_PROP_POS_FRAMES, i - 1)
      frame <- the_video$read()[1]

      if (input$dark_button_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$dark_button_x == "A bit of both") {
        frame <- cv2$absdiff(frame, background)
      } else {
        frame <- cv2$subtract(frame, background)
      }

      if (input$smooth_x > 0) {
        k_size <- ceiling((input$smooth_x - 0.35) / 0.5)
        k_size <- as.integer(k_size + ((k_size %% 2) == 0))
        frame <- cv2$GaussianBlur(frame, c(k_size, k_size), input$smooth_x)
      }

      frame <- cv2$multiply(frame, mask)
      cv2$cvtColor(frame, cv2$COLOR_BGR2GRAY)
    })

    shiny::removeNotification(id = "load")
    shiny::showNotification(
      "Optimizing threshold. Please wait.",
      id = "optim",
      duration = NULL
    )

    th <- as.integer(
      mean(
        sapply(frames, function(f) {
          as.integer(
            autothresholdr::auto_thresh(
              reticulate::py_to_r(f),
              method = input$threshold_method_x
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
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$dark_button_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$smooth_x, {
  refresh_display(refresh_display() + 1)
})
