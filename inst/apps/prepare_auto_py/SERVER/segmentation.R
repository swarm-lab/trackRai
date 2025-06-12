# Display 
shiny::observeEvent(refresh_display(), {
  if (input$main == "4") {
    if (n_row(the_image) != n_row(the_background) |
      n_col(the_image) != n_col(the_background)) {
      to_display <<- black_screen$copy()
    } else {
      background <- the_background$copy()
      if (input$dark_button_x == "Darker") {
        background <- cv2$bitwise_not(background)
      }

      if (!is_image(the_mask) | n_row(the_image) != n_row(the_mask) |
        n_col(the_image) != n_col(the_mask)) {
        mask <- reticulate::np_array(
          array(1L, c(n_row(the_background), n_col(the_background), 3)),
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
        frame <- cv2$absdiff(frame, background)
      } else {
        frame <- cv2$subtract(frame, background)
      }

      frame <- cv2$multiply(frame, mask)
      gray <- cv2$cvtColor(frame, cv2$COLOR_BGR2GRAY)

      to_display <<- cv2$multiply(gray, 255L / gray$max())
      to_display <<- cv2$cvtColor(to_display, cv2$COLOR_GRAY2BGR)

      bw <- cv2$compare(gray, input$threshold_x, 2L)
      ct <- cv2$findContours(bw, cv2$RETR_EXTERNAL, cv2$CHAIN_APPROX_NONE)[0]

      sc <- max(c(n_row(frame), n_col(frame)) / 720)
      cv2$drawContours(to_display, ct, -1L, c(0L, 224L, 0L), as.integer(max(1, 1.5 * sc)))
    }

    print_display(print_display() + 1)
  }
})


# UI
shiny::observe({
  if (input$main == "4") {
    if (is.null(input$threshold_x)) {
      toggleTabs(5:6, "OFF")
      toggled_tabs$toggled[5:6] <<- FALSE
    } else if (toggled_tabs$toggled[5] == FALSE) {
      toggleTabs(5, "ON")
      toggled_tabs$toggled[5] <<- TRUE
      shiny::updateCheckboxInput(session, "autoSelect_x", value = TRUE)
    }
  }
})


# Optimize segmentation threshold
shiny::observeEvent(input$optimize_thresholds_x, {
  if (is_video_capture(the_video) & is_image(the_background) &
    n_row(the_image) == n_row(the_background) &
    n_col(the_image) == n_col(the_background)) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$video_controls[1], input$video_controls[3],
      length.out = 20
    ))

    background <- the_background$copy()
    if (input$dark_button_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    if (!is_image(the_mask) | n_row(the_image) != n_row(the_mask) |
      n_col(the_image) != n_col(the_mask)) {
      mask <- reticulate::np_array(
        array(1L, c(n_row(the_background), n_col(the_background), 3)),
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
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$dark_button_x, {
  refresh_display(refresh_display() + 1)
})
