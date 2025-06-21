# Globals and reactives
video_range <- c(0, 0)


# UI
output$control_panel <- shiny::renderUI({
  shinyjs::hidden({
    shiny::div(
      id = "controls",
      style = "width: 90%; margin: 0 auto; padding-bottom: 0px;",
      shinyWidgets::noUiSliderInput(
        "video_controls_x",
        NULL,
        0,
        0,
        c(0, 0, 0),
        step = 1,
        width = "100%",
        color = "#2b4e8d",
        tooltips = TRUE,
        pips = list(mode = "count", values = 5, density = 0),
        format = shinyWidgets::wNumbFormat(decimals = 0)
      )
    )
  })
})

shiny::observeEvent(refresh_video(), {
  test_1 <- refresh_video() > 0
  test_2 <- input$main %in% c("1", "2", "3")
  test_3 <- trackRcv::is_video_capture(the_video)

  if (test_1 & test_2 & test_3) {
    shinyjs::show("controls")
  } else {
    shinyjs::hide("controls")
  }
})

shiny::observeEvent(input$main, {
  test_1 <- refresh_video() > 0
  test_2 <- input$main %in% c("1", "2", "3")
  test_3 <- trackRcv::is_video_capture(the_video)

  if (test_1 & test_2 & test_3) {
    shinyjs::show("controls")
  } else {
    shinyjs::hide("controls")
  }
})


# Update control sliders
shiny::observeEvent(refresh_video(), {
  test_1 <- refresh_video() > 0
  test_2 <- trackRcv::is_video_capture(the_video)

  if (test_1 & test_2) {
    min_val <- 1
    max_val <- trackRcv::n_frames(the_video)
    val <- 1
    video_range <<- c(1, trackRcv::n_frames(the_video))

    shinyWidgets::updateNoUiSliderInput(
      session,
      "video_controls_x",
      range = c(1, trackRcv::n_frames(the_video)),
      value = c(min_val, val, max_val)
    )
  } else {
    shinyWidgets::updateNoUiSliderInput(
      session,
      "video_controls_x",
      range = c(0, 0),
      value = c(0, 0, 0)
    )
  }
})

shiny::observeEvent(input$video_controls_x, {
  if (input$video_controls_x[1] != video_range[1]) {
    new_values <- input$video_controls_x
    video_range[1] <<- new_values[1]
    shinyWidgets::updateNoUiSliderInput(
      session,
      "video_controls_x",
      value = new_values
    )
  } else if (input$video_controls_x[3] != video_range[2]) {
    new_values <- input$video_controls_x
    video_range[2] <<- new_values[3]
    shinyWidgets::updateNoUiSliderInput(
      session,
      "video_controls_x",
      value = new_values
    )
  }
})
