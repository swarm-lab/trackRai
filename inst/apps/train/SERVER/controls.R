# UI
output$control_panel <- shiny::renderUI({
  shinyjs::hidden({
    shiny::div(
      id = "controls",
      style = "width: 90%; margin: 0 auto; padding-bottom: 0px;",
      shinyWidgets::noUiSliderInput(
        "video_controls_x", NULL, 0, 0, c(0),
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
  test_2 <- input$main %in% c("2")
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
    val <- 1

    shinyWidgets::updateNoUiSliderInput(
      session, "video_controls_x",
      range = c(1, trackRcv::n_frames(the_video)),
      value = c(val)
    )
  } else {
    shinyWidgets::updateNoUiSliderInput(
      session, "video_controls_x",
      range = c(0, 0),
      value = c(0)
    )
  }
})
