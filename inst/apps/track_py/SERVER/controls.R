output$control_panel <- shiny::renderUI({
  shinyjs::hidden({
    shiny::div(
      id = "controls",
      style = "width: 90%; margin: 0 auto; padding-bottom: 0px;",
      shinyWidgets::noUiSliderInput(
        "video_controls", NULL, 0, 0, c(0, 0, 0),
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

shiny::observe({
  test_1 <- refresh_video() > 0
  test_2 <- input$main %in% c("1")
  test_3 <- trackRai::is_video_capture(the_video)

  if (test_1 & test_2 & test_3) {
    shinyjs::show("controls")
  } else {
    shinyjs::hide("controls")
  }
})

shiny::observe({
  test_1 <- refresh_video() > 0
  test_2 <- trackRai::is_video_capture(the_video)

  if (test_1 & test_2) {
    isolate({
      min_val <- 1
      max_val <- trackRai::n_frames(the_video)
      val <- 1

      shinyWidgets::updateNoUiSliderInput(
        session, "video_controls",
        range = c(1, trackRai::n_frames(the_video)),
        value = c(min_val, val, max_val)
      )
    })
  } else {
    shinyWidgets::updateNoUiSliderInput(
      session, "video_controls",
      range = c(0, 0),
      value = c(0, 0, 0)
    )
  }
})
