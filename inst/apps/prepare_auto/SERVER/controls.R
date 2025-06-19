# Globals and reactives
shinyjs::hideElement("curtain")
toggled_tabs <- data.table::data.table(
  tab = 1:6,
  toggled = c(TRUE, rep(FALSE, 5))
)

black_screen <- reticulate::r_to_py(array(0L, c(1080, 1920, 3)))
to_display <- NULL

refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)


# UI
output$control_panel <- shiny::renderUI({
  shiny::tagList(
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
    }),
    shinyjs::hidden({
      shiny::div(
        id = "IDcontrols",
        style = "width: 90%; margin: 0 auto; padding-bottom: 0px;",
        shinyWidgets::noUiSliderInput(
          "id_controls",
          NULL,
          0,
          0,
          0,
          step = 1,
          width = "100%",
          color = "#2b4e8d",
          tooltips = TRUE,
          pips = list(mode = "count", values = 5, density = 0),
          format = shinyWidgets::wNumbFormat(decimals = 0)
        )
      )
    })
  )
})

shiny::observeEvent(input$main, {
  test_1 <- input$main %in% c("1", "4")
  test_2 <- trackRcv::is_video_capture(the_video)
  test_3 <- input$main %in% c("5")

  if (test_1 & test_2) {
    shinyjs::show("controls")
    shinyjs::hide("IDcontrols")
  } else {
    shinyjs::hide("controls")

    if (test_3) {
      shinyjs::show("IDcontrols")
      n <- input$video_controls_x[3] - input$video_controls_x[1] + 1
      step <- floor(n / input$n_ID_frames_x)
      id_frames(seq.int(
        input$video_controls_x[1],
        input$video_controls_x[3],
        step
      )[1:input$n_ID_frames_x])
    } else {
      shinyjs::hide("IDcontrols")
    }
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

  if (input$main == "1") {
    shinyWidgets::updateVerticalTabsetPanel(session, "main", "2")
    shinyWidgets::updateVerticalTabsetPanel(session, "main", "1")
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

shiny::observeEvent(id_frames(), {
  shinyWidgets::updateNoUiSliderInput(
    session,
    "id_controls",
    range = c(1, length(id_frames())),
    value = 1
  )
})
