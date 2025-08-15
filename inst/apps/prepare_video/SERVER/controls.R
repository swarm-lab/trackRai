# Globals and reactives
shinyjs::hideElement("curtain")
toggled_tabs <- data.table::data.table(
  tab = 1,
  toggled = TRUE
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
        ),
        shiny::div(
          style = "padding-bottom: 10px;",
          shiny::span("-1 second"),
          shiny::icon(
            "square-caret-down",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          ),
          shiny::span(" | "),
          shiny::span("-1 frame"),
          shiny::icon(
            "square-caret-left",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          ),
          shiny::span(" | "),
          shiny::span("+1 frame"),
          shiny::icon(
            "square-caret-right",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          ),
          shiny::span(" | "),
          shiny::span("+1 second"),
          shiny::icon(
            "square-caret-up",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          )
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
        ),
        shiny::div(
          style = "padding-bottom: 10px;",
          shiny::span("-1 frame"),
          shiny::icon(
            "square-caret-left",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          ),
          shiny::span(" | "),
          shiny::span("+1 frame"),
          shiny::icon(
            "square-caret-right",
            class = "fa-regular",
            style = "font-size: 16px; vertical-align: bottom;"
          )
        )
      )
    })
  )
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
    shinyjs::show("controls")
  } else {
    shinyWidgets::updateNoUiSliderInput(
      session,
      "video_controls_x",
      range = c(0, 0),
      value = c(0, 0, 0)
    )
    shinyjs::hide("controls")
  }
})

shiny::observeEvent(input$video_controls_x, {
  shiny::isolate({
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
})