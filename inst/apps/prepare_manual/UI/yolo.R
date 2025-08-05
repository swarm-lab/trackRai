shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "3",
    box_height = "100%",
    shiny::p("YOLO module", class = "module-title"),
    shiny::hr(),
    shiny::tags$b("Percent of images for..."),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::numericInput(
            "percent_train_x",
            "Training",
            70,
            0,
            100,
            1,
            "100%"
          ),
          style = "width: 32%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::numericInput(
            "percent_validate_x",
            "Validation",
            15,
            0,
            100,
            1,
            "100%"
          ),
          style = "width: 32%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::numericInput(
            "percent_test_x",
            "Testing",
            15,
            0,
            100,
            1,
            "100%"
          ),
          style = "width: 32%;"
        )
      ),
      class = "stateTable",
      style = "margin-bottom: -10px !important;"
    ),
    shiny::hr(),

    shinyFiles::shinyDirButton(
      "generate_dataset",
      "Generate YOLO dataset",
      "Save dataset in...",
      class = "fullWidth"
    ),
    shiny::hr()
  )
)
