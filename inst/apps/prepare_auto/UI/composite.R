shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "6",
    box_height = "100%",
    shiny::p("Composite module", class = "module-title"),
    shiny::hr(),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::numericInput(
            "n_instances_x",
            "Number of instances",
            100,
            1,
            Inf,
            1,
            "100%"
          ),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          numericInput("mask_buffer_x", "Mask buffer", 25, 0, Inf, 1, "100%"),
          style = "width: 49%;"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          numericInput(
            "overlap_buffer_x",
            "Overlap buffer",
            10,
            0,
            Inf,
            1,
            "100%"
          ),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          numericInput("saltpepper_x", "Random noise", 0, 0, 127, 1, "100%"),
          style = "width: 49%;"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          numericInput("gain_x", "Random gain", 0, 0, 1, 0.05, "100%"),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          numericInput("bias_x", "Random bias", 0, 0, 127, 1, "100%"),
          style = "width: 49%;"
        )
      )
    ),
    # shiny::sliderInput("saltpepper_x", "Add random image noise", 0, 50, 0),
    # shiny::sliderInput("gain_x", "Add random gain", 0, 1, 0, 0.05),
    # shiny::sliderInput("bias_x", "Add random bias", 0, 50, 0),
    shiny::actionButton(
      "test_composite",
      "Generate test composite",
      width = "100%"
    ),
    shiny::hr(),
    shiny::tags$b("Number of images for..."),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::numericInput(
            "n_train_x",
            "Training",
            140,
            1,
            Inf,
            1,
            "100%"
          ),
          style = "width: 32%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::numericInput(
            "n_validate_x",
            "Validation",
            30,
            1,
            Inf,
            1,
            "100%"
          ),
          style = "width: 32%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::numericInput(
            "n_test_x",
            "Testing",
            30,
            1,
            Inf,
            1,
            "100%"
          ),
          style = "width: 32%;"
        )
      )
    ),
    shiny::hr(),

    shinyFiles::shinyDirButton(
      "generate_dataset",
      "Generate YOLO dataset",
      "Save dataset in...",
      class = "fullWidth"
    ),

    shiny::div(
      style = "text-align: center;",
      shiny::checkboxInput(
        "export_video_x",
        "Include reframed video",
        value = FALSE
      )
    ),

    shiny::hr()
  )
)
