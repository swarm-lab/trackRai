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
      style = "margin-bottom: -15px;"
    ),

    shiny::hr(),

    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shinyWidgets::prettyToggle(
            inputId = "enrich_x",
            label_on = "Enrich dataset with...",
            label_off = "Enrich dataset  with...",
            value = FALSE,
            shape = "curve",
            outline = TRUE,
            bigger = TRUE,
            width = "100%"
          ),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shinyWidgets::autonumericInput(
            "enrich_n_x",
            NULL,
            5,
            "100%",
            decimalPlaces = 0,
            currencySymbol = " copies",
            currencySymbolPlacement = "s",
            minimumValue = 1,
            wheelOn = "hover",
            modifyValueOnWheel = TRUE,
            wheelStep = 1,
            caretPositionOnFocus = NULL
          ),
          style = "width: 49%;"
        )
      ),
      class = "stateTable",
      style = "margin-bottom: -15px;"
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
