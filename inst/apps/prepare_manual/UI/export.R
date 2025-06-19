disabled(
  verticalTabPanel(
    title = "3",
    box_height = "100%",
    p("Export module", class = "module-title"),
    hr(),
    tags$b("Number of images for..."),
    tags$table(
      tags$tr(
        tags$td(
          numericInput("n_train", "Training", 140, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        ),
        tags$td(),
        tags$td(
          numericInput("n_validate", "Validation", 30, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        ),
        tags$td(),
        tags$td(
          numericInput("n_test", "Testing", 30, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        )
      )
    ),
    hr(),

    shinyDirButton("generate_dataset", "Generate YOLO dataset", "Save dataset in...", class = "fullWidth"),

    hr()
  )
)