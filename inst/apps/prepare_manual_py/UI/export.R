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
          numericInput("nTrain_x", "Training", 140, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        ),
        tags$td(),
        tags$td(
          numericInput("nValidate_x", "Validation", 30, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        ),
        tags$td(),
        tags$td(
          numericInput("nTest_x", "Testing", 30, 1, Inf, 1, "100%"),
          style = "width: 32%;"
        )
      )
    ),
    hr(),

    shinyDirButton("generate_dataset_x", "Generate YOLO dataset", "Save dataset in...", class = "fullWidth"),

    hr()
  )
)