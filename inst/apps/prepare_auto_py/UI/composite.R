disabled(
  verticalTabPanel(
    title = "6",
    box_height = "100%",
    p("Composite module", class = "module-title"),
    hr(),
    tags$table(
      tags$tr(
        tags$td(
          numericInput("nObjects_x", "Number of objects", 100, 1, Inf, 1, "100%"),
          style = "width: 49%;"
        ),
        tags$td(),
        tags$td(
          numericInput("buffer_x", "Mask buffer", 25, 1, Inf, 1, "100%"),
          style = "width: 49%;"
        )
      )
    ),
    sliderInput("saltpepper_x", "Add random image noise", 0, 50, 0),
    sliderInput("gain_x", "Add random gain", 0, 1, 0, 0.05),
    sliderInput("bias_x", "Add random bias", 0, 50, 0),
    actionButton("test_composite_x", "Generate test composite", width = "100%"),
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

    div(
      style = "text-align: center;",
      checkboxInput("export_video_x", "Include reframed video", value = FALSE)
    ),

    hr()
  )
)
