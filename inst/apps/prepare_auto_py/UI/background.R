shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "2",
    box_height = "100%",
    shiny::p("Background module", class = "module-title"),
    shiny::hr(),
    shiny::htmlOutput("background_status", container = p, class = "good"),
    shinyFiles::shinyFilesButton("backgroundFile_x", "Select existing background",
      "Please select a background image", FALSE,
      class = "fullWidth"
    ),
    shiny::hr(class = "hr-text", `data-content` = "Or"),
    shiny::actionButton("computeBackground_x", "Automatically estimate background", width = "100%"),
    shiny::p(style = "padding-bottom: 10px;"),
    shiny::selectInput("backgroundType_x", "Background type:",
      choices = c("Median" = "median", "Mean" = "mean", "Minimum" = "min", "Maximum" = "max"),
      width = "100%"
    ),
    shiny::sliderInput("backroundImages_x", "Number of frames for estimating background:",
      min = 1, max = 200,
      value = 25, width = "100%"
    ),
    shiny::hr(),
    shiny::actionButton("ghostButton_x", "Select ghost for removal", width = "100%"),
    shiny::hr(),
    shinyFiles::shinySaveButton("save_background_x", "Save background file", "Save background as...",
      filetype = list(picture = c("png", "jpg")),
      class = "fullWidth"
    ),
    shiny::p(style = "padding-bottom: 10px;")
  )
)
