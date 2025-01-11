shinyWidgets::verticalTabPanel(
  title = "2",
  box_height = "100%",
  shiny::p("Checking module", class = "module-title"),
  shiny::hr(),
  shiny::sliderInput("frame_x", "Frame", 0, 1, 0, 1),
  shiny::hr(),
  shiny::numericInput(
    "maxObjects_x", "Maximum number of objects to detect:",
    300, 1, NA, 1, "100%"
  ),
  shiny::hr()
)
