shinyWidgets::verticalTabPanel(
  title = "3",
  box_height = "100%",
  shiny::p("Tracking module", class = "module-title"),
  shiny::hr(),
  shiny::h4("Detection parameters"),
  shiny::numericInput(
    "max_objects_x",
    "Maximum number of objects to detect:",
    30,
    1,
    NA,
    1,
    "100%"
  ),
  shiny::sliderInput(
    "conf_x",
    "Minimum confidence threshold:",
    0,
    1,
    0.25,
    0.01,
    width = "100%"
  ),
  shiny::sliderInput(
    "iou_x",
    "Intersection over union threshold:",
    0,
    1,
    0.7,
    0.01,
    width = "100%"
  ),
  shiny::hr(),
  shiny::h4("Tracking parameters"),
  shiny::sliderInput(
    "assoc_x",
    "Association thresholds:",
    0,
    1,
    c(0.1, 0.25),
    0.01,
    width = "100%"
  ),
  shiny::sliderInput(
    "new_track_x",
    "New track threshold:",
    0,
    1,
    0.25,
    0.01,
    width = "100%"
  ),
  shiny::numericInput(
    "track_buffer",
    "Old track buffer:",
    30,
    1,
    NA,
    1,
    "100%"
  ),
  shiny::sliderInput(
    "match_x",
    "Track matching threshold:",
    0,
    1,
    0.8,
    0.01,
    width = "100%"
  ),
  shiny::hr(),
  shiny::uiOutput("start_stop"),
  shiny::div(
    style = "text-align: center;",
    shiny::checkboxInput("preview", "Show preview", value = FALSE)
  ),
  shiny::hr()
)
