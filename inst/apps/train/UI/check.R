shinyWidgets::verticalTabPanel(
  title = "2",
  box_height = "100%",
  shiny::p("Checking module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton(
    "video_file",
    "Select video",
    "Please select a video file",
    FALSE,
    class = "fullWidth"
  ),
  shiny::hr(),
  shinyFiles::shinyFilesButton(
    "mask_file",
    "Select mask (optional)",
    "Please select an optional mask file",
    FALSE,
    class = "fullWidth"
  ),
  shiny::hr(),
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
  shiny::numericInput(
    "max_objects_x",
    "Maximum number of objects to detect:",
    300,
    1,
    NA,
    1,
    "100%"
  ),

  shiny::hr(),
  
  shiny::tags$table(
    shiny::tags$tr(
      shiny::tags$td(
        shinyWidgets::prettyToggle(
          inputId = "show_class",
          label_on = "Show classes",
          label_off = "Show classes",
          value = TRUE,
          shape = "curve",
          outline = TRUE,
          bigger = TRUE,
          width = "100%"
        ),
        style = "width: 49%; text-align:center;"
      ),
      shiny::tags$td(),
      shiny::tags$td(
        shinyWidgets::prettyToggle(
          inputId = "show_box",
          label_on = "Show boxes",
          label_off = "Show boxes",
          value = TRUE,
          shape = "curve",
          outline = TRUE,
          bigger = TRUE,
          width = "100%"
        ),
        style = "width: 49%; text-align:center;"
      )
    ),
    class = "stateTable",
    style = "margin-bottom: -15px;"
  ),

  shiny::hr()
)
