shinyWidgets::verticalTabPanel(
  title = "2",
  box_height = "100%",
  shiny::p("Checking module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton("video_file_x", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shinyFiles::shinyFilesButton("maskFile_x", "Select mask (optional)",
    "Please select an optional mask file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::sliderInput("frame_x", "Frame", 0, 1, 0, 1),
  shiny::hr(),
  shiny::sliderInput(
    "conf_x", "Minimum confidence threshold:",
    0, 1, 0.25, 0.01, width = "100%"
  ),
  shiny::sliderInput(
    "iou_x", "Intersection over union threshold:",
    0, 1, 0.7, 0.01, width = "100%"
  ),
  shiny::numericInput(
    "maxObjects_x", "Maximum number of objects to detect:",
    300, 1, NA, 1, "100%"
  ),
  shiny::hr()
)
