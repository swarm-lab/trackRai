shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Data module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton("video_file", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shinyFiles::shinyFilesButton("mask_file", "Select mask (optional)",
    "Please select an optional mask file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::htmlOutput("yolo_status"),
  shinyFiles::shinyDirButton("dataset_x", "Select trained dataset",
    "Please select the folder containing the trained dataset",
    class = "fullWidth"
  ),
  shiny::uiOutput("modelSelect"),
  shiny::hr()
)
