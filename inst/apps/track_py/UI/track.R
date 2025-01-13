shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Tracking module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("yoloStatus"),
  shiny::htmlOutput("videoStatus"),
  shinyFiles::shinyDirButton("dataset_x", "Select trained dataset",
    "Please select the folder containing the trained dataset",
    class = "fullWidth"
  ),
  shiny::uiOutput("modelSelect"),
  shiny::hr(),
  shinyFiles::shinyFilesButton("videoFile_x", "Select video",
    multiple = TRUE,
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::htmlOutput("rangeSlider"),
  shiny::htmlOutput("videoSlider"),
  shiny::uiOutput("startStop"),
  shiny::hr()
)
