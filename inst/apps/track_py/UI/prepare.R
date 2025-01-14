shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Preparation module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("videoStatus"),
  shinyFiles::shinyFilesButton("videoFile_x", "Select video",
    multiple = TRUE,
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::htmlOutput("rangeSlider"),
  shiny::htmlOutput("videoSlider"),
  shiny::hr(),
  shiny::htmlOutput("yoloStatus"),
  shinyFiles::shinyDirButton("dataset_x", "Select trained dataset",
    "Please select the folder containing the trained dataset",
    class = "fullWidth"
  ),
  shiny::uiOutput("modelSelect"),
  shiny::hr(),
  shiny::uiOutput("startStop")
)
