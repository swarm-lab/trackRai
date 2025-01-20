shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Video module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("videoStatus"),
  shinyFiles::shinyFilesButton("videoFile_x", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr()
)
