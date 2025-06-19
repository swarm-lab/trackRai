shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Video module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton("video_file", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr()
)
