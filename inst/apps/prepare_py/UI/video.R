shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Video module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("videoStatus"),
  shiny::tags$div(
    class = "panel panel-default",
    shinyFiles::shinyFilesButton("videoFile_x", "Select video",
      multiple = TRUE,
      "Please select a video file", FALSE, class = "fullWidth"
    )
  ),
  shiny::hr(),
  shiny::htmlOutput("rangeSlider"),
  shiny::htmlOutput("videoSlider")
)
