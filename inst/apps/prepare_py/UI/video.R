verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Video module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),

  tags$div(
    class = "panel panel-default",
    shinyFilesButton("videoFile_x", "Select video", multiple = TRUE,
                     "Please select a video file", FALSE, class = "fullWidth")
  ),

  hr(),

  htmlOutput("rangeSlider"),

  htmlOutput("videoSlider")
)
