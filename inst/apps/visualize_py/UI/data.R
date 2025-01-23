shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Data module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton("video_file_x", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::htmlOutput("trackStatus"),
  shinyFiles::shinyFilesButton("trackFile_x", "Select tracks",
    "Please select the file containing the tracks", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::tags$table(
    shiny::tags$tr(
      shiny::tags$td(
        shiny::numericInput("lineWidth_x", "Line width (pixels):", 1, 0, NA, 1, "100%"),
        style = "width: 49%;"
      ),
      tags$td(),
      tags$td(
        shiny::numericInput("trackLength_x", "Track length (frames):", 30, 0, NA, 1, "100%"),
        style = "width: 49%;"
      )
    ),
    class = "settingsTable"
  ),
  shiny::hr(),
  shiny::uiOutput("exportControls")
)
