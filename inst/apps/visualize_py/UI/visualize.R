shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Visualization module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton("video_file", "Select video",
    "Please select a video file", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::htmlOutput("track_status"),
  shinyFiles::shinyFilesButton("track_file_x", "Select tracks",
    "Please select the file containing the tracks", FALSE, class = "fullWidth"
  ),
  shiny::hr(),
  shiny::tags$table(
    shiny::tags$tr(
      shiny::tags$td(
        shiny::numericInput("line_width_x", "Line width (pixels):", 1, 0, NA, 1, "100%"),
        style = "width: 49%;"
      ),
      tags$td(),
      tags$td(
        shiny::numericInput("track_length_x", "Track length (frames):", 30, 0, NA, 1, "100%"),
        style = "width: 49%;"
      )
    ),
    class = "stateTable"
  ),
  shiny::hr(),
  shiny::uiOutput("export_controls")
)
