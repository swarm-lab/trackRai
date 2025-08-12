shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Video module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("video_status"),
  shinyFiles::shinyFilesButton(
    "video_file",
    "Select video",
    "Please select a video file",
    FALSE,
    class = "fullWidth"
  ),
  shiny::hr(),
  shinyjs::hidden({
    shiny::div(
      id = "output_controls",
      shiny::tags$table(
        shiny::tags$tr(
          shiny::tags$td(
            shiny::p(
              shiny::strong("Rescaling factor: "),
              style = "width: 100%;"
            ),
            style = "width: 74%;"
          ),
          shiny::tags$td(),
          shiny::tags$td(
            shiny::selectInput(
              "rescale",
              NULL,
              choices = paste0("1:", 1:8),
              selected = "1:1",
              width = "100%"
            ),
            style = "width: 24%;"
          )
        ),
        class = "stateTable",
        style = "margin-bottom: -15px;"
      ),
      shiny::hr(),

      shiny::p(
        shiny::strong("Region of interest: "),
        style = "width: 100%;"
      ),

      shiny::tags$table(
        shiny::tags$tr(
          shiny::tags$td(),
          shiny::tags$td(
            shiny::actionButton("roi_top", "Top", width = "100%"),
            style = "width: 40%;"
          ),
          shiny::tags$td()
        ),
        shiny::tags$tr(),
        class = "stateTable"
      ),

      shiny::tags$table(
        shiny::tags$tr(
          shiny::tags$td(
            shiny::actionButton("roi_left", "Left", width = "100%"),
            style = "width: 40%;"
          ),
          shiny::tags$td(),
          shiny::tags$td(
            shiny::actionButton("roi_right", "Right", width = "100%"),
            style = "width: 40%;"
          )
        ),
        class = "stateTable"
      ),

      shiny::tags$table(
        shiny::tags$tr(),
        shiny::tags$tr(
          shiny::tags$td(),
          shiny::tags$td(
            shiny::actionButton("roi_bottom", "Bottom", width = "100%"),
            style = "width: 40%;"
          ),
          shiny::tags$td()
        ),
        class = "stateTable"
      ),

      shiny::hr(),
      shinyFiles::shinySaveButton(
        "export",
        "Export video",
        "Export video as...",
        filetype = list(video = c("mp4")),
        class = "fullWidth"
      ),
      shiny::hr()
    )
  })
)
