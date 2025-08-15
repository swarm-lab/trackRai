shinyWidgets::verticalTabsetPanel(
  id = "bookmarking",
  contentWidth = 11,
  menuSide = "right",

  shinyWidgets::verticalTabPanel(
    title = "X",
    box_height = "100%",

    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shinyFiles::shinySaveButton(
            "save_state",
            "Save state",
            "Save state as...",
            filetype = list(R = c("Rds", "rds")),
            class = "fullWidth"
          ),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shinyFiles::shinyFilesButton(
            "load_state",
            "Load state",
            "Please select a state file",
            FALSE,
            class = "fullWidth"
          ),
          style = "width: 49%;"
        )
      ),

      shiny::tags$tr(),

      shiny::tags$tr(
        shiny::tags$td(
          colspan = "3",
          shiny::actionButton("reset", "Reset state", width = "100%")
        )
      ),

      class = "stateTable"
    ),

    shiny::p()
  )
)
