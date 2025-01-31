shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "3",
    box_height = "100%",
    shiny::p("Mask module", class = "module-title"),
    shiny::hr(),
    shinyFiles::shinyFilesButton("maskFile_x", "Select existing mask",
      "Please select a mask file", FALSE,
      class = "fullWidth"
    ),
    shiny::hr(),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(actionButton("includeAll_x", "Include all", width = "100%"),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(actionButton("excludeAll_x", "Exclude all", width = "100%"),
          style = "width: 49%;"
        )
      ),
      shiny::tags$tr(),
      shiny::tags$tr(
        shiny::tags$td(actionButton("polyButton_x", "Add polygon ROI", width = "100%"),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(actionButton("ellButton_x", "Add ellipse ROI", width = "100%"),
          style = "width: 49%;"
        )
      ),
      class = "settingsTable"
    ),
    shiny::p(),
    shiny::div(
      style = "text-align: center;",
      shinyWidgets::awesomeRadio(
        inputId = "incButton_x", label = NULL,
        choices = c("Including", "Excluding"), selected = "Including",
        inline = TRUE, checkbox = TRUE, width = "100%"
      )
    ),
    shiny::numericInput("roi_x", "ROI id", 1, 1, 255, 1, "100%"),
    shiny::hr(),
    shinyFiles::shinySaveButton("save_mask_x", "Save mask file", "Save mask as...",
      filetype = list(picture = c("png", "jpg")),
      class = "fullWidth"
    ),
    shiny::p(style = "padding-bottom: 10px;")
  )
)
