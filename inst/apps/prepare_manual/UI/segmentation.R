shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "2",
    box_height = "100%",
    shiny::p("Segmentation module", class = "module-title"),
    shiny::hr(),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton("add_object", "Add object [q]", width = "100%"),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::actionButton(
            "remove_object",
            "Remove object [w]",
            width = "100%"
          ),
          style = "width: 49%;"
        )
      ),
      shiny::tags$tr(),
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton(
            "random_frame",
            "Select a random frame [r]",
            width = "100%"
          ),
          style = "width: 100%;",
          colspan="3"
        )
      ),
      class = "stateTable"
    ),
    shiny::hr(),
    shiny::selectizeInput(
      "object_tag",
      "Object tag (you can add your own tags)",
      "Object",
      width = "100%",
      options = list(
        create = TRUE,
        dropdownParent = "body",
        addPrecedence = TRUE
      )
    ),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shinyWidgets::prettyCheckbox(
            "show_box",
            "Show boxes",
            value = TRUE,
            width = "100%"
          ),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shinyWidgets::prettyCheckbox(
            "show_tag",
            "Show tags",
            value = FALSE,
            width = "100%"
          ),
          style = "width: 49%;"
        )
      ),
      class = "stateTable",
      style = "text-align: center; margin-bottom: -20px !important;"
    ),
    shiny::hr(),
    shiny::tags$label("Tagged frames"),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton(
            "previous_tagged_frame",
            NULL,
            icon = shiny::icon(
              "caret-left",
              class = "fa-regular"
            )
          ),
          style = "width: 10%; vertical-align: top;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::selectInput(
            "tagged_frame",
            NULL,
            NULL,
            width = "100%"
          )
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::actionButton(
            "next_tagged_frame",
            NULL,
            icon = shiny::icon(
              "caret-right",
              class = "fa-regular"
            )
          ),
          style = "width: 10%; vertical-align: top;"
        )
      ),
      class = "stateTable",
      style = "margin-bottom: -20px !important;"
    ),
    shiny::hr(),
    shiny::tableOutput("stats"),
    shiny::hr()
  )
)
