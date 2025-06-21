shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "2",
    box_height = "100%",
    shiny::p("Scaling module", class = "module-title"),

    shiny::hr(),

    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton("origin_button", "Set origin", width = "100%"),
          style = "width: 49%;"
        ),
        shiny::tags$td(),
        shiny::tags$td(
          shiny::actionButton("scale_button", "Set scale", width = "100%"),
          style = "width: 49%;"
        )
      ),

      class = "stateTable"
    ),

    shiny::p(),

    shiny::htmlOutput("scale_status", container = p),

    shiny::hr()
  )
)
