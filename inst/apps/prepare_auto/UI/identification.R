shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "5",
    box_height = "100%",
    shiny::p("Identification module", class = "module-title"),
    shiny::hr(),
    shiny::sliderInput("shape_buffer_x", "Shape buffer", 0, 50, 5, 1),
    shiny::tags$table(
      style = "margin-bottom: -15px",
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton(
            "compute_stats",
            "Detect instances",
            width = "100%"
          ),
          style = "width: 54%; vertical-align: top; padding: 0px;"
        ),
        shiny::tags$td(
          shiny::HTML("&nbsp;in&nbsp;"),
          style = "vertical-align: top; padding-top: 7px; text-align: center;"
        ),
        shiny::tags$td(
          shinyWidgets::autonumericInput(
            "n_ID_frames_x",
            NULL,
            100,
            "100%",
            decimalPlaces = 0,
            currencySymbol = " frames",
            currencySymbolPlacement = "s",
            minimumValue = 1,
            wheelStep = 1
          ),
          style = "width: 36%; padding-top: 0px; "
        )
      )
    ),
    shiny::hr(),
    plotly::plotlyOutput("stats", width = "308px", height = "308px"),
    shiny::htmlOutput("blob_stats"),
    shiny::hr(),
    shiny::div(
      style = "text-align: center;",
      shiny::checkboxInput("auto_select_x", "Automatic instance selection", TRUE)
    ),
    shinyWidgets::numericRangeInput(
      "range_width_x",
      "Width range",
      width = "100%",
      min = 0,
      max = NA,
      value = c(0, 0),
      step = 1
    ),
    shinyWidgets::numericRangeInput(
      "range_height_x",
      "Height range",
      width = "100%",
      min = 0,
      max = NA,
      value = c(0, 0),
      step = 1
    ),
    shiny::hr()
  )
)
