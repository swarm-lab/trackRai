shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "4",
    box_height = "100%",
    shiny::p("Segmentation module", class = "module-title"),
    shiny::hr(),
    shiny::div(
      style = "text-align: center;",
      shinyWidgets::awesomeRadio(
        inputId = "dark_button_x",
        label = "Are the objects darker or lighter than the background?",
        choices = c("Darker", "Lighter", "A bit of both"),
        selected = "Darker",
        inline = TRUE,
        checkbox = TRUE,
        width = "100%"
      )
    ),
    shiny::hr(class = "hr-text", `data-content` = "And either..."),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$td(
          shiny::actionButton(
            "optimize_thresholds",
            "Autothreshold",
            width = "100%"
          ),
          style = "width: 39%; padding-top: 4px"
        ),
        shiny::tags$td(shiny::HTML("&nbsp;with&nbsp;")),
        shiny::tags$td(
          shiny::selectInput(
            "threshold_method_x",
            "",
            c(
              "Otsu's method" = "Otsu",
              "Renyi's entropy" = "RenyiEntropy",
              "ImageJ's method" = "IJDefault",
              "Huang's method" = "Huang",
              "Huang-Schindelin's method" = "Huang2",
              "Intermodes method" = "Intermodes",
              "Minimum method" = "Minimum",
              "Ridler-Calvar's isodata" = "IsoData",
              "Li's method" = "Li",
              "Kapur-Sahoo-Wong's method" = "MaxEntropy",
              "Mean method" = "Mean",
              "Kittler-Illingworth’s method" = "MinErrorI",
              "Tsai’s method" = "Moments",
              "Percentile method" = "Percentile",
              "Shanbhag's method" = "Shanbhag",
              "Triangle method" = "Triangle",
              "Yen's method" = "Yen"
            ),
            width = "100%"
          ),
          style = "width: 59%;"
        )
      )
    ),
    shiny::hr(class = "hr-text", `data-content` = "Or manual threshold"),
    shiny::sliderInput(
      "threshold_x",
      NULL,
      width = "100%",
      min = 1,
      max = 255,
      value = 30,
      step = 1
    ),
    shiny::hr(),
    shiny::sliderInput(
      "smooth_x",
      "Contour smoothing",
      width = "100%",
      min = 0,
      max = 10,
      value = 0,
      step = 0.1
    ),
    shiny::hr()
  )
)
