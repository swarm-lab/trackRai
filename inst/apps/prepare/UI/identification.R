disabled(
  verticalTabPanel(
    title = "5",
    box_height = "100%",
    p("Identification module", class = "module-title"),
    hr(),
    sliderInput("shapeBuffer_x", "Shape buffer", 1, 50, 5, 1),
    tags$table(
      style = "margin-bottom: -15px",
      tags$tr(
        tags$td(
          actionButton(
            "computeStats_x", "Compute object stats",
            width = "100%"
          ),
          style = "width: 54%; vertical-align: top; padding: 0px;"
        ),
        tags$td(HTML("&nbsp;with&nbsp;"), style = "vertical-align: top; padding-top: 7px"),
        tags$td(
          autonumericInput(
            "nIDFrames_x", NULL, 100, "100%",
            decimalPlaces = 0, currencySymbol = " frames", currencySymbolPlacement = "s",
            minimumValue = 1
          ),
          style = "width: 36%; padding-top: 0px; "
        )
      )
    ),
    hr(),
    plotlyOutput("stats", width = "308px", height = "308px"),
    hr(),
    div(
      style = "text-align: center;",
      checkboxInput("autoSelect_x", "Automatic object selection", FALSE)
    ),
    numericRangeInput("rangeWidth_x", "Width range",
      width = "100%", min = 0, max = NA,
      value = c(0, 1), step = 1
    ),
    numericRangeInput("rangeHeight_x", "Height range",
      width = "100%", min = 0, max = NA,
      value = c(0, 1), step = 1
    ),
    hr(),
    htmlOutput("videoSlider3")
  )
)
