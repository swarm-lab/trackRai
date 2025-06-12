shinyjs::disabled(
  shinyWidgets::verticalTabPanel(
    title = "2",
    box_height = "100%",
    shiny::p("Tagging module", class = "module-title"),
    shiny::hr(),
    shiny::actionButton("new_object_x", "Add new object [N]", width = "100%"),
    shiny::hr(),
    # shiny::sliderInput("hw_ratio_x", "Standard height/width ratio", 1, 10, 2, 0.1),
    # shiny::hr(),
    shiny::div(
      style = "text-align: center;",
      shiny::h5("Click inside a box to select an object"),
    ),
    # shiny::hr(),
    # tags$table(
    #   tags$tr(
    #     tags$td(
    #       numericInput("height_x", "Adjust height", 30, 1, Inf, 1, "100%"),
    #       style = "width: 49%;"
    #     ),
    #     tags$td(),
    #     tags$td(
    #       numericInput("width_x", "Adjust width", 10, 1, Inf, 1, "100%"),
    #       style = "width: 49%;"
    #     )
    #   )
    # ),
    shiny::actionButton("remove_object_x", "Remove object [R]", width = "100%"),
    shiny::hr(),
    shiny::selectInput("tagged_frame_x", "Go to tagged frame:", NULL, width = "100%"),
    tags$table(
      tags$tr(
        tags$td(
          tags$b("Tagged frames: "),
          textOutput("frame_count", inline = TRUE),
          style = "width: 49%;"
        ),
        tags$td(),
        tags$td(
          tags$b("Tagged objects: "),
          textOutput("object_count", inline = TRUE),
          style = "width: 49%;"
        )
      ),
      width = "100%"
    ),
    shiny::hr(),
    shiny::p(style = "padding-bottom: 10px;")
  )
)