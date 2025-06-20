#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(trackRai)


#--------------------------------------------------------------
# Custom functions
#--------------------------------------------------------------


#--------------------------------------------------------------
# User Interface
#--------------------------------------------------------------
shiny::addResourcePath(
  prefix = "share",
  directoryPath = system.file("apps/share", package = "trackRai")
)

ui <- function(request) {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::includeCSS(path = "../share/css/custom.css")
    ),
    shinyjs::useShinyjs(),

    shinyjs::hidden(
      shiny::div(
        shinyWidgets::verticalTabsetPanel(
          id = "main",
          shinyWidgets::verticalTabPanel(
            title = "1"
          )
        )
      )
    ),

    shiny::div(
      style = paste0(
        "width: 400px;",
        "margin-left: auto;",
        "margin-right: auto;"
      ),

      shiny::div(
        class = "vrtc-tab-panel-container",
        style = paste0(
          "margin-left: auto;",
          "margin-right: auto;"
        ),
        shiny::div(
          style = paste0(
            "margin: 20px;"
          ),
          shiny::p("trackRai", class = "module-title"),
          shiny::hr(),

          shiny::div(
            style = paste0(
              "margin-left: 40px;",
              "margin-right: 40px;"
            ),
            shiny::actionButton(
              "prepare",
              " - Prepare",
              icon = shiny::icon("tasks", class = "fa-solid"),
              style = "font-size: 20px;",
              width = "100%"
            ),

            shiny::hr(),

            shiny::actionButton(
              "train",
              " - Train",
              icon = shiny::icon("chalkboard-teacher", class = "fa-solid"),
              style = "font-size: 20px;",
              width = "100%"
            ),

            shiny::hr(),

            shiny::actionButton(
              "track",
              " - Track",
              icon = shiny::icon("search-location", class = "fa-solid"),
              style = "font-size: 20px;",
              width = "100%"
            ),

            shiny::hr(),

            shiny::actionButton(
              "fix",
              " - Fix",
              icon = shiny::icon("tools", class = "fa-solid"),
              style = "font-size: 20px;",
              width = "100%"
            ),

            shiny::hr(),

            shiny::actionButton(
              "visualize",
              " - Visualize",
              icon = shiny::icon("eye", class = "fa-solid"),
              style = "font-size: 20px;",
              width = "100%"
            )
          ),

          shiny::hr()
        )
      )
    )
  )
}


#--------------------------------------------------------------
# Application server
#--------------------------------------------------------------
server <- function(input, output, session) {
  shiny::observeEvent(input$prepare, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/prepare_auto/"
      ),
      options = list(launch.browser = TRUE)
    ))
  })

  shiny::observeEvent(input$train, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/train/"
      ),
      options = list(launch.browser = TRUE)
    ))
  })

  shiny::observeEvent(input$track, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/track/"
      ),
      options = list(launch.browser = TRUE)
    ))
  })

  shiny::observeEvent(input$fix, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRcv"),
        "/apps/trackFixer/"
      ),
      options = list(launch.browser = TRUE)
    ))
  })

  shiny::observeEvent(input$visualize, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRcv"),
        "/apps/trackPlayer/"
      ),
      options = list(launch.browser = TRUE)
    ))
  })

  session$onSessionEnded(function() {})
}

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
