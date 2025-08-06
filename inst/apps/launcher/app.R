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

            shinyWidgets::dropMenu(
              shiny::actionButton(
                "prepare",
                " - Prepare",
                icon = shiny::icon("tasks", class = "fa-solid"),
                style = "font-size: 20px; padding-bottom: 10px !important;",
                width = "100%"
              ),
              shiny::actionButton(
                "prepare_auto",
                " - Auto annotation",
                icon = shiny::icon("gear", class = "fa-solid"),
                style = "font-size: 20px; margin-bottom: 10px;",
                width = "100%"
              ),
              shiny::actionButton(
                "prepare_manual",
                " - Manual annotation",
                icon = shiny::icon("hand", class = "fa-solid"),
                style = "font-size: 20px;",
                width = "100%"
              ),
              icon = shiny::icon("tasks", class = "fa-solid"),
              label = " - Prepare",
              width = "100%",
              maxWidth = "275px"
            ),

            # shiny::actionButton(
            #   "prepare",
            #   " - Prepare",
            #   icon = shiny::icon("tasks", class = "fa-solid"),
            #   style = "font-size: 20px;",
            #   width = "100%"
            # ),

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
  browser <- if (is.null(shiny::getShinyOption("shiny.launch.browser"))) {
    getOption("shiny.launch.browser")
  } else {
    shiny::getShinyOption("shiny.launch.browser")
  }

  shiny::observeEvent(input$prepare_auto, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/prepare_auto/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  shiny::observeEvent(input$prepare_manual, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/prepare_manual/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  shiny::observeEvent(input$train, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/train/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  shiny::observeEvent(input$track, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRai"),
        "/apps/track/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  shiny::observeEvent(input$fix, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRcv"),
        "/apps/fix/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  shiny::observeEvent(input$visualize, {
    shiny::stopApp(shiny::shinyAppDir(
      paste0(
        find.package("trackRcv"),
        "/apps/visualize/"
      ),
      options = list(launch.browser = browser)
    ))
  })

  session$onSessionEnded(function() {})
}

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
