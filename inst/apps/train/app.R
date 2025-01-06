#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(htmlwidgets)
library(Rvision)
library(data.table)
library(plotly)
library(processx)
library(cli)


#--------------------------------------------------------------
# Custom functions
#--------------------------------------------------------------
source("FUNCTIONS/toggler.R", local = FALSE)


#--------------------------------------------------------------
# User Interface
#--------------------------------------------------------------
ui <- function(request) {
  fluidPage(
    tags$head(includeCSS("www/custom.css")),
    useShinyjs(),
    extendShinyjs(text = "shinyjs.replace = function(url) {
                            location.replace(url);
                          }", functions = "replace"),
    div(id = "curtain", class = "curtain"),
    div(
      style = "width: 100%;",
      div(
        style = "width: min(100vh, calc(100% - 410px));
            float: left;
            margin-top: 20px;
            margin-left: calc((calc(100% - 410px) -
              min(100vh, calc(100% - 410px))) / 2);",
        class = "vrtc-tab-panel-container",
        uiOutput("display")
      ),
      div(
        style = "width: 400px; margin-left: calc(100% - 400px);",
        verticalTabsetPanel(
          id = "main",
          contentWidth = 11,
          menuSide = "right",
          selected = "1",
          source("UI/train.R", local = TRUE)$value,
          source("UI/check.R", local = TRUE)$value
        )
      )
    )
  )
}


#--------------------------------------------------------------
# Application server
#--------------------------------------------------------------
server <- function(input, output, session) {
  source("SERVER/global.R", local = TRUE)
  source("SERVER/train.R", local = TRUE)
  source("SERVER/check.R", local = TRUE)
  session$onSessionEnded(function() {
    if (!is.null(yolo_proc)) {
      yolo_proc$kill_tree()
    }
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
