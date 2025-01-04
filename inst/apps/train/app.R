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
        div(
          style = "padding-left: 20px; padding-right: 20px; padding-top: 20px;",
          plotlyOutput("display", width = "100%", height = "420px"),
          hr()
        ),
        div(
          htmlOutput("console"),
          style = "padding-left: 20px; padding-right: 20px; padding-bottom: 20px;"
        ),
        tags$head(
          tags$style(
            "#console{
              font-size: 10px;
              font-family: monospace;
              overflow-y:auto;
              height: 200px;
              display: flex;
              flex-direction: column-reverse;
              border-style: solid;
              border-width: 1px;
              border-color: black;
              border-radius: 5px;
              background-color: #e5e5e5;
              width: 100%;
              margin: auto;
              padding: 10px;
              text-align: start;
            }"
          )
        )
      ),
      div(
        style = "width: 400px; margin-left: calc(100% - 400px);",
        verticalTabsetPanel(
          id = "main",
          contentWidth = 11,
          menuSide = "right",
          selected = "1",
          source("UI/train.R", local = TRUE)$value
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
  session$onSessionEnded(function() { })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
