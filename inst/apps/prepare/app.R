#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(htmlwidgets)
library(wand)
library(plotly)
library(sortable)
library(Rvision)
library(data.table)


#--------------------------------------------------------------
# Custom functions
#--------------------------------------------------------------
source("FUNCTIONS/backgrounder.R", local = FALSE)
source("FUNCTIONS/toggler.R", local = FALSE)
source("FUNCTIONS/ellipse.R", local = FALSE)


#--------------------------------------------------------------
# Global Variables
#--------------------------------------------------------------


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
              min(100vh, calc(100% - 410px))) / 2)",
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
          source("UI/video.R", local = TRUE)$value,
          source("UI/background.R", local = TRUE)$value,
          source("UI/mask.R", local = TRUE)$value,
          source("UI/segmentation.R", local = TRUE)$value,
          source("UI/identification.R", local = TRUE)$value,
          source("UI/composite.R", local = TRUE)$value
        ),
        verticalTabsetPanel(
          id = "settings",
          contentWidth = 11,
          menuSide = "right",
          source("UI/controls.R", local = TRUE)$value
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
  source("SERVER/video.R", local = TRUE)
  source("SERVER/background.R", local = TRUE)
  source("SERVER/mask.R", local = TRUE)
  source("SERVER/segmentation.R", local = TRUE)
  source("SERVER/identification.R", local = TRUE)
  source("SERVER/composite.R", local = TRUE)
  source("SERVER/controls.R", local = TRUE)
  session$onSessionEnded(function() { })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
