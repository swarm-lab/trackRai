#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
if (Sys.info()["sysname"] == "Darwin") {
  Sys.setenv(KMP_DUPLICATE_LIB_OK=TRUE)
}

library(reticulate)
reticulate::use_condaenv("trackRai")
torch <- reticulate::import("torch", convert = FALSE)
cv2 <- import("cv2", convert = FALSE)
np <- import("numpy", convert = FALSE)
base64 <- import("base64", convert = FALSE)
ul <- import("ultralytics", convert = FALSE)

library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(htmlwidgets)
library(trackRai)
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
  shiny::fluidPage(
    shiny::tags$head(includeCSS("www/custom.css")),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = "custom.js",
      functions = c("uishape")
    ),
    shiny::div(id = "curtain", class = "curtain"),
    shiny::div(
      style = "width: 100%;",
      shiny::div(
        style = "width: min(100vh, calc(100% - 410px));
            float: left;
            margin-top: 20px;
            margin-left: calc((calc(100% - 410px) -
              min(100vh, calc(100% - 410px))) / 2);",
        class = "vrtc-tab-panel-container",
        shiny::uiOutput("display")
      ),
      shiny::div(
        style = "width: 400px; margin-left: calc(100% - 400px);",
        shinyWidgets::verticalTabsetPanel(
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

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
