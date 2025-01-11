#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
if (Sys.info()["sysname"] == "Darwin") {
  Sys.setenv(KMP_DUPLICATE_LIB_OK=TRUE)
}

library(reticulate)
reticulate::use_condaenv("trackRai")
cv2 <- import("cv2", convert = FALSE)
np <- import("numpy", convert = FALSE)
base64 <- import("base64", convert = FALSE)

library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(htmlwidgets)
library(trackRai)
library(autothresholdr)
library(stringr)
library(plotly)
library(data.table)


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
              min(100vh, calc(100% - 410px))) / 2)",
        class = "vrtc-tab-panel-container",
        shiny::uiOutput("display")
      ),
      shiny::div(
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
  session$onSessionEnded(function() { })
}

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
