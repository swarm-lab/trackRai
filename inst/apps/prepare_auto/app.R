#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
if (Sys.info()["sysname"] == "Darwin") {
  Sys.setenv(KMP_DUPLICATE_LIB_OK = TRUE)
}

library(reticulate)
reticulate::use_virtualenv("trackR", required = TRUE)
cv2 <- reticulate::import("cv2", convert = FALSE)
np <- reticulate::import("numpy", convert = FALSE)
base64 <- reticulate::import("base64", convert = FALSE)

library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(trackRcv)
library(trackRai)
library(autothresholdr)
library(stringr)
library(plotly)
library(data.table)


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
    shinyjs::extendShinyjs(
      script = "share/js/window.js",
      functions = c("uishape", "imgshape")
    ),
    shinyjs::extendShinyjs(
      script = "share/js/keyboard.js",
      functions = c()
    ),
    shinyjs::extendShinyjs(
      script = "share/js/reset.js",
      functions = c("replace")
    ),
    shiny::div(id = "curtain", class = "curtain"),
    shiny::div(
      style = "width: 100%;",
      shiny::div(
        class = "vrtc-tab-panel-container display-panel",
        shiny::uiOutput("display"),
        source("UI/controls.R", local = TRUE)$value
      ),
      shiny::div(
        style = "width: 400px; margin-left: calc(100% - 400px);",
        shinyWidgets::verticalTabsetPanel(
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
        source("UI/bookmarking.R", local = TRUE)$value
      )
    )
  )
}


#--------------------------------------------------------------
# Application server
#--------------------------------------------------------------
server <- function(input, output, session) {
  source("../share/r/togglers.R", local = TRUE)
  source("../share/r/drawers.R", local = TRUE)
  source("../share/r/samplers.R", local = TRUE)
  source("SERVER/controls.R", local = TRUE)
  source("SERVER/video.R", local = TRUE)
  source("SERVER/background.R", local = TRUE)
  source("SERVER/mask.R", local = TRUE)
  source("SERVER/segmentation.R", local = TRUE)
  source("SERVER/identification.R", local = TRUE)
  source("SERVER/composite.R", local = TRUE)
  source("SERVER/bookmarking.R", local = TRUE)
  session$onSessionEnded(function() {})
}

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
