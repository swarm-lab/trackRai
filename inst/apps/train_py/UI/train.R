shinyWidgets::verticalTabPanel(
  title = "1",
  box_height = "100%",
  shiny::p("Training module", class = "module-title"),
  shiny::hr(),
  shiny::htmlOutput("yoloStatus"),
  shinyFiles::shinyDirButton("dataset_x", "Select training dataset",
    "Please select the folder containing the training dataset",
    class = "fullWidth"
  ),
  shiny::hr(),
  shiny::selectInput("yolo_x", "Select YOLO model size:",
    c("nano" = "n", "small" = "s", "medium" = "m", "large" = "l"),
    "m", 
    width = "100%"
  ),
  shiny::hr(),
  shiny::numericInput("epochs_x", "Set the number of training epochs", 100, 1, NA, 1, "100%"),
  shiny::hr(),
  shiny::htmlOutput("nvidiaStatus"),
  shiny::uiOutput("startStop"),
  shiny::hr()
)
