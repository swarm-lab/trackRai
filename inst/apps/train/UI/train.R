verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Training module", class = "module-title"),
  hr(),
  htmlOutput("yoloStatus"),
  shinyDirButton("dataset_x", "Select training dataset",
    "Please select the folder containing the training dataset",
    class = "fullWidth"
  ),
  hr(),
  selectInput("yolo_x", "Select YOLO model size:",
    c("nano" = "n", "small" = "s", "medium" = "m", "large" = "l"),
    "m", 
    width = "100%"
  ),
  hr(),
  numericInput("epochs_x", "Set the number of training epochs", 100, 1, NA, 1, "100%"),
  hr(),
  htmlOutput("nvidiaStatus"),
  uiOutput("startStop"),
  hr()
)
