# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
theModelFolder <- shiny::reactiveVal()
theModel <- shiny::reactiveVal()

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
defaultRoot <- reactiveVal()
defaultPath <- reactiveVal("")
theVideoPath <- shiny::reactiveVal()

# Progress monitoring
theRawProgress <- shiny::reactiveVal()
theProgress <- shiny::reactiveVal()
monitorProgress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggleTabs(2, "OFF")

# Video
theVideo <- shiny::reactiveVal()
refreshVideo <- reactiveVal(0)
refreshFrame <- shiny::reactiveVal(0)
theFrame <- shiny::reactiveVal()
theImage <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
toDisplay <- NULL
refreshDisplay <- reactiveVal(0)
printDisplay <- reactiveVal(0)