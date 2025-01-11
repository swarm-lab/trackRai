# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
yolo_proc <- NULL
theModel <- shiny::reactiveVal()

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
theYOLOPath <- shiny::reactiveVal()
theTempFile <- NULL

# Progress monitoring
theRawProgress <- shiny::reactiveVal()
theProgress <- shiny::reactiveVal()
monitorProgress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggleTabs(2, "OFF")

# Check
theVideo <- shiny::reactiveVal()
refreshFrame <- shiny::reactiveVal(0)
theFrame <- NULL
theTempFrame <- NULL
theTempPredict <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
toDisplay <- NULL
refreshDisplay <- reactiveVal(0)
printDisplay <- reactiveVal(0)