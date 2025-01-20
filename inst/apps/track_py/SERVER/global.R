# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) "cuda:0" else if (mps) "mps" else "cpu"
theModelFolder <- shiny::reactiveVal()
theModel <- NULL

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
defaultRoot <- shiny::reactiveVal()
defaultPath <- shiny::reactiveVal("")
theVideoPath <- shiny::reactiveVal()
theTrackPath <- shiny::reactiveVal()

# Progress monitoring
theRawProgress <- shiny::reactiveVal()
theProgress <- shiny::reactiveVal()
monitorProgress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggledTabs <- data.frame(
  tab = 1:2,
  toggled = c(TRUE, FALSE)
)

# Video
theVideo <- shiny::reactiveVal()
refreshVideo <- shiny::reactiveVal(0)
refreshFrame <- shiny::reactiveVal(0)
theFrame <- shiny::reactiveVal()
theImage <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
toDisplay <- NULL
refreshDisplay <- shiny::reactiveVal(0)
printDisplay <- shiny::reactiveVal(0)
displayTable <- NULL
col <- pals::alphabet()

# Tracking
theTmpTracker <- tempfile("tracker", fileext = ".yaml")
theLoop <- shiny::reactiveVal()
theDebounce <- shiny::debounce(theLoop, 1)
inProgress <- shiny::reactiveVal(FALSE)
frame <- NULL
tracks <- NULL
sc <- NULL
font_scale <- NULL
font_thickness <- NULL
lab <- NULL
xywhr <- NULL
pb <- NULL
n <- NULL
old_check <- NULL
old_frame <- NULL
old_time <- NULL