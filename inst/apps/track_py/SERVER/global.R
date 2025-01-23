# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) "cuda:0" else if (mps) "mps" else "cpu"
theModelFolder <- shiny::reactiveVal()
theModel <- NULL

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
mask_path <- shiny::reactiveVal()
theTrackPath <- shiny::reactiveVal()

# Progress monitoring
theRawProgress <- shiny::reactiveVal()
theProgress <- shiny::reactiveVal()
monitorProgress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggled_tabs <- data.frame(
  tab = 1:2,
  toggled = c(TRUE, FALSE)
)

# Video
the_video <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
refreshFrame <- shiny::reactiveVal(0)
the_frame <- shiny::reactiveVal()
the_image <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
to_display <- NULL
refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)
refresh_mask <- shiny::reactiveVal(0)
the_mask <- NULL
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