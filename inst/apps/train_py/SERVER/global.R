# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
yolo_proc <- NULL
theModelFolder <- shiny::reactiveVal()
theModel <- shiny::reactiveVal()


# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
yolo_path <- shiny::reactiveVal()
theTempFile <- NULL

# Progress monitoring
theRawProgress <- shiny::reactiveVal()
theProgress <- shiny::reactiveVal()
monitorProgress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggleTabs(2, "OFF")

# Check
the_video <- shiny::reactiveVal()
refreshFrame <- shiny::reactiveVal(0)
the_frame <- NULL
theTempFrame <- NULL
theTempPredict <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
to_display <- NULL
refresh_display <- reactiveVal(0)
print_display <- reactiveVal(0)