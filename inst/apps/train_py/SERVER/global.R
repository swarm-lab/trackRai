# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
yolo_proc <- NULL
the_model_folder <- shiny::reactiveVal()
the_model <- shiny::reactiveVal()

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
yolo_path <- shiny::reactiveVal()
the_temp_file <- NULL

# Progress monitoring
the_raw_progress <- shiny::reactiveVal()
the_progress <- shiny::reactiveVal()
monitor_progress <- shiny::reactiveVal(FALSE)

# UI
shinyjs::hideElement("curtain")
toggleTabs(2, "OFF")
toggled_tabs <- data.frame(
  tab = 1:2,
  toggled = c(TRUE, FALSE)
)

# Check
the_video <- shiny::reactiveVal()
refresh_frame <- shiny::reactiveVal(0)
the_frame <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
