# YOLO
yolo_installed <- !is.na(trackRai:::.yolo_path())
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) "cuda:0" else if (mps) "mps" else "cpu"
yolo_proc <- NULL
the_model_folder <- shiny::reactiveVal()
the_model <- shiny::reactiveVal()
retrain <- shiny::reactiveVal(TRUE)

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
yolo_path <- shiny::reactiveVal()
video_path <- shiny::reactiveVal()
mask_path <- shiny::reactiveVal()
the_temp_file <- NULL

# Progress monitoring
the_raw_progress <- shiny::reactiveVal()
the_progress <- shiny::reactiveVal()
monitor_progress <- shiny::reactiveVal(FALSE)
monitor_tick <- shiny::reactiveVal(0)

# UI
shinyjs::hideElement("curtain")
toggleTabs(2, "OFF")
toggled_tabs <- data.frame(
  tab = 1:2,
  toggled = c(TRUE, FALSE)
)

# Video
the_video <- NULL
refresh_video <- shiny::reactiveVal(0)
refresh_frame <- shiny::reactiveVal(0)
the_frame <- NULL

# Mask
refresh_mask <- shiny::reactiveVal(0)
the_mask <- NULL

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
refresh_display <- shiny::reactiveVal(0)
to_display <- NULL
