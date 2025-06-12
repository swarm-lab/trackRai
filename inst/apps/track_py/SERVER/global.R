# YOLO
yolo_installed <- yolo_installed()
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) "cuda:0" else if (mps) "mps" else "cpu"
the_model_folder <- shiny::reactiveVal()
the_model <- NULL

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
mask_path <- shiny::reactiveVal()
the_track_path <- shiny::reactiveVal()

# UI
shinyjs::hideElement("curtain")
toggled_tabs <- data.frame(
  tab = 1:2,
  toggled = c(TRUE, FALSE)
)

# Display
black_screen <- reticulate::r_to_py(array(0L, c(1080, 1920, 3)))
to_display <- NULL
refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)
video_range <- c(0, 0)
display_table <- NULL
col <- pals::alphabet()

# Video
the_video <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
the_frame <- shiny::reactiveVal()
the_image <- NULL

# Mask
refresh_mask <- shiny::reactiveVal(0)
the_mask <- NULL

# Tracking
the_temp_tracker <- tempfile("tracker", fileext = ".yaml")
the_loop <- shiny::reactiveVal()
the_debounce <- shiny::debounce(the_loop, 1)
in_progress <- shiny::reactiveVal(FALSE)
frame <- NULL
tracks <- NULL
sc <- NULL
pb <- NULL
n <- NULL
old_check <- NULL
old_frame <- NULL
old_time <- NULL