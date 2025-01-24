# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
the_track_path <- shiny::reactiveVal()
the_export_path <- shiny::reactiveVal()

# UI
shinyjs::hideElement("curtain")

# Video
the_video <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
the_frame <- shiny::reactiveVal()
the_image <- NULL
tmp_vw <- cv2$VideoWriter(
  normalizePath(tempfile(fileext = ".mp4"), mustWork = FALSE),
  cv2$VideoWriter_fourcc("a", "v", "c", "1"),
  30L, c(1920L, 1080L)
)
if (reticulate::py_to_r(tmp_vw$isOpened())) {
  codec <- cv2$VideoWriter_fourcc("a", "v", "c", "1")
} else {
  codec <- cv2$VideoWriter_fourcc("m", "p", "4", "v")
}
tmp_vw$release()

# Display
black_screen <- reticulate::r_to_py(array(0L, c(1080, 1920, 3)))
to_display <- NULL
refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)
video_range <- c(0, 0)
display_table <- NULL
col <- pals::alphabet()

# Tracks
the_tracks <- NULL
track_names <- c(
  "frame", "id", "x", "y", "width", "height", "angle", 
  "x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4"
)
refresh_tracks <- shiny::reactiveVal(0)

# Exporting
the_loop <- shiny::reactiveVal()
the_debounce <- shiny::debounce(the_loop, 1)
in_progress <- shiny::reactiveVal(FALSE)
frame <- NULL
tracks <- NULL
pb <- NULL
n <- NULL
old_check <- NULL
old_frame <- NULL
old_time <- NULL



