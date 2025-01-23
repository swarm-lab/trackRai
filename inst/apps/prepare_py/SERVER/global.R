# File manager 
volumes <- c(Home = fs::path_home(), getVolumes()())
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
background_path <- shiny::reactiveVal()
mask_path <- shiny::reactiveVal()
yolo_path <- shiny::reactiveVal()

# UI
shinyjs::hideElement("curtain")
toggled_tabs <- data.frame(
  tab = 1:6,
  toggled = c(TRUE, rep(FALSE, 5))
)

# Display
black_screen <- reticulate::r_to_py(array(0L, c(1080, 1920, 3)))
to_display <- NULL
refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)
video_range <- c(0, 0)

# Video 
the_video <- NULL
the_frame <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
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

# Image
the_image <- NULL

# Background
the_background <- NULL
refresh_background <- shiny::reactiveVal(0)
collect_ghost <- shiny::reactiveVal(0)
stop_ghost_collection <- shiny::reactiveVal(0)
ghost_coords <- NULL

# Mask
the_mask <- NULL
refresh_mask <- shiny::reactiveVal(0)
collect_mask <- shiny::reactiveVal(0)
stop_mask_collection <- shiny::reactiveVal(0)
mask_coords <- NULL

# Objects
the_stats <- shiny::reactiveVal()
refresh_stats <- shiny::reactiveVal(0)
the_subs <- list()
the_submasks <- list()
the_composite <- NULL
