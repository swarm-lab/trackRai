# Status
output$video_status <- shiny::renderUI({
  if (refresh_display() > -1 & !trackRai::is_video_capture(the_video)) {
    toggleTabs(2:6, "OFF")
    toggled_tabs$toggled[2:6] <<- FALSE
    p("Video missing (and required).", class = "bad")
  } else if (!trackRai::is_video_capture(the_video)) {
    toggleTabs(2:6, "OFF")
    toggled_tabs$toggled[2:6] <<- FALSE
    p("Incompatible videos.", class = "bad")
  } else {
    if (toggled_tabs$toggled[2] == FALSE) {
      toggleTabs(2, "ON")
      toggled_tabs$toggled[2] <<- TRUE
      NULL
    }
  }
})


# Display
shiny::observeEvent(input$main, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (trackRai::is_image(the_image)) {
      to_display <<- the_image$copy()
    } else {
      to_display <<- black_screen$copy()
    }

    js$uishape("display_img")
    print_display(print_display() + 1)
  }
})

output$display <- shiny::renderUI({
  if (print_display() > 0) {
    if (trackRai::is_image(to_display)) {
      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", to_display)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    } else {
      shiny::tags$img(
        src = paste0("data:image/jpg;base64,", reticulate::py_to_r(
          base64$b64encode(cv2$imencode(".jpg", black_screen)[1])$decode("utf-8")
        )),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    }
  }
})

session$onFlushed(function() {
  js$uishape("display_img")
})

shiny::observeEvent(input$win_resize, {
  js$uishape("display_img")
})


# Load video
shinyFiles::shinyFileChoose(input, "video_file_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$video_file_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$video_file_x)
  if (nrow(path) > 0) {
    video_path(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(video_path(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(video_path(), volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      }
    )
  )
  volume <- volumes[ix]

  if (length(volume) > 0) {
    dir <- dirname(video_path())
    default_root(names(volumes)[ix])
    # default_path(gsub(volume, "", dir))
    default_path(gsub(paste0(".*", volume), "", dir))
  }
})

shiny::observeEvent(video_path(), {
  to_check <- cv2$VideoCapture(video_path())

  if (reticulate::py_to_r(to_check$isOpened())) {
    if (!is.na(trackRai::n_frames(to_check))) {
      the_video <<- to_check
      the_image <<- the_video$read()[1]
      refresh_video(refresh_video() + 1)
      refresh_display(refresh_display() + 1)
    }
  }
})


# Read frame
shiny::observeEvent(input$video_controls, {
  if (trackRai::is_video_capture(the_video)) {
    the_frame(input$video_controls[2])
  }
})

shiny::observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    the_image <<- trackRai::read_frame(the_video, the_frame())
    refresh_display(refresh_display() + 1)
  }
})
