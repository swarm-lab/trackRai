# Globals and reactives
volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
video_range <- c(0, 0)
the_video <- NULL
the_image <- NULL

default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
the_frame <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)


# UI
output$video_status <- shiny::renderUI({
  if (refresh_display() > -1 & !trackRcv::is_video_capture(the_video)) {
    shiny::p("Video missing (and required).", class = "bad")
  } else if (!trackRcv::is_video_capture(the_video)) {
    shiny::p("Incompatible videos.", class = "bad")
  } else {
    NULL
  }
})

shiny::observeEvent(refresh_display(), {
  if (!trackRcv::is_video_capture(the_video)) {
    .toggleTabs(2, "OFF")
    toggled_tabs$toggled[2] <<- FALSE
  } else {
    if (toggled_tabs$toggled[2] == FALSE) {
      .toggleTabs(2, "ON")
      toggled_tabs$toggled[2] <<- TRUE
    }
  }
})


# Display
shiny::observeEvent(input$main, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (trackRcv::is_image(the_image)) {
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
    if (trackRcv::is_image(to_display)) {
      shiny::tags$img(
        src = paste0(
          "data:image/jpg;base64,",
          reticulate::py_to_r(
            base64$b64encode(cv2$imencode(".jpg", to_display)[1])$decode(
              "utf-8"
            )
          )
        ),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    } else {
      shiny::tags$img(
        src = paste0(
          "data:image/jpg;base64,",
          reticulate::py_to_r(
            base64$b64encode(cv2$imencode(".jpg", black_screen)[1])$decode(
              "utf-8"
            )
          )
        ),
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
shinyFiles::shinyFileChoose(
  input,
  "video_file",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$video_file, {
  path <- shinyFiles::parseFilePaths(volumes, input$video_file)
  if (nrow(path) > 0) {
    video_path(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(video_path(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(
        video_path(),
        stringr::fixed(sapply(volumes, normalizePath))
      ),
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
    default_path(gsub(paste0(".*", volume), "", dir))
  }
})

shiny::observeEvent(video_path(), {
  to_check <- cv2$VideoCapture(video_path())

  if (reticulate::py_to_r(to_check$isOpened())) {
    if (!is.na(trackRcv::n_frames(to_check))) {
      if (trackRcv::n_frames(to_check) > 1) {
        the_video <<- to_check
        the_image <<- the_video$read()[1]
        refresh_video(refresh_video() + 1)
        refresh_display(refresh_display() + 1)
      }
    }
  }
})


# Read frame
shiny::observeEvent(input$leftKey, {
  if (trackRcv::is_video_capture(the_video)) {
    val <- input$video_controls_x

    if (input$video_controls_x > 1) {
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = input$video_controls_x - 1
      )
    }
  }
})

shiny::observeEvent(input$rightKey, {
  if (trackRcv::is_video_capture(the_video)) {
    vals <- input$video_controls_x

    if (input$video_controls_x < trackRcv::n_frames(the_video)) {
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = input$video_controls_x + 1
      )
    }
  }
})

shiny::observeEvent(input$downKey, {
  if (trackRcv::is_video_capture(the_video)) {
    if (input$video_controls_x >= (1 + trackRcv::fps(the_video))) {
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = input$video_controls_x - trackRcv::fps(the_video)
      )
    }
  }
})

shiny::observeEvent(input$upKey, {
  if (trackRcv::is_video_capture(the_video)) {
    if (
      input$video_controls_x <=
        (trackRcv::n_frames(the_video) - trackRcv::fps(the_video))
    ) {
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = input$video_controls_x + trackRcv::fps(the_video)
      )
    }
  }
})

shiny::observeEvent(input$video_controls_x, {
  if (trackRcv::is_video_capture(the_video)) {
    the_frame(input$video_controls_x)
  }
})

shiny::observeEvent(input$main, {
  if (trackRcv::is_video_capture(the_video)) {
    if (input$main %in% c("1", "2")) {
      the_frame(input$video_controls_x)
    }
  }
})

shiny::observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    the_image <<- trackRcv::read_frame(the_video, the_frame())
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$tagged_frame, {
  if ((nchar(input$tagged_frame) > 0) & !is.null(input$video_controls_x)) {
    tagged_frame <- as.numeric(input$tagged_frame)

    if (tagged_frame != input$video_controls_x) {
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = tagged_frame
      )
    }
  }
})

shiny::observeEvent(input$next_tagged_frame, {
  if (!is.null(obb)) {
    choices <- sort(unique(obb$frame))
    selected <- which(choices == input$tagged_frame) + 1
    shiny::updateSelectInput(
      session,
      "tagged_frame",
      selected = choices[
        if (selected > length(choices)) {
          selected <- 1
        } else {
          selected
        }
      ]
    )
  }
})

shiny::observeEvent(input$previous_tagged_frame, {
  if (!is.null(obb)) {
    choices <- sort(unique(obb$frame))
    selected <- which(choices == input$tagged_frame) - 1
    shiny::updateSelectInput(
      session,
      "tagged_frame",
      selected = choices[
        if (selected < 1) {
          selected <- length(choices)
        } else {
          selected
        }
      ]
    )
  }
})
