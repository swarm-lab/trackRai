# Display
.shades <- col2rgb(pals::alphabet())[3:1, ]
mode(.shades) <- "integer"

.drawBoxes <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    box <- array(c(SD$x1, SD$x2, SD$x3, SD$x4, SD$y1, SD$y2, SD$y3, SD$y4), dim = c(nrow(SD), 4, 2))
    mode(box) <- "integer"
    cv2$drawContours(
      img, list(reticulate::r_to_py(box)), 0L,
      .shades[, (BY$id %% ncol(.shades)) + 1],
      as.integer(linewidth)
    )
  }
  NULL
}

.drawTracks <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    trace <- matrix(c(SD$x, SD$y), ncol = 2)
    mode(trace) <- "integer"
    cv2$polylines(
      img, list(reticulate::r_to_py(trace)), 0L,
      .shades[, (BY$id %% ncol(.shades)) + 1],
      as.integer(linewidth)
    )
  }
  NULL
}

shiny::observeEvent(input$video_controls, {
  if (trackRai::is_video_capture(the_video)) {
    the_frame(input$video_controls[2])
  }
})

shiny::observeEvent(input$line_width_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$track_length_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    the_image <<- trackRai::read_frame(the_video, the_frame())
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (trackRai::is_image(the_image)) {
      to_display <<- the_image$copy()

      if (data.table::is.data.table(the_tracks)) {
        void <- the_tracks[frame == the_frame(),
          .drawBoxes(to_display, .SD, .BY, input$line_width_x),
          by = .(id), .SDcols = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")
        ]
        void <- the_tracks[frame %in% (the_frame() - input$track_length_x + 1):the_frame(),
          .drawTracks(to_display, .SD, .BY, input$line_width_x),
          by = .(id), .SDcols = c("x", "y")
        ]
      }
    } else {
      to_display <<- black_screen$copy()
    }

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
}, once = TRUE)

shiny::observeEvent(input$win_resize, {
  js$uishape("display_img")
})


# Status
output$video_status <- shiny::renderUI({
  if (refresh_display() > -1 & !trackRai::is_video_capture(the_video)) {
    shiny::p("Video missing (and required).", class = "bad")
  } else if (!trackRai::is_video_capture(the_video)) {
    shiny::p("Incompatible videos.", class = "bad")
  }
})

output$track_status <- shiny::renderUI({
  if (refresh_display() > -1 & !data.table::is.data.table(the_tracks)) {
    shiny::p("Tracks missing (and required).", class = "bad")
  } else if (!(all(names(the_tracks) == track_names))) {
    shiny::p("Incompatible tracks.", class = "bad")
  }
})


# UI
output$export_controls <- shiny::renderUI({
  if (refresh_display() > -1 & trackRai::is_video_capture(the_video) &
    data.table::is.data.table(the_tracks)) {
    shiny::tagList(
      shinyFiles::shinySaveButton(
        "export_x", "Export video with tracks", "Save video as...",
        filename = "video_tracks.mp4",
        filetype = list(video = c("mp4")),
        class = "fullWidth"
      ),
      shiny::hr()
    )
  }
})


# Load video file
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
      stringr::str_locate_all(video_path(), fixed(sapply(volumes, normalizePath))),
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


# Load track file
shinyFiles::shinyFileChoose(input, "track_file_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$track_file_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$track_file_x)
  if (nrow(path) > 0) {
    the_track_path(normalizePath(path$datapath, mustWork = FALSE))
    refresh_tracks(refresh_tracks() + 1)
  }
})

shiny::observeEvent(refresh_tracks(), {
  if (refresh_tracks() > 0) {
    shinyjs::show("curtain")
    shiny::showNotification("Loading data. Please wait.", id = "loading", duration = NULL)
    to_check <- data.table::fread(the_track_path())
    shiny::removeNotification("loading")
    shinyjs::hide("curtain")

    if (all(names(to_check) == track_names)) {
      the_tracks <<- to_check
      ix <- which.max(
        sapply(
          stringr::str_locate_all(the_track_path(), fixed(sapply(volumes, normalizePath))),
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
      dir <- dirname(the_track_path())
      default_root(names(volumes)[ix])
      # default_path(gsub(volume, "", dir))
      default_path(gsub(paste0(".*", volume), "", dir))

      refresh_display(refresh_display() + 1)
    }
  }
})

shinyFiles::shinyFileSave(input, "export_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$export_x, {
  path <- shinyFiles::parseSavePath(volumes, input$export_x)

  if (nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    the_export_path(path)
  }
})


# Export video with track
shiny::observeEvent(the_export_path(), {
  shiny::showNotification("Export in progress.", id = "export", duration = NULL)
  toggleInputs(input, state = "OFF")

  the_video_writer <<- cv2$VideoWriter(
    normalizePath(the_export_path(), mustWork = FALSE),
    codec,
    the_video$get(cv2$CAP_PROP_FPS),
    as.integer(c(trackRai::n_col(the_image), trackRai::n_row(the_image)))
  )
  the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls[1] - 1)
  pb <<- Progress$new()
  pb$set(message = "Computing: ", value = 0, detail = "0%")
  n <<- input$video_controls[3] - input$video_controls[1] + 1
  old_check <<- 0
  old_frame <<- 1
  old_time <<- Sys.time()
  the_loop(0)
  in_progress(TRUE)
})

shiny::observeEvent(the_debounce(), {
  if (!is.null(the_loop())) {
    if (the_loop() < n) {
      out <<- the_video$read()[1]

      void <- the_tracks[
        frame == (input$video_controls[1] + the_loop()),
        .drawBoxes(out, .SD, .BY, input$line_width_x),
        by = .(id), .SDcols = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")
      ]
      void <- the_tracks[
        frame %in% ((input$video_controls[1] + the_loop()) - input$track_length_x + 1):(input$video_controls[1] + the_loop()),
        .drawTracks(out, .SD, .BY, input$line_width_x),
        by = .(id), .SDcols = c("x", "y")
      ]

      the_video_writer$write(out)

      new_check <- floor(100 * the_loop() / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (the_loop() - old_frame + 1) /
          as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <<- new_check
        old_frame <<- the_loop()
        old_time <<- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps")
        )
      }

      the_loop(the_loop() + 1)
    } else {
      the_video_writer$release()
      the_loop(NULL)
      the_export_path(NULL)
      in_progress(FALSE)
      shiny::removeNotification(id = "export")
      pb$close()
      toggleInputs(input, state = "ON")
    }
  }
})
