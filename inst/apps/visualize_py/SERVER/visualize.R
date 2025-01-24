# Display
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
        display_table <- the_tracks[frame <= the_frame() & frame > (the_frame() - input$track_length_x)]
        last <- display_table[frame == max(frame)]
        box <- reticulate::r_to_py(
          simplify2array(
            list(
              as.matrix(last[, c("x1", "x2", "x3", "x4")]),
              as.matrix(last[, c("y1", "y2", "y3", "y4")])
            )
          )
        )
        box <- np$int_(box)
        shades <- col[(last$id %% length(col)) + 1]

        for (i in seq_len(py_to_r(box$shape[0]))) {
          to_display <<- cv2$drawContours(
            to_display, list(box[i - 1]), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
            as.integer(input$line_width_x)
          )
          trace <- reticulate::r_to_py(as.matrix(display_table[id == last$id[i], c("x", "y")]))
          to_display <<- cv2$polylines(
            to_display, list(np$int_(trace)), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
            as.integer(input$line_width_x)
          )
        }
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
    default_path(gsub(volume, "", dir))
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
    to_check <- data.table::fread(the_track_path())

    if (all(names(to_check) == track_names)) {
      the_tracks <<- to_check
      ix <- which.max(
        sapply(
          stringr::str_locate_all(the_track_path(), volumes),
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
      default_path(gsub(volume, "", dir))

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
      frame <<- the_video$read()[1]

      track_table <- the_tracks[frame <= (input$video_controls[1] + the_loop()) &
        frame > (input$video_controls[1] + the_loop() - input$track_length_x)]
      last <- track_table[frame == max(frame)]
      box <- reticulate::r_to_py(
        simplify2array(
          list(
            as.matrix(last[, c("x1", "x2", "x3", "x4")]),
            as.matrix(last[, c("y1", "y2", "y3", "y4")])
          )
        )
      )
      box <- np$int_(box)
      shades <- col[(last$id %% length(col)) + 1]

      for (i in seq_len(py_to_r(box$shape[0]))) {
        frame <<- cv2$drawContours(
          frame, list(box[i - 1]), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
          as.integer(input$line_width_x)
        )
        trace <- reticulate::r_to_py(as.matrix(track_table[id == last$id[i], c("x", "y")]))
        frame <<- cv2$polylines(
          frame, list(np$int_(trace)), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
          as.integer(input$line_width_x)
        )
      }

      the_video_writer$write(frame)

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
