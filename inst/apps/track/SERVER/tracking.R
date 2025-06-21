# Globals and reactives
display_table <- NULL
the_temp_tracker <- tempfile("tracker", fileext = ".yaml")
frame <- NULL
tracks <- NULL
sc <- NULL
pb <- NULL
n <- NULL
old_check <- NULL
old_frame <- NULL
old_time <- NULL

the_track_path <- shiny::reactiveVal()
the_loop <- shiny::reactiveVal()
the_debounce <- shiny::debounce(the_loop, 1)
in_progress <- shiny::reactiveVal(FALSE)


# UI
output$start_stop <- shiny::renderUI({
  if (in_progress()) {
    shiny::actionButton(
      "stop_track",
      "Stop tracking",
      width = "100%",
      class = "btn-danger"
    )
  } else {
    shinyFiles::shinySaveButton(
      "start_track_x",
      "Start tracking",
      "Please select a location to save the tracks",
      "tracks",
      "csv",
      class = "fullWidth btn-success"
    )
  }
})


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "3") {
    to_display <<- cv2$multiply(
      the_image,
      cv2$divide(cv2$compare(the_mask, 0, 1L), 255L)
    )

    pred <- the_model(
      source = to_display,
      imgsz = c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)),
      conf = input$conf_x,
      iou = input$iou_x,
      max_det = as.integer(input$max_objects_x),
      show = FALSE,
      verbose = FALSE,
      device = device
    )
    obb <- pred[0]$obb$xyxyxyxy$cpu()$numpy()
    obb <- np$int_(obb)
    sc <- max(c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)) / 720)

    for (i in seq_len(py_to_r(obb$shape[0]))) {
      .drawContour(
        to_display,
        list(obb[i - 1]),
        color = .shades[, 6],
        contrast = c(255, 255, 255),
        thickness = as.integer(max(1, round(sc)))
      )
    }

    print_display(print_display() + 1)
  }
})

shiny::observeEvent(input$conf_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$iou_x, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$max_objects_x, {
  refresh_display(refresh_display() + 1)
})


# Initiate tracking
shinyFiles::shinyFileSave(
  input,
  "start_track_x",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$start_track_x, {
  path <- shinyFiles::parseSavePath(volumes, input$start_track_x)
  if (nrow(path) > 0) {
    the_track_path(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(the_track_path(), {
  the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls_x[1] - 1)
  the_loop(0)
  in_progress(TRUE)
  shiny::showNotification(
    "Tracking in progress.",
    id = "tracking",
    duration = NULL
  )
  .toggleTabs(1:2, "OFF")
  toggled_tabs$toggled[1:2] <<- FALSE
  .toggleInputs(input, state = "OFF")
  sc <<- round(max(c(
    1,
    trackRcv::n_row(the_image) / 720,
    trackRcv::n_col(the_image) / 720
  )))
  the_model <<- ultralytics$YOLO(normalizePath(paste0(
    the_model_folder(),
    "/runs/obb/",
    input$model_x
  )))

  pb <<- Progress$new()
  pb$set(message = "Computing: ", value = 0, detail = "0%")
  n <<- input$video_controls_x[3] - input$video_controls_x[1] + 1
  old_check <<- 0
  old_frame <<- 1
  old_time <<- Sys.time()

  con <- file(normalizePath(the_temp_tracker, mustWork = FALSE), "w")
  write("tracker_type: botsort", con)
  write(paste0("track_high_thresh: ", input$assoc_x[2]), con, append = TRUE)
  write(paste0("track_low_thresh: ", input$assoc_x[1]), con, append = TRUE)
  write(paste0("new_track_thresh: ", input$new_track_x), con, append = TRUE)
  write(paste0("track_buffer: ", input$track_buffer), con, append = TRUE)
  write(paste0("match_thresh: ", input$match_x), con, append = TRUE)
  write("fuse_score: True", con, append = TRUE)
  write("gmc_method: sparseOptFlow", con, append = TRUE)
  write("proximity_thresh: 0.5", con, append = TRUE)
  write("appearance_thresh: 0.25", con, append = TRUE)
  write("with_reid: False", con, append = TRUE)

  close(con)
})


# Tracking loop
shiny::observeEvent(the_debounce(), {
  if (!is.null(the_loop())) {
    if (the_loop() < n) {
      frame <<- the_video$read()

      if (reticulate::py_to_r(frame[0])) {
        tracks <<- the_model$track(
          source = cv2$multiply(frame[1], the_mask),
          show = FALSE,
          persist = TRUE,
          imgsz = c(trackRcv::n_row(frame[1]), trackRcv::n_col(frame[1])),
          conf = input$conf_x,
          iou = input$iou_x,
          max_det = as.integer(input$max_objects_x),
          tracker = normalizePath(the_temp_tracker, mustWork = FALSE),
          verbose = FALSE,
          device = device
        )

        if (!reticulate::py_to_r(tracks[0]$obb$id == py_none())) {
          obb <- asplit(
            reticulate::py_to_r(tracks[0]$obb$xyxyxyxy$cpu()$numpy()),
            3
          )
          xywhr <- reticulate::py_to_r(tracks[0]$obb$xywhr$cpu()$numpy())
          ids <- reticulate::py_to_r(tracks[0]$obb$id$cpu()$numpy())

          # tab <- data.table::as.data.table(
          #   cbind(
          #     input$video_controls_x[1] + the_loop(),
          #     ids,
          #     xywhr,
          #     obb[[1]],
          #     obb[[2]]
          #   )
          # )
          # names(tab) <- c(
          #   "frame",
          #   "track",
          #   "x",
          #   "y",
          #   "width",
          #   "height",
          #   "angle",
          #   "x1",
          #   "x2",
          #   "x3",
          #   "x4",
          #   "y1",
          #   "y2",
          #   "y3",
          #   "y4"
          # )
          to_write <- data.table::as.data.table(
            cbind(
              input$video_controls_x[1] + the_loop(),
              ids,
              xywhr
            )
          )
          names(to_write) <- c(
            "frame",
            "track",
            "x",
            "y",
            "width",
            "height",
            "angle"
          )
          to_write[, angle := angle * 180 / pi]

          if (!is.null(scale_px()) & !is.null(scale_real())) {
            if (!is.na(scale_real())) {
              to_write[,
                paste0(c("x", "y", "width", "height"), "_", unit_real()) := .(
                  (x - origin()[1]) * scale_real() / scale_px(),
                  (y - origin()[2]) * scale_real() / scale_px(),
                  width * scale_real() / scale_px(),
                  height * scale_real() / scale_px()
                )
              ]
            }
          }

          if (the_loop() == 0) {
            if (
              file.exists(normalizePath(the_track_path(), mustWork = FALSE))
            ) {
              unlink(normalizePath(the_track_path(), mustWork = FALSE))
            }
            fwrite(
              to_write,
              normalizePath(the_track_path(), mustWork = FALSE),
              append = FALSE
            )
          } else {
            fwrite(
              to_write,
              normalizePath(the_track_path(), mustWork = FALSE),
              append = TRUE
            )
          }

          display_table <<- data.table::rbindlist(list(
            display_table,
            to_write
          ))[
            frame >= (max(frame) - input$track_buffer),
          ]
        }

        if (input$preview) {
          if ((the_loop() %% input$track_buffer) == 0) {
            to_display <<- cv2$multiply(frame[1], the_mask)

            if (!is.null(display_table)) {
              last <- display_table[frame == max(frame)]
              void <- last[,
                .drawBox(
                  to_display,
                  .SD$x,
                  .SD$y,
                  .SD$width,
                  .SD$height,
                  .SD$angle,
                  .shades[, (.BY$track %% ncol(.shades)) + 1],
                  c(255, 255, 255),
                  as.integer(max(1, round(sc)))
                ),
                by = .(track)
              ]

              void <- display_table[,
                .drawPolyLine(
                  to_display,
                  cbind(.SD$x, .SD$y),
                  closed = FALSE,
                  color = .shades[, (.BY$track[1] %% ncol(.shades)) + 1],
                  contrast = c(255, 255, 255),
                  thickness = as.integer(max(1, round(sc)))
                ),
                by = .(track)
              ]
            }

            print_display(print_display() + 1)
          }
        }

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
      }
    } else {
      the_loop(NULL)
      the_track_path(NULL)
      in_progress(FALSE)
      the_model <<- NULL
      display_table <<- NULL
      shiny::removeNotification(id = "tracking")
      pb$close()
      .toggleTabs(1:2, "ON")
      toggled_tabs$toggled[1:2] <<- TRUE
      .toggleInputs(input, state = "ON")
    }
  }
})

shiny::observeEvent(input$stop_track, {
  the_loop(NULL)
  the_track_path(NULL)
  in_progress(FALSE)
  the_model <<- NULL
  display_table <<- NULL
  shiny::removeNotification(id = "tracking")
  pb$close()
  .toggleTabs(1:2, "ON")
  toggled_tabs$toggled[1:2] <<- TRUE
  .toggleInputs(input, state = "ON")
})
