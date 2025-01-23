# UI
output$startStop <- shiny::renderUI({
  if (inProgress()) {
    shiny::actionButton(
      "stopTrack", "Stop tracking",
      width = "100%", class = "btn-danger"
    )
  } else {
    shinyFiles::shinySaveButton("startTrack_x", "Start tracking",
      "Please select a location to save the tracks", "tracks", "csv",
      class = "fullWidth btn-success"
    )
  }
})


# Events
shinyFiles::shinyFileSave(input, "startTrack_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$startTrack_x, {
  path <- shinyFiles::parseSavePath(volumes, input$startTrack_x)
  if (nrow(path) > 0) {
    theTrackPath(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(theTrackPath(), {
  the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls[1] - 1)
  theLoop(0)
  inProgress(TRUE)
  shiny::showNotification("Tracking in progress.", id = "tracking", duration = NULL)
  toggleTabs(1, "OFF")
  toggled_tabs$toggled[1] <<- FALSE
  toggleInputs(input, state = "OFF")
  sc <<- max(c(trackRai::n_row(the_image), trackRai::n_col(the_image)) / 720)
  font_scale <<- as.integer(sc)
  font_thickness <<- as.integer(max(1, 1.5 * sc))
  theModel <<- ultralytics$YOLO(normalizePath(paste0(theModelFolder(), "/runs/obb/", input$model_x)))

  pb <<- Progress$new()
  pb$set(message = "Computing: ", value = 0, detail = "0%")
  n <<- input$video_controls[3] - input$video_controls[1] + 1
  old_check <<- 0
  old_frame <<- 1
  old_time <<- Sys.time()

  con <- file(normalizePath(theTmpTracker, mustWork = FALSE), "w")
  write("tracker_type: botsort", con)
  write(paste0("track_high_thresh: ", input$assoc_x[2]), con, append = TRUE)
  write(paste0("track_low_thresh: ", input$assoc_x[1]), con, append = TRUE)
  write(paste0("new_track_thresh: ", input$newTrack_x), con, append = TRUE)
  write(paste0("track_buffer: ", input$trackBuffer_x), con, append = TRUE)
  write(paste0("match_thresh: ", input$match_x), con, append = TRUE)
  write("fuse_score: True", con, append = TRUE)
  write("gmc_method: sparseOptFlow", con, append = TRUE)
  write("proximity_thresh: 0.5", con, append = TRUE)
  write("appearance_thresh: 0.25", con, append = TRUE)
  write("with_reid: False", con, append = TRUE)

  close(con)
})


# Tracking loop

# The loop doesnt stop if the user-defined upper frame limit is reached

shiny::observeEvent(theDebounce(), {
  if (!is.null(theLoop())) {
    frame <<- the_video$read()

    if (reticulate::py_to_r(frame[0])) {
      tracks <<- theModel$track(
        source = cv2$multiply(frame[1], the_mask),
        show = FALSE,
        persist = TRUE,
        imgsz = c(trackRai::n_row(frame[1]), trackRai::n_col(frame[1])),
        conf = input$conf_x,
        iou = input$iou_x,
        max_det = as.integer(input$maxObjects_x),
        tracker = normalizePath(theTmpTracker, mustWork = FALSE),
        verbose = FALSE,
        device = device
      )

      if (!reticulate::py_to_r(tracks[0]$obb$id == py_none())) {
        obb <- asplit(reticulate::py_to_r(tracks[0]$obb$xyxyxyxy$cpu()$numpy()), 3)
        xywhr <- reticulate::py_to_r(tracks[0]$obb$xywhr$cpu()$numpy())
        ids <- reticulate::py_to_r(tracks[0]$obb$id$cpu()$numpy())

        tab <- data.table::as.data.table(
          cbind(
            input$video_controls[1] + theLoop(),
            ids,
            xywhr,
            obb[[1]],
            obb[[2]]
          )
        )
        names(tab) <- c(
          "frame", "id", "x", "y", "width", "height", "angle",
          "x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4"
        )

        if (theLoop() == 0) {
          if (file.exists(normalizePath(theTrackPath(), mustWork = FALSE))) {
            unlink(normalizePath(theTrackPath(), mustWork = FALSE))
          }
          fwrite(tab, normalizePath(theTrackPath(), mustWork = FALSE), append = FALSE)
        } else {
          fwrite(tab, normalizePath(theTrackPath(), mustWork = FALSE), append = TRUE)
        }

        displayTable <<- data.table::rbindlist(list(displayTable, tab))[frame >= (max(frame) - input$trackBuffer_x), ]
      }

      if (input$preview) {
        if ((theLoop() %% input$trackBuffer_x) == 0) {
          to_display <<- cv2$multiply(frame[1], the_mask)

          if (!is.null(displayTable)) {
            last <- displayTable[frame == max(frame)]
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
                to_display, list(box[i - 1]), 0L, c(255L, 255L, 255),
                as.integer(max(0.5, 4 * sc))
              )
              to_display <<- cv2$drawContours(
                to_display, list(box[i - 1]), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
                as.integer(max(0.5, 2 * sc))
              )
              trace <- reticulate::r_to_py(as.matrix(displayTable[id == last$id[i], c("x", "y")]))
              to_display <<- cv2$polylines(
                to_display, list(np$int_(trace)), 0L, as.integer(col2rgb(shades[i], FALSE)[3:1, , drop = FALSE]),
                as.integer(max(0.5, 2 * sc))
              )
            }
          }

          print_display(print_display() + 1)
        }
      }

      new_check <- floor(100 * theLoop() / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (theLoop() - old_frame + 1) /
          as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <<- new_check
        old_frame <<- theLoop()
        old_time <<- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps")
        )
      }

      theLoop(theLoop() + 1)
    } else {
      theLoop(NULL)
      theTrackPath(NULL)
      inProgress(FALSE)
      shiny::removeNotification(id = "tracking")
      pb$close()
      toggleTabs(1, "ON")
      toggled_tabs$toggled[1] <<- TRUE
      toggleInputs(input, state = "ON")
    }
  }
})

shiny::observeEvent(input$stopTrack, {
  theLoop(NULL)
  theTrackPath(NULL)
  inProgress(FALSE)
  shiny::removeNotification(id = "tracking")
  pb$close()
  toggleTabs(1, "ON")
  toggled_tabs$toggled[1] <<- TRUE
  toggleInputs(input, state = "ON")
})
