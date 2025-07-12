# Globals and reactives
the_composite <- NULL

yolo_path <- shiny::reactiveVal()


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "6") {
    if (!trackRcv::is_image(the_composite)) {
      to_display <<- the_background
    } else {
      to_display <<- the_composite
    }

    print_display(print_display() + 1)
  }
})


# Create test composite
shiny::observeEvent(input$test_composite, {
  if (!is.null(the_stats())) {
    shinyjs::showElement("curtain")
    shiny::showNotification(
      "Computing composite image.",
      id = "composite",
      duration = NULL
    )

    dt <- the_stats()
    dt[, select := (select_h & select_w & mod != 2) | (mod == 1)]

    mask <- cv2$cvtColor(the_mask, cv2$COLOR_BGR2GRAY)
    tmp <- cv2$copyMakeBorder(
      mask,
      1L,
      1L,
      1L,
      1L,
      cv2$BORDER_CONSTANT,
      NULL,
      0L
    )
    k <- cv2$getStructuringElement(
      cv2$MORPH_RECT,
      as.integer(c(
        2 * input$mask_buffer_x,
        2 * input$mask_buffer_x
      )) +
        1L
    )
    tmp <- cv2$erode(tmp, k)
    mask <- tmp[1:(1 + trackRcv::n_row(mask)), 1:(1 + trackRcv::n_col(mask))]
    roi <- reticulate::py_to_r(mask)
    roi <- roi[nrow(roi):1, ]

    the_composite <<- the_background$copy()

    stamp <- reticulate::np_array(
      array(
        0L,
        c(trackRcv::n_row(the_composite), trackRcv::n_col(the_composite), 3)
      ),
      dtype = "uint8"
    )

    rnd_loc <- .sampleMinDist(
      roi,
      input$n_instances_x,
      round(mean(dt$width[dt$select]) / 2)
    )
    rnd_blob <- sample(which(dt$select), nrow(rnd_loc), TRUE)
    rnd_rot <- sample(c(0L, 1L, 2L, -1L), nrow(rnd_loc), TRUE)

    pb <- shiny::Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- nrow(rnd_loc)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in seq_len(n)) {
      if (rnd_rot[i] == -1L) {
        sub <- the_subs[[rnd_blob[i]]]
        submask <- the_submasks[[rnd_blob[i]]]
      } else {
        sub <- cv2$rotate(the_subs[[rnd_blob[i]]], rnd_rot[i])
        submask <- cv2$rotate(the_submasks[[rnd_blob[i]]], rnd_rot[i])
      }

      bottom <- rnd_loc[i, 1] - round(trackRcv::n_row(sub) / 2)
      left <- rnd_loc[i, 2] - round(trackRcv::n_col(sub) / 2)
      top <- trackRcv::n_row(stamp) - bottom - trackRcv::n_row(sub)
      right <- trackRcv::n_col(stamp) - left - trackRcv::n_col(sub)

      if (bottom < 0) {
        top <- top + bottom
        bottom <- 0
      }

      if (top < 0) {
        bottom <- bottom + top
        top <- 0
      }

      if (left < 0) {
        right <- right + left
        left <- 0
      }

      if (right < 0) {
        left <- left + right
        right <- 0
      }

      stamp <- cv2$copyMakeBorder(
        sub,
        as.integer(top),
        as.integer(bottom),
        as.integer(left),
        as.integer(right),
        cv2$BORDER_CONSTANT,
        NULL,
        0L
      )
      if (input$dark_button_x == "Darker") {
        the_composite <<- cv2$subtract(the_composite, stamp)
      } else {
        the_composite <<- cv2$add(the_composite, stamp)
      }

      nz <- cv2$findNonZero(cv2$cvtColor(submask, cv2$COLOR_BGR2GRAY))
      ell <- reticulate::py_to_r(cv2$fitEllipse(nz))
      sc <- max(
        c(trackRcv::n_row(the_composite), trackRcv::n_col(the_composite)) / 720
      )
      .drawBox(
        the_composite,
        ell[[1]][[1]] + left,
        ell[[1]][[2]] + top,
        ell[[2]][[1]],
        ell[[2]][[2]],
        ell[[3]],
        color = c(0L, 224L, 0L),
        contrast = c(255L, 255L, 255),
        thickness = as.integer(max(1, round(sc)))
      )

      new_check <- floor(100 * i / n)
      if (new_check > (old_check + 5)) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) /
          as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(
            new_check,
            "% - ",
            round(fps, digits = 2),
            "fps"
          )
        )
      }
    }

    the_composite <<- cv2$multiply(
      the_composite,
      runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x)
    )
    the_composite <<- cv2$add(
      the_composite,
      runif(1, -input$bias_x, input$bias_x)
    )

    if (input$saltpepper_x > 0) {
      r <- sample(0:input$saltpepper_x, 1)
      sp <- reticulate::np_array(
        array(
          sample(
            -r:r,
            trackRcv::n_row(the_composite) * trackRcv::n_col(the_composite) * 3,
            replace = TRUE
          ),
          c(trackRcv::n_row(the_composite), trackRcv::n_col(the_composite), 3)
        ),
        dtype = "int_"
      )
      the_composite <<- cv2$add(
        the_composite,
        sp,
        dtype = the_composite$dtype$type()
      )
    }

    refresh_display(refresh_display() + 1)

    pb$close()

    shiny::removeNotification(id = "composite")
    shinyjs::hideElement("curtain")
  }
})


# Create YOLO dataset
shinyFiles::shinyDirChoose(
  input,
  "generate_dataset",
  roots = volumes,
  session = session
)

shiny::observeEvent(input$generate_dataset, {
  path <- shinyFiles::parseDirPath(volumes, input$generate_dataset)
  if (length(path) > 0) {
    yolo_path(path)
  }
})

shiny::observeEvent(yolo_path(), {
  if (!is.null(yolo_path())) {
    if (dir.exists(paste0(yolo_path(), "/YOLO"))) {
      shiny::showNotification(
        "A folder named YOLO already exists at this location. Choose another one.",
        id = "yolo",
        type = "error"
      )
      shiny::isolate(yolo_path(NULL))
    } else {
      shinyjs::showElement("curtain")
      shiny::showNotification(
        "Generating YOLO dataset.",
        id = "yolo",
        duration = NULL
      )

      # Background and mask
      shiny::showNotification(
        "Creating background and mask.",
        id = "yolo_step_1",
        duration = NULL
      )

      dir.create(paste0(yolo_path(), "/YOLO"))

      nz <- cv2$findNonZero(cv2$cvtColor(the_mask, cv2$COLOR_BGR2GRAY))
      roi <- cbind(reticulate::py_to_r(nz[,, 0]), reticulate::py_to_r(nz[,, 1]))
      x <- min(roi[, 1])
      y <- min(roi[, 2])
      w <- diff(range(roi[, 1])) + 1
      h <- diff(range(roi[, 2])) + 1

      sub <- the_background[y:(y + h), x:(x + w)]

      top <- ceiling((ceiling(h / 32) * 32 - h) / 2)
      bottom <- floor((ceiling(h / 32) * 32 - h) / 2)
      left <- ceiling((ceiling(w / 32) * 32 - w) / 2)
      right <- floor((ceiling(w / 32) * 32 - w) / 2)

      prepped <- cv2$copyMakeBorder(
        sub,
        as.integer(top),
        as.integer(bottom),
        as.integer(left),
        as.integer(right),
        cv2$BORDER_CONSTANT,
        NULL,
        0L
      )
      prepped_background <- prepped$copy()
      cv2$imwrite(
        normalizePath(
          paste0(yolo_path(), "/YOLO/background.png"),
          mustWork = FALSE
        ),
        prepped_background
      )

      sub <- the_mask[y:(y + h), x:(x + w)]
      prepped <- cv2$copyMakeBorder(
        sub,
        as.integer(top),
        as.integer(bottom),
        as.integer(left),
        as.integer(right),
        cv2$BORDER_CONSTANT,
        NULL,
        0L
      )
      cv2$imwrite(
        normalizePath(paste0(yolo_path(), "/YOLO/mask.png"), mustWork = FALSE),
        prepped
      )

      prepped_mask <- cv2$divide(cv2$compare(prepped, 0, 1L), 255L)

      shiny::removeNotification(id = "yolo_step_1")

      # Tracking video
      if (input$export_video_x) {
        shiny::showNotification(
          "Creating video for tracking.",
          id = "yolo_step_2",
          duration = NULL
        )

        the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls_x[1] - 1)

        vw <- cv2$VideoWriter(
          normalizePath(
            paste0(yolo_path(), "/YOLO/video.mp4"),
            mustWork = FALSE
          ),
          trackRcv::fourcc("avc1"),
          trackRcv::fps(the_video),
          as.integer(c(
            trackRcv::n_col(prepped),
            trackRcv::n_row(prepped)
          ))
        )

        if (!reticulate::py_to_r(vw$isOpened())) {
          vw <- cv2$VideoWriter(
            normalizePath(
              paste0(yolo_path(), "/YOLO/video.mp4"),
              mustWork = FALSE
            ),
            trackRcv::fourcc("mp4v"),
            trackRcv::fps(the_video),
            as.integer(c(
              trackRcv::n_col(prepped),
              trackRcv::n_row(prepped)
            ))
          )
        }

        pb <- shiny::Progress$new()
        pb$set(message = "Computing: ", value = 0, detail = "0%")
        n <- input$video_controls_x[3] - input$video_controls_x[1] + 1
        old_check <- 0
        old_frame <- 1
        old_time <- Sys.time()

        for (i in seq_len(n)) {
          frame <- the_video$read()[1]
          sub <- frame[y:(y + h), x:(x + w)]
          prepped <- cv2$copyMakeBorder(
            sub,
            as.integer(top),
            as.integer(bottom),
            as.integer(left),
            as.integer(right),
            cv2$BORDER_CONSTANT,
            NULL,
            0L
          )
          vw$write(prepped)

          new_check <- floor(100 * i / n)
          if (new_check > (old_check + 5)) {
            new_time <- Sys.time()
            fps <- (i - old_frame + 1) /
              as.numeric(difftime(new_time, old_time, units = "secs"))
            old_check <- new_check
            old_frame <- i
            old_time <- new_time
            pb$set(
              value = new_check / 100,
              detail = paste0(
                new_check,
                "% - ",
                round(fps, digits = 2),
                "fps"
              )
            )
          }
        }

        pb$close()
        vw$release()

        shiny::removeNotification(id = "yolo_step_2")
      }

      # Training images
      dir.create(paste0(yolo_path(), "/YOLO/labels"))
      dir.create(paste0(yolo_path(), "/YOLO/images"))

      dt <- the_stats()
      dt[, select := (select_h & select_w & mod != 2) | (mod == 1)]

      tmp <- cv2$cvtColor(prepped_mask, cv2$COLOR_BGR2GRAY)
      k <- cv2$getStructuringElement(
        cv2$MORPH_RECT,
        as.integer(c(
          2 * input$mask_buffer_x,
          2 * input$mask_buffer_x
        )) +
          1L
      )
      tmp <- cv2$erode(tmp, k)
      roi <- reticulate::py_to_r(tmp)
      roi <- roi[nrow(roi):1, ]

      composite <- prepped_background$copy()
      stamp <- reticulate::np_array(
        array(
          0L,
          c(
            trackRcv::n_row(prepped_background),
            trackRcv::n_col(prepped_background),
            3
          )
        ),
        dtype = "uint8"
      )

      composite_folders <- c("train/", "val/", "test/")
      composite_tasks <- c("training", "validation", "testing")
      n_img <- c(input$n_train_x, input$n_validate_x, input$n_test_x)

      for (i in seq_along(composite_folders)) {
        shiny::showNotification(
          paste0("Creating the ", composite_tasks[i], " composites."),
          id = paste0("yolo_step_3", 2 + i),
          duration = NULL
        )

        dir.create(paste0(yolo_path(), "/YOLO/labels/", composite_folders[i]))
        dir.create(paste0(yolo_path(), "/YOLO/images/", composite_folders[i]))

        pb <- shiny::Progress$new()
        pb$set(message = "Computing: ", value = 0, detail = "0%")
        n <- n_img[i]
        old_check <- 0
        old_frame <- 1
        old_time <- Sys.time()

        for (ii in seq_len(n_img[i])) {
          composite <- prepped_background$copy()
          rnd_loc <- .sampleMinDist(
            roi,
            input$n_instances_x,
            round(mean(dt$width[dt$select]) / 2)
          )
          rnd_blob <- sample(which(dt$select), input$n_instances_x, TRUE)
          rnd_rot <- sample(c(0L, 1L, 2L, -1L), input$n_instances_x, TRUE)
          annotations <- matrix(NA_real_, input$n_instances_x, 9)
          nn <- nrow(rnd_loc)

          for (iii in seq_len(nn)) {
            if (rnd_rot[iii] == -1L) {
              sub <- the_subs[[rnd_blob[iii]]]
              submask <- the_submasks[[rnd_blob[iii]]]
            } else {
              sub <- cv2$rotate(the_subs[[rnd_blob[iii]]], rnd_rot[iii])
              submask <- cv2$rotate(the_submasks[[rnd_blob[iii]]], rnd_rot[iii])
            }

            bottom <- rnd_loc[iii, 1] - round(trackRcv::n_row(sub) / 2)
            left <- rnd_loc[iii, 2] - round(trackRcv::n_col(sub) / 2)
            top <- trackRcv::n_row(stamp) - bottom - trackRcv::n_row(sub)
            right <- trackRcv::n_col(stamp) - left - trackRcv::n_col(sub)

            if (bottom < 0) {
              top <- top + bottom
              bottom <- 0
            }

            if (top < 0) {
              bottom <- bottom + top
              top <- 0
            }

            if (left < 0) {
              right <- right + left
              left <- 0
            }

            if (right < 0) {
              left <- left + right
              right <- 0
            }

            stamp <- cv2$copyMakeBorder(
              sub,
              as.integer(top),
              as.integer(bottom),
              as.integer(left),
              as.integer(right),
              cv2$BORDER_CONSTANT,
              NULL,
              0L
            )
            if (input$dark_button_x == "Darker") {
              composite <- cv2$subtract(composite, stamp)
            } else {
              composite <- cv2$add(composite, stamp)
            }

            nz <- cv2$findNonZero(cv2$cvtColor(submask, cv2$COLOR_BGR2GRAY))
            ell <- cv2$fitEllipse(nz)
            box <- cv2$boxPoints(ell)
            box[, 0] <- (box[, 0] + left) / trackRcv::n_col(stamp)
            box[, 1] <- (box[, 1] + top) / trackRcv::n_row(stamp)

            annotations[iii, ] <- c(0, c(t(reticulate::py_to_r(box))))
          }

          composite <- cv2$multiply(
            composite,
            runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x)
          )
          composite <- cv2$add(composite, runif(1, -input$bias_x, input$bias_x))

          if (input$saltpepper_x > 0) {
            r <- sample(0:input$saltpepper_x, 1)
            sp <- reticulate::np_array(
              array(
                sample(
                  -r:r,
                  trackRcv::n_row(composite) * trackRcv::n_col(composite) * 3,
                  replace = TRUE
                ),
                c(trackRcv::n_row(composite), trackRcv::n_col(composite), 3)
              ),
              dtype = "int_"
            )
            composite <- cv2$add(composite, sp, dtype = composite$dtype$type())
          }

          cv2$imwrite(
            normalizePath(
              paste0(
                yolo_path(),
                "/YOLO/images/",
                composite_folders[i],
                ii,
                ".png"
              ),
              mustWork = FALSE
            ),
            composite
          )

          write.table(
            annotations,
            normalizePath(
              paste0(
                yolo_path(),
                "/YOLO/labels/",
                composite_folders[i],
                ii,
                ".txt"
              ),
              mustWork = FALSE
            ),
            row.names = FALSE,
            col.names = FALSE
          )

          new_check <- floor(100 * ii / n)
          if (new_check > (old_check + 5)) {
            new_time <- Sys.time()
            fps <- (ii - old_frame + 1) /
              as.numeric(difftime(new_time, old_time, units = "secs"))
            old_check <- new_check
            old_frame <- ii
            old_time <- new_time
            pb$set(
              value = new_check / 100,
              detail = paste0(
                new_check,
                "% - ",
                round(fps, digits = 2),
                "fps"
              )
            )
          }
        }

        pb$close()
        shiny::removeNotification(id = paste0("yolo_step_3", 2 + i))
      }

      con <- file(
        normalizePath(
          paste0(yolo_path(), "/YOLO/dataset.yaml"),
          mustWork = FALSE
        ),
        "w"
      )
      write(paste0("path: ", paste0(yolo_path(), "/YOLO/")), con)
      write("train: images/train", con, append = TRUE)
      write("val: images/val", con, append = TRUE)
      write("test: images/test", con, append = TRUE)
      write("", con, append = TRUE)
      write("names:", con, append = TRUE)
      write("    0: object", con, append = TRUE)
      close(con)

      yolo_path(NULL)
      shiny::removeNotification(id = "yolo")
      shinyjs::hideElement("curtain")
    }
  }
})
