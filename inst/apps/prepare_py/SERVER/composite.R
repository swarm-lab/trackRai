# Events
shiny::observeEvent(input$testComposite_x, {
  if (!is.null(theStats())) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Computing composite image.",
      id = "composite",
      duration = NULL
    )

    dt <- theStats()
    rw <- input$rangeWidth_x
    rh <- input$rangeHeight_x
    dt[, select := (width >= rw[1]) & (width <= rw[2]) &
      (height >= rh[1]) & (height <= rh[2])]

    theMask <- cv2$compare(cv2$imread(normalizePath("~/Desktop/termites/mask.png")), 0, 1L)
    mask <- cv2$cvtColor(theMask, cv2$COLOR_BGR2GRAY)
    tmp <- cv2$copyMakeBorder(mask, 1L, 1L, 1L, 1L, cv2$BORDER_CONSTANT, NULL, 0L)
    k <- cv2$getStructuringElement(
      cv2$MORPH_RECT,
      as.integer(c(
        2 * input$buffer_x,
        2 * input$buffer_x
      )) + 1L
    )
    tmp <- cv2$erode(tmp, k)
    mask <- tmp[1:(1 + trackRai::n_row(mask)), 1:(1 + trackRai::n_col(mask))]
    nz <- cv2$findNonZero(mask)
    roi <- cbind(reticulate::py_to_r(nz[, , 0]), reticulate::py_to_r(nz[, , 1]))

    theComposite <<- theBackground$copy()
    stamp <- reticulate::np_array(
      array(0L, c(trackRai::n_row(theComposite), trackRai::n_col(theComposite), 3)),
      dtype = "uint8"
    )

    rnd_loc <- sample(1:nrow(roi), input$nObjects_x, TRUE)
    rnd_blob <- sample(which(dt$select), input$nObjects_x, TRUE)
    rnd_rot <- sample(c(0L, 1L, 2L, -1L), input$nObjects_x, TRUE)

    pb <- shiny::Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- input$nObjects_x
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in seq_len(input$nObjects_x)) {
      if (rnd_rot[i] == -1L) {
        sub <- subs[[rnd_blob[i]]]
        submask <- submasks[[rnd_blob[i]]]
      } else {
        sub <- cv2$rotate(subs[[rnd_blob[i]]], rnd_rot[i])
        submask <- cv2$rotate(submasks[[rnd_blob[i]]], rnd_rot[i])
      }

      bottom <- roi[rnd_loc[i], 2] - round(trackRai::n_row(sub) / 2)
      left <- roi[rnd_loc[i], 1] - round(trackRai::n_col(sub) / 2)
      top <- trackRai::n_row(stamp) - bottom - trackRai::n_row(sub)
      right <- trackRai::n_col(stamp) - left - trackRai::n_col(sub)

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
        sub, as.integer(top), as.integer(bottom),
        as.integer(left), as.integer(right), cv2$BORDER_CONSTANT, NULL, 0L
      )
      theComposite <<- cv2$subtract(theComposite, stamp)

      new_check <- floor(100 * i / n)
      if (new_check > (old_check + 5)) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time,
          units = "secs"
        ))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(
            new_check, "% - ",
            round(fps, digits = 2), "fps"
          )
        )
      }
    }

    theComposite <<- cv2$multiply(theComposite, runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x))
    theComposite <<- cv2$add(theComposite, runif(1, -input$bias_x, input$bias_x))

    if (input$saltpepper_x > 0) {
      r <- sample(0:input$saltpepper_x, 1)
      sp <- reticulate::np_array(
        array(
          sample(-r:r, trackRai::n_row(theComposite) * trackRai::n_col(theComposite) * 3, replace = TRUE),
          c(trackRai::n_row(theComposite), trackRai::n_col(theComposite), 3)
        ),
        dtype = "int_"
      )
      theComposite <<- cv2$add(theComposite, sp, dtype = theComposite$dtype$type())
    }

    refreshDisplay(refreshDisplay() + 1)

    pb$close()

    shiny::removeNotification(id = "composite")
    shinyjs::hideElement("curtain")
  }
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "6") {
    if (!trackRai::is_image(theComposite)) {
      toDisplay <<- theBackground
    } else {
      toDisplay <<- theComposite
    }

    printDisplay(printDisplay() + 1)
  }
})

shinyFiles::shinyDirChoose(input, "generateDataset_x",
  roots = volumes, session = session
)

shiny::observeEvent(input$generateDataset_x, {
  path <- shinyFiles::parseDirPath(volumes, input$generateDataset_x)
  if (length(path) > 0) {
    theYOLOPath(path)
  }
})

shiny::observeEvent(theYOLOPath(), {
  if (!is.null(theYOLOPath())) {
    if (dir.exists(paste0(theYOLOPath(), "/YOLO"))) {
      shiny::showNotification(
        "A folder named YOLO already exists at this location. Choose another one.",
        id = "yolo", type = "error"
      )
      shiny::isolate(theYOLOPath(NULL))
    } else {
      shinyjs::showElement("curtain")
      shiny::showNotification("Generating YOLO dataset.", id = "yolo", duration = NULL)

      # Background and mask
      shiny::showNotification("Creating background and mask.", id = "yoloStep1", duration = NULL)

      dir.create(paste0(theYOLOPath(), "/YOLO"))

      nz <- cv2$findNonZero(cv2$cvtColor(theMask, cv2$COLOR_BGR2GRAY))
      roi <- cbind(reticulate::py_to_r(nz[, , 0]), reticulate::py_to_r(nz[, , 1]))
      x <- min(roi[, 1])
      y <- min(roi[, 2])
      w <- diff(range(roi[, 1])) + 1
      h <- diff(range(roi[, 2])) + 1

      sub <- cv2$multiply(theBackground, cv2$divide(theMask, 255L))[y:(y + h), x:(x + w)]

      top <- ceiling((ceiling(h / 32) * 32 - h) / 2)
      bottom <- floor((ceiling(h / 32) * 32 - h) / 2)
      left <- ceiling((ceiling(w / 32) * 32 - w) / 2)
      right <- floor((ceiling(w / 32) * 32 - w) / 2)

      prepped <- cv2$copyMakeBorder(
        sub, as.integer(top), as.integer(bottom),
        as.integer(left), as.integer(right), cv2$BORDER_CONSTANT, NULL, 0L
      )
      prepped_background <- prepped$copy()
      cv2$imwrite(normalizePath(paste0(theYOLOPath(), "/YOLO/background.png"), mustWork = FALSE), prepped_background)

      sub <- theMask[y:(y + h), x:(x + w)]
      prepped <- cv2$copyMakeBorder(
        sub, as.integer(top), as.integer(bottom),
        as.integer(left), as.integer(right), cv2$BORDER_CONSTANT, NULL, 0L
      )
      cv2$imwrite(normalizePath(paste0(theYOLOPath(), "/YOLO/mask.png"), mustWork = FALSE), prepped)

      prepped_mask <- cv2$divide(prepped, 255L)

      shiny::removeNotification(id = "yoloStep1")

      # Tracking video
      shiny::showNotification("Creating video for tracking.", id = "yoloStep2", duration = NULL)

      theVideo$set(cv2$CAP_PROP_POS_FRAMES, input$rangePos_x[1] - 1)
      vw <- cv2$VideoWriter(
        normalizePath(paste0(theYOLOPath(), "/YOLO/video.mp4"), mustWork = FALSE),
        cv2$VideoWriter_fourcc("A", "V", "C", "1"),
        theVideo$get(cv2$CAP_PROP_FPS),
        as.integer(c(trackRai::n_col(prepped), trackRai::n_row(prepped)))
      )

      pb <- shiny::Progress$new()
      pb$set(message = "Computing: ", value = 0, detail = "0%")
      n <- input$rangePos_x[2] - input$rangePos_x[1] + 1
      old_check <- 0
      old_frame <- 1
      old_time <- Sys.time()

      for (i in input$rangePos_x[1]:input$rangePos_x[2]) {
        frame <- theVideo$read()[1]
        sub <- frame[y:(y + h), x:(x + w)]
        prepped <- cv2$copyMakeBorder(
          sub, as.integer(top), as.integer(bottom),
          as.integer(left), as.integer(right), cv2$BORDER_CONSTANT, NULL, 0L
        )
        prepped <- cv2$multiply(prepped, prepped_mask)
        vw$write(prepped)

        new_check <- floor(100 * i / n)
        if (new_check > (old_check + 5)) {
          new_time <- Sys.time()
          fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time,
            units = "secs"
          ))
          old_check <- new_check
          old_frame <- i
          old_time <- new_time
          pb$set(
            value = new_check / 100,
            detail = paste0(
              new_check, "% - ",
              round(fps, digits = 2), "fps"
            )
          )
        }
      }

      pb$close()
      vw$release()

      shiny::removeNotification(id = "yoloStep2")

      # Training images
      dir.create(paste0(theYOLOPath(), "/YOLO/labels"))
      dir.create(paste0(theYOLOPath(), "/YOLO/images"))

      dt <- theStats()
      rw <- input$rangeWidth_x
      rh <- input$rangeHeight_x
      dt[, select := (width >= rw[1]) & (width <= rw[2]) &
        (height >= rh[1]) & (height <= rh[2])]

      tmp <- cv2$cvtColor(prepped_mask, cv2$COLOR_BGR2GRAY)
      k <- cv2$getStructuringElement(
        cv2$MORPH_RECT,
        as.integer(c(
          2 * input$buffer_x,
          2 * input$buffer_x
        )) + 1L
      )
      tmp <- cv2$erode(tmp, k)
      nz <- cv2$findNonZero(tmp)
      roi <- cbind(reticulate::py_to_r(nz[, , 0]), reticulate::py_to_r(nz[, , 1]))

      composite <- prepped_background$copy()
      stamp <- reticulate::np_array(
        array(0L, c(trackRai::n_row(prepped_background), trackRai::n_col(prepped_background), 3)),
        dtype = "uint8"
      )

      composite_folders <- c("train/", "val/", "test/")
      composite_tasks <- c("training", "validation", "testing")
      n_img <- c(input$nTrain_x, input$nValidate_x, input$nTest_x)

      for (i in seq_along(composite_folders)) {
        shiny::showNotification(paste0("Creating the ", composite_tasks[i], " composites."),
          id = paste0("yoloStep", 2 + i), duration = NULL
        )

        dir.create(paste0(theYOLOPath(), "/YOLO/labels/", composite_folders[i]))
        dir.create(paste0(theYOLOPath(), "/YOLO/images/", composite_folders[i]))

        pb <- shiny::Progress$new()
        pb$set(message = "Computing: ", value = 0, detail = "0%")
        n <- n_img[i]
        old_check <- 0
        old_frame <- 1
        old_time <- Sys.time()

        for (ii in seq_len(n_img[i])) {
          composite <- prepped_background$copy()
          rnd_loc <- sample(1:nrow(roi), input$nObjects_x, TRUE)
          rnd_blob <- sample(which(dt$select), input$nObjects_x, TRUE)
          rnd_rot <- sample(c(0L, 1L, 2L, -1L), input$nObjects_x, TRUE)
          annotations <- matrix(NA_real_, input$nObjects_x, 9)

          for (iii in seq_len(input$nObjects_x)) {
            if (rnd_rot[iii] == -1L) {
              sub <- subs[[rnd_blob[iii]]]
              submask <- submasks[[rnd_blob[iii]]]
            } else {
              sub <- cv2$rotate(subs[[rnd_blob[iii]]], rnd_rot[iii])
              submask <- cv2$rotate(submasks[[rnd_blob[iii]]], rnd_rot[iii])
            }

            bottom <- roi[rnd_loc[iii], 2] - round(trackRai::n_row(sub) / 2)
            left <- roi[rnd_loc[iii], 1] - round(trackRai::n_col(sub) / 2)
            top <- trackRai::n_row(stamp) - bottom - trackRai::n_row(sub)
            right <- trackRai::n_col(stamp) - left - trackRai::n_col(sub)

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
              sub, as.integer(top), as.integer(bottom),
              as.integer(left), as.integer(right), cv2$BORDER_CONSTANT, NULL, 0L
            )
            composite <- cv2$subtract(composite, stamp)

            nz <- cv2$findNonZero(cv2$cvtColor(submask, cv2$COLOR_BGR2GRAY))
            ell <- cv2$fitEllipse(nz)
            box <- reticulate::py_to_r(cv2$boxPoints(ell))
            box[, 1] <- box[, 1] + left
            box[, 2] <- box[, 2] + bottom

            annotations[iii, ] <- c(
              0,
              box[1, 1] / ncol(stamp),
              1 - box[1, 2] / nrow(stamp),
              box[2, 1] / ncol(stamp),
              1 - box[2, 2] / nrow(stamp),
              box[3, 1] / ncol(stamp),
              1 - box[3, 2] / nrow(stamp),
              box[4, 1] / ncol(stamp),
              1 - box[4, 2] / nrow(stamp)
            )
          }

          composite <- cv2$multiply(composite, runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x))
          composite <- cv2$add(composite, runif(1, -input$bias_x, input$bias_x))

          if (input$saltpepper_x > 0) {
            r <- sample(0:input$saltpepper_x, 1)
            sp <- reticulate::np_array(
              array(
                sample(-r:r, trackRai::n_row(composite) * trackRai::n_col(composite) * 3, replace = TRUE),
                c(trackRai::n_row(composite), trackRai::n_col(composite), 3)
              ),
              dtype = "int_"
            )
            composite <- cv2$add(composite, sp, dtype = composite$dtype$type())
          }

          cv2$imwrite(
            normalizePath(paste0(theYOLOPath(), "/YOLO/images/", composite_folders[i], ii, ".png"), mustWork = FALSE), 
            composite
          )

          write.table(annotations,
            normalizePath(paste0(theYOLOPath(), "/YOLO/labels/", composite_folders[i], ii, ".txt"), mustWork = FALSE),
            row.names = FALSE,
            col.names = FALSE
          )

          new_check <- floor(100 * ii / n)
          if (new_check > (old_check + 5)) {
            new_time <- Sys.time()
            fps <- (ii - old_frame + 1) / as.numeric(difftime(new_time, old_time,
              units = "secs"
            ))
            old_check <- new_check
            old_frame <- ii
            old_time <- new_time
            pb$set(
              value = new_check / 100,
              detail = paste0(
                new_check, "% - ",
                round(fps, digits = 2), "fps"
              )
            )
          }
        }

        pb$close()
        shiny::removeNotification(id = paste0("yoloStep", 2 + i))
      }

      con <- file(normalizePath(paste0(theYOLOPath(), "/YOLO/dataset.yaml"), mustWork = FALSE), "w")
      write(paste0("path: ", paste0(theYOLOPath(), "/YOLO/")), con)
      write("train: images/train", con, append = TRUE)
      write("val: images/val", con, append = TRUE)
      write("test: images/test", con, append = TRUE)
      write("", con, append = TRUE)
      write("names:", con, append = TRUE)
      write("    0: object", con, append = TRUE)
      close(con)

      shiny::removeNotification(id = "yolo")
      shinyjs::hideElement("curtain")
    }
  }
})
