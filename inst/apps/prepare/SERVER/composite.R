# Events
observeEvent(input$testComposite_x, {
  if (!is.null(theStats())) {
    showElement("curtain")
    showNotification("Computing composite image.",
      id = "composite",
      duration = NULL
    )

    dt <- theStats()
    rw <- input$rangeWidth_x
    rh <- input$rangeHeight_x
    dt[, select := (width >= rw[1]) & (width <= rw[2]) &
      (height >= rh[1]) & (height <= rh[2])]

    gray <- changeColorSpace(theMask, "GRAY")
    gray1 <- border(gray, 1)
    morph(gray1, "erode",
      k_shape = "rectangle",
      k_height = input$buffer_x,
      k_width = input$buffer_x,
      target = gray1
    )
    subImage(gray1, 1, 1, ncol(gray), nrow(gray), gray)
    roi <- findNonZero(gray)

    theComposite <<- cloneImage(theBackground)
    stamp <- zeros(nrow(theComposite), ncol(theComposite), 3)
    rnd_loc <- sample(1:nrow(roi), input$nObjects_x, TRUE)
    rnd_blob <- sample(which(dt$select), input$nObjects_x, TRUE)
    rnd_rot <- sample(c("CLOCKWISE", "COUNTER", "180", "NONE"), input$nObjects_x, TRUE)

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- input$nObjects_x
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in seq_len(input$nObjects_x)) {
      if (rnd_rot[i] == "NONE") {
        sub <- subs[[rnd_blob[i]]]
        submask <- submasks[[rnd_blob[i]]]
      } else {
        sub <- rotate(subs[[rnd_blob[i]]], rnd_rot[i])
        submask <- rotate(submasks[[rnd_blob[i]]], rnd_rot[i])
      }

      bottom <- roi[rnd_loc[i], 2] - round(nrow(sub) / 2)
      left <- roi[rnd_loc[i], 1] - round(ncol(sub) / 2)
      top <- nrow(stamp) - bottom - nrow(sub)
      right <- ncol(stamp) - left - ncol(sub)

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

      border(sub, top, bottom, left, right, target = stamp)
      subtract(theComposite, stamp, theComposite)

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

    multiply(
      theComposite, runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x),
      theComposite
    )
    add(theComposite, runif(1, -input$bias_x, input$bias_x), theComposite)

    if (input$saltpepper_x > 0) {
      r <- sample(0:input$saltpepper_x, 1)
      sp <- image(
        array(
          sample(-r:r,
            prod(dim(theComposite)),
            replace = TRUE
          ),
          dim = dim(theComposite)
        )
      )
      add(theComposite, sp, theComposite)
    }

    refreshDisplay(refreshDisplay() + 1)

    pb$close()

    removeNotification(id = "composite")
    hideElement("curtain")
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "6") {
    if (!isImage(theComposite) & !isImage(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(1080, 1920, 3),
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else if (!isImage(theComposite)) {
      suppressMessages(
        write.Image(
          theBackground,
          paste0(tmpDir, "/display.bmp"), TRUE
        )
      )
    } else {
      suppressMessages(
        write.Image(theComposite, paste0(tmpDir, "/display.bmp"), TRUE)
      )
    }

    printDisplay(printDisplay() + 1)
  }
})

shinyDirChoose(input, "generateDataset_x",
  roots = volumes, session = session
)

observeEvent(input$generateDataset_x, {
  path <- parseDirPath(volumes, input$generateDataset_x)
  if (length(path) > 0) {
    theYOLOPath(path)
  }
})

observeEvent(theYOLOPath(), {
  if (!is.null(theYOLOPath())) {
    if (dir.exists(paste0(theYOLOPath(), "/YOLO"))) {
      showNotification(
        "A folder named YOLO already exists at this location. Choose another one.",
        id = "yolo", type = "error"
      )
      isolate(theYOLOPath(NULL))
    } else {
      showElement("curtain")
      showNotification("Generating YOLO dataset.", id = "yolo", duration = NULL)

      # Background and mask
      showNotification("Creating background and mask.", id = "yoloStep1", duration = NULL)

      dir.create(paste0(theYOLOPath(), "/YOLO"))

      nz <- findNonZero(extractChannel(theMask, 1))
      x <- min(nz[, 1])
      y <- min(nz[, 2])
      w <- diff(range(nz[, 1])) + 1
      h <- diff(range(nz[, 2])) + 1
      sub <- subImage(theBackground * (theMask / 255), x, y, w, h)

      top <- ceiling((ceiling(h / 32) * 32 - h) / 2)
      bottom <- floor((ceiling(h / 32) * 32 - h) / 2)
      left <- ceiling((ceiling(w / 32) * 32 - w) / 2)
      right <- floor((ceiling(w / 32) * 32 - w) / 2)
      prepped <- border(sub, top, bottom, left, right)
      prepped_background <- cloneImage(prepped)
      write.Image(prepped_background, paste0(theYOLOPath(), "/YOLO/background.png"), TRUE)

      subImage(theMask, x, y, w, h, sub)
      border(sub, top, bottom, left, right, target = prepped)
      write.Image(prepped * 255, paste0(theYOLOPath(), "/YOLO/mask.png"), TRUE)
      prepped_mask <- cloneImage(prepped) / 255

      removeNotification(id = "yoloStep1")

      # Tracking video
      showNotification("Creating video for tracking.", id = "yoloStep2", duration = NULL)

      frame <- readFrame(theVideo, input$rangePos_x[1] - 1)
      vw <- videoWriter(
        paste0(theYOLOPath(), "/YOLO/video.mp4"), "avc1",
        fps(theVideo), nrow(prepped), ncol(prepped)
      )

      pb <- Progress$new()
      pb$set(message = "Computing: ", value = 0, detail = "0%")
      n <- input$rangePos_x[2] - input$rangePos_x[1] + 1
      old_check <- 0
      old_frame <- 1
      old_time <- Sys.time()

      for (i in input$rangePos_x[1]:input$rangePos_x[2]) {
        if (i == 1) {
          readFrame(theVideo, input$rangePos_x[1] - 1, frame)
        } else {
          readNext(theVideo, frame)
        }

        subImage(frame, x, y, w, h, sub)
        border(sub, top, bottom, left, right, target = prepped)
        multiply(prepped, prepped_mask, prepped)
        writeFrame(vw, prepped)

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
      release(vw)

      removeNotification(id = "yoloStep2")

      # Training images
      dir.create(paste0(theYOLOPath(), "/YOLO/labels"))
      dir.create(paste0(theYOLOPath(), "/YOLO/images"))

      dt <- theStats()
      rw <- input$rangeWidth_x
      rh <- input$rangeHeight_x
      dt[, select := (width >= rw[1]) & (width <= rw[2]) &
        (height >= rh[1]) & (height <= rh[2])]

      roi <- findNonZero(
        morph(extractChannel(prepped_mask, 1), "erode",
          k_shape = "rectangle",
          k_height = input$buffer_x,
          k_width = input$buffer_x
        )
      )
      composite <- zeros(nrow(prepped_background), ncol(prepped_background), 3)
      stamp <- zeros(nrow(prepped_background), ncol(prepped_background), 3)
      composite_folders <- c("train/", "val/", "test/")
      composite_tasks <- c("training", "validation", "testing")
      n_img <- c(input$nTrain_x, input$nValidate_x, input$nTest_x)

      for (i in seq_along(composite_folders)) {
        showNotification(paste0("Creating the ", composite_tasks[i], " composites."),
          id = paste0("yoloStep", 2 + i), duration = NULL
        )

        dir.create(paste0(theYOLOPath(), "/YOLO/labels/", composite_folders[i]))
        dir.create(paste0(theYOLOPath(), "/YOLO/images/", composite_folders[i]))

        pb <- Progress$new()
        pb$set(message = "Computing: ", value = 0, detail = "0%")
        n <- n_img[i]
        old_check <- 0
        old_frame <- 1
        old_time <- Sys.time()

        for (ii in seq_len(n_img[i])) {
          cloneImage(prepped_background, composite)
          rnd_loc <- sample(1:nrow(roi), input$nObjects_x, TRUE)
          rnd_blob <- sample(which(dt$select), input$nObjects_x, TRUE)
          rnd_rot <- sample(c("CLOCKWISE", "COUNTER", "180", "NONE"), input$nObjects_x, TRUE)
          annotations <- matrix(NA_real_, input$nObjects_x, 9)

          for (iii in seq_len(input$nObjects_x)) {
            if (rnd_rot[iii] == "NONE") {
              sub <- subs[[rnd_blob[iii]]]
              submask <- submasks[[rnd_blob[iii]]]
            } else {
              sub <- rotate(subs[[rnd_blob[iii]]], rnd_rot[iii])
              submask <- rotate(submasks[[rnd_blob[iii]]], rnd_rot[iii])
            }

            bottom <- roi[rnd_loc[iii], 2] - round(nrow(sub) / 2)
            left <- roi[rnd_loc[iii], 1] - round(ncol(sub) / 2)
            top <- nrow(stamp) - bottom - nrow(sub)
            right <- ncol(stamp) - left - ncol(sub)

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

            border(sub, top, bottom, left, right, target = stamp)
            subtract(composite, stamp, composite)

            nz <- findNonZero(submask)
            bp <- boxPoints(fitEllipse(nz[, 1], nz[, 2]))
            bp[, 1] <- bp[, 1] + left
            bp[, 2] <- bp[, 2] + bottom

            annotations[iii, ] <- c(
              0,
              bp[1, 1] / ncol(stamp),
              1 - bp[1, 2] / nrow(stamp),
              bp[2, 1] / ncol(stamp),
              1 - bp[2, 2] / nrow(stamp),
              bp[3, 1] / ncol(stamp),
              1 - bp[3, 2] / nrow(stamp),
              bp[4, 1] / ncol(stamp),
              1 - bp[4, 2] / nrow(stamp)
            )
          }

          multiply(
            composite, runif(1, 1 / (1 + input$gain_x), 1 + input$gain_x),
            composite
          )
          add(composite, runif(1, -input$bias_x, input$bias_x), composite)

          if (input$saltpepper_x > 0) {
            r <- sample(0:input$saltpepper_x, 1)
            sp <- image(
              array(
                sample(-r:r,
                  prod(dim(composite)),
                  replace = TRUE
                ),
                dim = dim(composite)
              )
            )
            add(composite, sp, composite)
          }

          suppressMessages(
            write.Image(composite, paste0(theYOLOPath(), "/YOLO/images/", composite_folders[i], ii, ".png"))
          )

          write.table(annotations,
            paste0(theYOLOPath(), "/YOLO/labels/", composite_folders[i], ii, ".txt"),
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
        removeNotification(id = paste0("yoloStep", 2 + i))
      }

      con <- file(paste0(theYOLOPath(), "/YOLO/dataset.yaml"), "w")
      write(paste0("path: ", paste0(theYOLOPath(), "/YOLO/")), con)
      write("train: images/train", con, append = TRUE)
      write("val: images/val", con, append = TRUE)
      write("test: images/test", con, append = TRUE)
      write("", con, append = TRUE)
      write("names:", con, append = TRUE)
      write("    0: object", con, append = TRUE)
      close(con)

      removeNotification(id = "yolo")
      hideElement("curtain")
    }
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "testComposite_x"))

