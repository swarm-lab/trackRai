# Globals and reactives
yolo_path <- shiny::reactiveVal()


# Percentages
shiny::observeEvent(input$percent_train_x, {
  train <- input$percent_train_x
  val <- input$percent_validate_x
  test <- input$percent_test_x
  d <- 100 - (train + val + test)

  if (d != 0) {
    op <- sign(d)
    n_steps <- abs(d)

    for (i in seq_len(n_steps)) {
      prefer_val <- ((val + test) %% 2) == 0
      val_at_boundary <- (op == 1 && val == 100) || (op == -1 && val == 0)
      test_at_boundary <- (op == 1 && test == 100) || (op == -1 && test == 0)
      if (
        (prefer_val && !val_at_boundary) || (!prefer_val && test_at_boundary)
      ) {
        val <- val + op
      } else {
        test <- test + op
      }
    }
  }

  shiny::updateNumericInput(session, "percent_validate_x", value = val)
  shiny::updateNumericInput(session, "percent_test_x", value = test)
})

shiny::observeEvent(input$percent_validate_x, {
  train <- input$percent_train_x
  val <- input$percent_validate_x
  test <- input$percent_test_x
  d <- 100 - (train + val + test)

  if (d != 0) {
    op <- sign(d)
    n_steps <- abs(d)

    for (i in seq_len(n_steps)) {
      prefer_train <- ((train + test) %% 2) == 0
      train_at_boundary <- (op == 1 && train == 100) || (op == -1 && train == 0)
      test_at_boundary <- (op == 1 && test == 100) || (op == -1 && test == 0)
      if (
        (prefer_train && !train_at_boundary) ||
          (!prefer_train && test_at_boundary)
      ) {
        train <- train + op
      } else {
        test <- test + op
      }
    }
  }

  shiny::updateNumericInput(session, "percent_train_x", value = train)
  shiny::updateNumericInput(session, "percent_test_x", value = test)
})

shiny::observeEvent(input$percent_test_x, {
  train <- input$percent_train_x
  val <- input$percent_validate_x
  test <- input$percent_test_x
  d <- 100 - (train + val + test)

  if (d != 0) {
    op <- sign(d)
    n_steps <- abs(d)

    for (i in seq_len(n_steps)) {
      prefer_train <- ((train + val) %% 2) == 0
      train_at_boundary <- (op == 1 && train == 100) || (op == -1 && train == 0)
      val_at_boundary <- (op == 1 && val == 100) || (op == -1 && val == 0)
      if (
        (prefer_train && !train_at_boundary) ||
          (!prefer_train && val_at_boundary)
      ) {
        train <- train + op
      } else {
        val <- val + op
      }
    }
  }

  shiny::updateNumericInput(session, "percent_train_x", value = train)
  shiny::updateNumericInput(session, "percent_validate_x", value = val)
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

      # Training images
      dir.create(paste0(yolo_path(), "/YOLO/"))
      dir.create(paste0(yolo_path(), "/YOLO/labels/"))
      dir.create(paste0(yolo_path(), "/YOLO/images/"))
      yolo_folders <- c("train/", "val/", "test/")

      for (i in seq_along(yolo_folders)) {
        dir.create(paste0(yolo_path(), "/YOLO/labels/", yolo_folders[i]))
        dir.create(paste0(yolo_path(), "/YOLO/images/", yolo_folders[i]))
      }

      frames <- unique(obb$frame)
      n_frames <- length(frames)
      percent <- c(
        input$percent_train_x,
        input$percent_validate_x,
        input$percent_test_x
      )
      folder <- rep(yolo_folders, floor(n_frames * percent / 100))
      frames <- sample(frames, length(folder))
      obb[, tag_n := as.numeric(as.factor(tag)) - 1]

      pb <- shiny::Progress$new()
      pb$set(message = "Computing: ", value = 0, detail = "0%")
      n <- length(frames)
      old_check <- 0
      old_frame <- 1
      old_time <- Sys.time()

      for (i in seq_along(frames)) {
        frame <- trackRcv::read_frame(the_video, frames[i])
        cv2$imwrite(
          normalizePath(
            paste0(
              yolo_path(),
              "/YOLO/images/",
              folder[i],
              i,
              ".png"
            ),
            mustWork = FALSE
          ),
          frame
        )

        annotations <- obb[
          frame == frames[i],
          as.list(c(
            tag = tag_n,
            c(
              t(
                reticulate::py_to_r(
                  cv2$boxPoints(
                    list(
                      c(x, y),
                      c(width, height),
                      angle
                    )
                  )
                )
              )
            )
          )),
          by = .I
        ]

        cols <- c("V2", "V4", "V6", "V8")
        annotations[,
          (cols) := lapply(.SD, "/", trackRcv::n_col(frame)),
          .SDcols = cols
        ]
        cols <- c("V3", "V5", "V7", "V9")
        annotations[,
          (cols) := lapply(.SD, "/", trackRcv::n_row(frame)),
          .SDcols = cols
        ]

        write.table(
          annotations[, .SD, .SDcols = !c("I")],
          normalizePath(
            paste0(
              yolo_path(),
              "/YOLO/labels/",
              folder[i],
              i,
              ".txt"
            ),
            mustWork = FALSE
          ),
          row.names = FALSE,
          col.names = FALSE
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

      pb$close()

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

      tags <- unique(obb[, c("tag_n", "tag")], by = c("tag_n", "tag"))[order(
        tag_n
      )]

      for (i in seq_len(nrow(tags))) {
        write(
          paste0("    ", tags[i, "tag_n"], ": ", tags[i, "tag"]),
          con,
          append = TRUE
        )
      }

      close(con)

      yolo_path(NULL)
      shiny::removeNotification(id = "yolo")
      shinyjs::hideElement("curtain")
    }
  }
})
