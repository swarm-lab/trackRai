# Globals and reactives
volumes <- c(Home = fs::path_home(), getVolumes()())
the_temp_file <- NULL
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) {
  "cuda:0"
} else if (mps) {
  "mps"
} else {
  "cpu"
}
yolo_proc <- NULL

yolo_installed <- yolo_installed()
yolo_path <- shiny::reactiveVal()
the_model_folder <- shiny::reactiveVal()
the_model <- shiny::reactiveVal()
retrain <- shiny::reactiveVal(TRUE)
the_raw_progress <- shiny::reactiveVal()
the_progress <- shiny::reactiveVal()
monitor_progress <- shiny::reactiveVal(FALSE)
monitor_tick <- shiny::reactiveVal(0)


# Display
output$console <- shiny::renderUI({
  shiny::invalidateLater(1000, session)

  if (monitor_progress()) {
    raw <- suppressWarnings(readLines(the_temp_file))
    isolate({
      the_raw_progress(raw)
      monitor_tick(monitor_tick() + 1)
    })
    shiny::HTML(paste(raw, collapse = "<br/>"))
  }
})

output$plotly <- plotly::renderPlotly({
  if (!is.null(the_progress())) {
    plotly::plot_ly(
      the_progress(),
      x = ~epoch,
      y = ~box_loss,
      name = "Box loss",
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      plotly::add_trace(
        y = ~cls_loss,
        name = "Class loss",
        mode = "lines+markers"
      ) %>%
      plotly::add_trace(
        y = ~dfl_loss,
        name = "DFL loss",
        mode = "lines+markers"
      ) %>%
      plotly::layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", rangemode = "tozero")
      ) %>%
      plotly::config(displayModeBar = FALSE)
  } else {
    plotly::plot_ly(type = "scatter", mode = "lines+markers") %>%
      plotly::layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", range = c(0, 1))
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
})

output$display <- shiny::renderUI({
  if (input$main == "1") {
    shiny::tagList(
      shiny::div(
        style = "padding-left: 20px; padding-right: 20px; padding-top: 20px;",
        plotly::plotlyOutput("plotly", width = "100%", height = "420px"),
        shiny::hr()
      ),
      shiny::div(
        shiny::htmlOutput("console"),
        style = "padding-left: 20px; padding-right: 20px; padding-bottom: 20px;"
      ),
      shiny::tags$head(
        shiny::tags$style(
          "#console{
            font-size: 10px;
            font-family: monospace;
            overflow-y:auto;
            height: 200px;
            display: flex;
            flex-direction: column-reverse;
            border-style: solid;
            border-width: 1px;
            border-color: black;
            border-radius: 5px;
            background-color: #e5e5e5;
            width: 100%;
            margin: auto;
            padding: 10px;
            text-align: start;
          }"
        )
      )
    )
  } else if (input$main == "2") {
    shiny::uiOutput("display_frame")
  }
})


# Status
output$yolo_status <- shiny::renderUI({
  if (!yolo_installed) {
    shiny::p(
      "No YOLO installation was detected.",
      shiny::tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  }
})

output$nvidia_status <- shiny::renderUI({
  if (yolo_installed) {
    if (n_gpus == 0) {
      shiny::p(
        "No CUDA backend detected.",
        shiny::tags$br(),
        "Training may be very slow.",
        class = "bad"
      )
    }
  }
})


# UI
output$start_stop <- shiny::renderUI({
  if (monitor_progress()) {
    shiny::actionButton(
      "stop_train_x",
      "Stop training",
      width = "100%",
      class = "btn-danger"
    )
  } else {
    if (!is.null(yolo_path()) & retrain()) {
      shiny::actionButton(
        "start_train_x",
        "Start training",
        width = "100%",
        class = "btn-success"
      )
    } else {
      shinyjs::disabled(shiny::actionButton(
        "start_train_x",
        "Start training",
        width = "100%",
        class = "btn-success"
      ))
    }
  }
})

shiny::observeEvent(the_model_folder(), {
  if (is.null(the_model_folder())) {
    .toggleTabs(2, "OFF")
    toggled_tabs[2] <- FALSE
  } else {
    .toggleTabs(2, "ON")
    toggled_tabs[2] <- TRUE
  }
})


# Train
shinyFiles::shinyDirChoose(
  input,
  "dataset_x",
  roots = volumes,
  session = session
)

shiny::observeEvent(input$dataset_x, {
  path <- shinyFiles::parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    check <- file.exists(paste0(path, "/dataset.yaml")) &
      dir.exists(paste0(path, "/images")) &
      dir.exists(paste0(path, "/labels"))

    if (check) {
      if (any(grepl("best.pt", list.files(path, recursive = TRUE)))) {
        shinyalert::shinyalert(
          title = "Trained model detected",
          text = paste0(
            "A trained model was detected in this folder.",
            "\nDo you want to load it for checking",
            "\nor erase it and train anew?"
          ),
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "RETRAIN",
          cancelButtonText = "LOAD",
          inputId = "restart_training"
        )
      }

      yolo_path(path)
    } else {
      shiny::showNotification(
        "Incorrectly formatted dataset. Choose another one.",
        id = "yolo",
        type = "error"
      )
    }
  }
})

shiny::observeEvent(input$restart_training, {
  retrain(input$restart_training)

  if (!input$restart_training) {
    the_model_folder(normalizePath(paste0(yolo_path(), "/runs/obb/train")))
  }
})

shiny::observeEvent(yolo_path(), {
  if (!is.null(yolo_path())) {
    shinyjs::enable("start_train_x")
  } else {
    shinyjs::disable("start_train_x")
  }
})

shiny::observeEvent(input$start_train_x, {
  if (!is.null(yolo_path())) {
    if (retrain()) {
      unlink(paste0(yolo_path(), "/runs/obb/train*"), recursive = TRUE)
      test_image <- cv2$imread(
        list.files(
          normalizePath(yolo_path(), mustWork = FALSE),
          recursive = TRUE,
          pattern = "*.png",
          full.names = TRUE
        )[1]
      )
      imgsz <- trackRcv::n_col(test_image)
      the_temp_file <<- tempfile(fileext = ".txt")
      model <- paste0("yolo11", input$yolo_x, "-obb.pt")
      epochs <- input$epochs_x
      patience <- input$patience_x
      yaml <- yaml::read_yaml(paste0(yolo_path(), "/dataset.yaml"))
      single_cls <- if (length(yaml$names) > 1) "False" else "True"
      if (n_gpus > 1) {
        yolo_proc <<- processx::process$new(
          trackRai:::.yolo_path(),
          c(
            "obb",
            "train",
            "data=dataset.yaml",
            paste0("model=", model),
            paste0("epochs=", epochs),
            paste0("patience=", patience),
            paste0("imgsz=", imgsz),
            "batch=8",
            paste0("single_cls=", single_cls),
            paste0("device=", paste0((1:n_gpus) - 1, collapse = ","))
          ),
          stdout = the_temp_file,
          stderr = "2>&1",
          wd = yolo_path()
        )
      } else if (mps) {
        yolo_proc <<- processx::process$new(
          trackRai:::.yolo_path(),
          c(
            "obb",
            "train",
            "data=dataset.yaml",
            paste0("model=", model),
            paste0("epochs=", epochs),
            paste0("patience=", patience),
            paste0("imgsz=", imgsz),
            "batch=-1",
            paste0("single_cls=", single_cls),
            "device=mps"
          ),
          stdout = the_temp_file,
          stderr = "2>&1",
          wd = yolo_path()
        )
      } else {
        yolo_proc <<- processx::process$new(
          trackRai:::.yolo_path(),
          c(
            "obb",
            "train",
            "data=dataset.yaml",
            paste0("model=", model),
            paste0("epochs=", epochs),
            paste0("patience=", patience),
            paste0("imgsz=", imgsz),
            "batch=-1",
            paste0("single_cls=", single_cls)
          ),
          stdout = the_temp_file,
          stderr = "2>&1",
          wd = yolo_path()
        )
      }

      monitor_progress(TRUE)
      shinyjs::disable("epochs_x")
      shinyjs::disable("patience_x")
      shinyjs::disable("dataset_x")
    }
  }
})

shiny::observeEvent(input$stop_train_x, {
  monitor_progress(FALSE)
  shinyjs::enable("epochs_x")
  shinyjs::enable("patience_x")
  shinyjs::enable("dataset_x")
  yolo_proc$kill_tree()
})

shiny::observeEvent(monitor_tick(), {
  if (monitor_progress() & length(the_raw_progress()) > 0) {
    start <- grep("Starting training for", the_raw_progress())

    if (length(start) > 0) {
      progress_tab <- data.table::data.table(string = the_raw_progress())[
        (1:.N) > start &
          !grepl("Class", string) &
          grepl("100%", string),
      ]

      if (nrow(progress_tab) > 0) {
        progress_tab <- unique(
          progress_tab[,
            tstrsplit(string, "\\s{2,}")[c(2, 4:6)]
          ]
        )
        names(progress_tab) <- c("epoch", "box_loss", "cls_loss", "dfl_loss")
        progress_tab[, epoch := as.numeric(gsub("(\\d+)/(\\d+)", "\\1", epoch))]
        progress_tab[, box_loss := as.numeric(box_loss)]
        progress_tab[, cls_loss := as.numeric(cls_loss)]
        progress_tab[, dfl_loss := as.numeric(dfl_loss)]
        the_progress(progress_tab)
      }
    }

    if (!yolo_proc$is_alive()) {
      stop <- grep("Results saved to", the_raw_progress())

      if (length(stop) > 0) {
        model_folder <- paste0(
          # yolo_path(),
          # "/",
          gsub(
            "Results saved to ",
            "",
            cli::ansi_strip(the_raw_progress()[stop])
          )
        )
        the_model_folder(model_folder)
        shiny::showNotification(
          paste0("Results saved to ", model_folder),
          id = "done",
          duration = NULL,
          type = "message"
        )
      } else {
        shiny::showNotification(
          paste0("Training failed. Logs can be found here: ", the_temp_file),
          id = "done",
          duration = NULL,
          type = "error"
        )
      }

      monitor_progress(FALSE)
      shinyjs::enable("epochs_x")
      shinyjs::enable("dataset_x")
    }
  }
})
