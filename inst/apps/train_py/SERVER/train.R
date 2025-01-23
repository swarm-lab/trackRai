# Status
output$yoloStatus <- shiny::renderUI({
  if (!yolo_installed) {
    p(
      "No YOLO installation was detected.",
      tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  }
})

output$nvidiaStatus <- shiny::renderUI({
  if (yolo_installed) {
    if (n_gpus == 0 & !mps) {
      p(
        "No CUDA or MPS backend detected.",
        tags$br(),
        "Training may be very slow.",
        class = "bad"
      )
    }
  }
})


# UI
output$startStop <- shiny::renderUI({
  if (monitorProgress()) {
    shiny::actionButton(
      "stopTrain_x", "Stop training",
      width = "100%", class = "btn-danger"
    )
  } else {
    if (!is.null(yolo_path())) {
      shiny::actionButton(
        "startTrain_x", "Start training",
        width = "100%", class = "btn-success"
      )
    } else {
      shinyjs::disabled(shiny::actionButton(
        "startTrain_x", "Start training",
        width = "100%", class = "btn-success"
      ))
    }
  }
})

output$console <- shiny::renderUI({
  invalidateLater(1000, session)

  if (monitorProgress()) {
    raw <- suppressWarnings(readLines(theTempFile))
    theRawProgress(raw)
    shiny::HTML(paste(raw, collapse = "<br/>"))
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
    # shiny::imageOutput("display_img",
    #   height = "auto"
    # )
    shiny::uiOutput("displayFrame")
  }
})

shiny::observeEvent(theModelFolder(), {
  if (is.null(theModelFolder())) {
    toggleTabs(2, "OFF")
  } else {
    toggleTabs(2, "ON")
  }
})


# Events
shinyFiles::shinyDirChoose(input, "dataset_x",
  roots = volumes, session = session
)

shiny::observeEvent(input$dataset_x, {
  path <- shinyFiles::parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    check <- file.exists(paste0(path, "/dataset.yaml")) &
      file.exists(paste0(path, "/video.mp4")) &
      dir.exists(paste0(path, "/images")) &
      dir.exists(paste0(path, "/labels"))

    if (check) {
      yolo_path(path)
    } else {
      shiny::showNotification(
        "Incorrectly formatted dataset. Choose another one.",
        id = "yolo", type = "error"
      )
    }
  }
})

shiny::observeEvent(yolo_path(), {
  if (!is.null(yolo_path())) {
    shinyjs::enable("startTrain_x")
  } else {
    shinyjs::disable("startTrain_x")
  }
})

shiny::observeEvent(input$startTrain_x, {
  if (!is.null(yolo_path())) {
    background <- cv2$imread(normalizePath(paste0(yolo_path(), "/background.png"), mustWork = FALSE))
    imgsz <- trackRai::n_col(background)
    theTempFile <<- tempfile(fileext = ".txt")

    if (n_gpus > 1) {
      yolo_proc <<- processx::process$new(
        trackRai:::.yolo_path(),
        c(
          "obb",
          "train",
          "data=dataset.yaml",
          paste0("model=yolo11", input$yolo_x, "-obb.pt"),
          paste0("epochs=", input$epochs_x),
          paste0("imgsz=", imgsz),
          "batch=8",
          "single_cls=True",
          paste0("device=", paste0((1:n_gpus) - 1, collapse = ","))
        ),
        stdout = theTempFile,
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
          paste0("model=yolo11", input$yolo_x, "-obb.pt"),
          paste0("epochs=", input$epochs_x),
          paste0("imgsz=", imgsz),
          "batch=-1",
          "single_cls=True",
          "device=mps"
        ),
        stdout = theTempFile,
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
          paste0("model=yolo11", input$yolo_x, "-obb.pt"),
          paste0("epochs=", input$epochs_x),
          paste0("imgsz=", imgsz),
          "batch=-1",
          "single_cls=True"
        ),
        stdout = theTempFile,
        stderr = "2>&1",
        wd = yolo_path()
      )
    }

    monitorProgress(TRUE)
    shinyjs::disable("epochs_x")
    shinyjs::disable("dataset_x")
  }
})

shiny::observeEvent(input$stopTrain_x, {
  monitorProgress(FALSE)
  shinyjs::enable("epochs_x")
  shinyjs::enable("dataset_x")
  yolo_proc$kill_tree()
})

shiny::observeEvent(theRawProgress(), {
  if (monitorProgress()) {
    start <- grep("Starting training for", theRawProgress())
    stop <- grep("Results saved to", theRawProgress())

    if (length(start) > 0) {
      progress_tab <- data.table::data.table(string = theRawProgress())[
        (1:.N) > start &
          !grepl("Class", string) &
          grepl("100%", string),
      ]

      if (nrow(progress_tab) > 0) {
        progress_tab <- unique(
          progress_tab[
            ,
            tstrsplit(string, "\\s{2,}")[c(2, 4:6)]
          ]
        )
        names(progress_tab) <- c("epoch", "box_loss", "cls_loss", "dfl_loss")
        progress_tab[, epoch := as.numeric(gsub("(\\d+)/(\\d+)", "\\1", epoch))]
        progress_tab[, box_loss := as.numeric(box_loss)]
        progress_tab[, cls_loss := as.numeric(cls_loss)]
        progress_tab[, dfl_loss := as.numeric(dfl_loss)]
        theProgress(progress_tab)

        if (length(stop) > 0) {
          model_folder <- paste0(
            yolo_path(), "/",
            gsub("Results saved to ", "", ansi_strip(theRawProgress()[stop]))
          )
          theModelFolder(model_folder)
          shiny::showNotification(
            paste0("Results saved to ", model_folder),
            id = "done", duration = NULL, type = "message"
          )
          monitorProgress(FALSE)
          shinyjs::enable("epochs_x")
          shinyjs::enable("dataset_x")
        }
      }
    }
  }
})


# Plotting
output$plotly <- plotly::renderPlotly({
  if (!is.null(theProgress())) {
    plotly::plot_ly(theProgress(),
      x = ~epoch, y = ~box_loss, name = "Box loss",
      type = "scatter", mode = "lines+markers"
    ) %>%
      plotly::add_trace(y = ~cls_loss, name = "Class loss", mode = "lines+markers") %>%
      plotly::add_trace(y = ~dfl_loss, name = "DFL loss", mode = "lines+markers") %>%
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
