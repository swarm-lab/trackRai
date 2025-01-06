# Status
output$yoloStatus <- renderUI({
  if (!yolo_installed) {
    p(
      "No YOLO installation was detected.",
      tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  }
})

output$nvidiaStatus <- renderUI({
  if (yolo_installed) {
    if (n_gpus == 0) {
      p(
        "No CUDA framework detected.",
        tags$br(),
        "Training may be very slow.",
        class = "bad"
      )
    }
  }
})


# UI
output$startStop <- renderUI({
  if (monitorProgress()) {
    actionButton(
      "stopTrain_x", "Stop training",
      width = "100%", class = "btn-danger"
    )
  } else {
    if (!is.null(theYOLOPath())) {
      actionButton(
        "startTrain_x", "Start training",
        width = "100%", class = "btn-success"
      )
    } else {
      disabled(actionButton(
        "startTrain_x", "Start training",
        width = "100%", class = "btn-success"
      ))
    }
  }
})

output$console <- renderUI({
  invalidateLater(1000, session)

  if (monitorProgress()) {
    raw <- suppressWarnings(readLines(theTempFile))
    theRawProgress(raw)
    HTML(paste(raw, collapse = "<br/>"))
  }
})

output$display <- renderUI({
  if (input$main == "1") {
    tagList(
      div(
        style = "padding-left: 20px; padding-right: 20px; padding-top: 20px;",
        plotlyOutput("plotly", width = "100%", height = "420px"),
        hr()
      ),
      div(
        htmlOutput("console"),
        style = "padding-left: 20px; padding-right: 20px; padding-bottom: 20px;"
      ),
      tags$head(
        tags$style(
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
    imageOutput("displayImg",
      height = "auto"
    )
  }
})

observeEvent(theModel(), {
  if (is.null(theModel())) {
    toggleTabs(2, "OFF")
  } else {
    toggleTabs(2, "ON")
  }
})


# Events
shinyDirChoose(input, "dataset_x",
  roots = volumes, session = session
)

observeEvent(input$dataset_x, {
  path <- parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    check <- file.exists(paste0(path, "/dataset.yaml")) &
      file.exists(paste0(path, "/video.mp4")) &
      dir.exists(paste0(path, "/images")) &
      dir.exists(paste0(path, "/labels"))

    if (check) {
      theYOLOPath(path)
    } else {
      showNotification(
        "Incorrectly formatted dataset. Choose another one.",
        id = "yolo", type = "error"
      )
    }
  }
})

observeEvent(theYOLOPath(), {
  if (!is.null(theYOLOPath())) {
    enable("startTrain_x")
  } else {
    disable("startTrain_x")
  }
})

observeEvent(input$startTrain_x, {
  if (!is.null(theYOLOPath())) {
    background <- Rvision::image(paste0(theYOLOPath(), "/background.png"))
    theTempFile <<- tempfile(fileext = ".txt")

    if (n_gpus > 1) {
      yolo_proc <<- process$new(
        .yolo_path(),
        c(
          "obb",
          "train",
          "data=dataset.yaml",
          "model=yolo11m-obb.pt",
          paste0("epochs=", input$epochs_x),
          paste0("imgsz=", ncol(background)),
          "batch=8",
          "single_cls=True",
          paste0("device=", paste0((1:n_gpus) - 1, collapse = ","))
        ),
        stdout = theTempFile,
        stderr = "2>&1",
        wd = theYOLOPath()
      )
    } else {
      yolo_proc <<- process$new(
        .yolo_path(),
        c(
          "obb",
          "train",
          "data=dataset.yaml",
          "model=yolo11m-obb.pt",
          paste0("epochs=", input$epochs_x),
          paste0("imgsz=", ncol(background)),
          "batch=-1",
          "single_cls=True"
        ),
        stdout = theTempFile,
        stderr = "2>&1",
        wd = theYOLOPath()
      )
    }

    monitorProgress(TRUE)
    disable("epochs_x")
    disable("dataset_x")
  }
})

observeEvent(input$stopTrain_x, {
  monitorProgress(FALSE)
  enable("epochs_x")
  enable("dataset_x")
  yolo_proc$kill_tree()
})

observeEvent(theRawProgress(), {
  if (monitorProgress()) {
    start <- grep("Starting training for", theRawProgress())
    stop <- grep("Results saved to", theRawProgress())

    if (length(start) > 0) {
      progress_tab <- data.table(string = theRawProgress())[
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
            theYOLOPath(), "/",
            gsub("Results saved to ", "", ansi_strip(theRawProgress()[stop]))
          )
          theModel(model_folder)
          showNotification(
            paste0("Results saved to ", model_folder),
            id = "done", duration = NULL, type = "message"
          )
          monitorProgress(FALSE)
          enable("epochs_x")
          enable("dataset_x")
        }
      }
    }
  }
})


# Plotting
output$plotly <- renderPlotly({
  if (!is.null(theProgress())) {
    plot_ly(theProgress(),
      x = ~epoch, y = ~box_loss, name = "Box loss",
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(y = ~cls_loss, name = "Class loss", mode = "lines+markers") %>%
      add_trace(y = ~dfl_loss, name = "DFL loss", mode = "lines+markers") %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", rangemode = "tozero")
      ) %>%
      config(displayModeBar = FALSE)
  } else {
    plot_ly(type = "scatter", mode = "lines+markers") %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", range = c(0, 1))
      ) %>%
      config(displayModeBar = FALSE)
  }
})
