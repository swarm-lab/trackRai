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


# Events
shinyDirChoose(input, "dataset_x",
  roots = volumes, session = session
)

observeEvent(input$dataset_x, {
  path <- parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    if (file.exists(paste0(path, "/dataset.yaml"))) {
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
    mem_folder <- getwd()
    setwd(theYOLOPath())
    background <- Rvision::image("background.png")
    theTempFile <<- tempfile(fileext = ".txt")
    print(theTempFile)

    if (n_gpus > 1) {
      dump <- system(
        paste0(
          .yolo_path(), " obb train data=dataset.yaml",
          " model=yolo11m-obb.pt epochs=", input$epochs_x,
          " imgsz=", ncol(background), " batch=8 single_cls=True",
          paste0(" device=", paste0((1:n_gpus) - 1, collapse = ",")),
          " > ", theTempFile, " 2>&1"
        ),
        wait = FALSE
      )
    } else {
      dump <- system(
        paste0(
          .yolo_path(), " obb train data=dataset.yaml",
          " model=yolo11m-obb.pt epochs=", input$epochs_x,
          " imgsz=", ncol(background), " batch=-1 single_cls=True",
          " > ", theTempFile, " 2>&1"
        ),
        wait = FALSE
      )
    }

    setwd(mem_folder)
    monitorProgress(TRUE)
    disable("epochs_x")
    disable("dataset_x")
  }
})

observeEvent(input$stopTrain_x, {
  monitorProgress(FALSE)
  enable("epochs_x")
  enable("dataset_x")
  system("killall pt_main_thread")
})

observeEvent(theRawProgress(), {
  if (monitorProgress()) {
    start <- grep("Starting training for", theRawProgress())

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
        theProgress(progress_tab)

        if (nrow(progress_tab) >= input$epochs_x) {
          monitorProgress(FALSE)
          enable("epochs_x")
          enable("dataset_x")
        }
      }
    }
  }
})

# Display
output$display <- renderPlotly({
  if (!is.null(theProgress())) {
    plot_ly(theProgress(),
      x = ~epoch, y = ~box_loss, name = "Box loss",
      type = "scatter", mode = "lines"
    ) %>%
      add_trace(y = ~cls_loss, name = "Class loss", mode = "lines") %>%
      add_trace(y = ~dfl_loss, name = "DFL loss", mode = "lines") %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", rangemode = "tozero")
      ) %>%
      config(displayModeBar = FALSE)
  } else {
    plot_ly(type = "scatter", mode = "lines") %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20, pad = 0),
        xaxis = list(title = "Epochs", range = c(0, input$epochs_x)),
        yaxis = list(title = "Loss", range = c(0, 1))
      ) %>%
      config(displayModeBar = FALSE)
  }
})
