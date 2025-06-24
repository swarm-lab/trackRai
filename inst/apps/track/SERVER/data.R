# Globals and reactives
yolo_installed <- yolo_installed()
n_gpus <- reticulate::py_to_r(torch$cuda$device_count())
mps <- reticulate::py_to_r(torch$backends$mps$is_available())
device <- if (n_gpus > 0) "cuda:0" else if (mps) "mps" else "cpu"
the_model <- NULL
volumes <- c(Home = fs::path_home(), getVolumes()())
black_screen <- reticulate::r_to_py(array(0L, c(1080, 1920, 3)))
to_display <- NULL
the_image <- NULL
the_mask <- NULL

the_model_folder <- shiny::reactiveVal()
default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
mask_path <- shiny::reactiveVal()
refresh_display <- shiny::reactiveVal(0)
print_display <- shiny::reactiveVal(0)
the_video <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
the_frame <- shiny::reactiveVal()
refresh_mask <- shiny::reactiveVal(0)


# UI
shinyjs::hideElement("curtain")
toggled_tabs <- data.frame(
  tab = 1:3,
  toggled = c(TRUE, FALSE, FALSE)
)

shiny::observe({
  if (
    !is.null(the_model_folder()) &
      !is.null(video_path()) &
      trackRcv::is_video_capture(the_video)
  ) {
    if (toggled_tabs$toggled[2] == FALSE) {
      .toggleTabs(2:3, "ON")
      toggled_tabs$toggled[2:3] <<- TRUE
    }
  } else {
    .toggleTabs(2:3, "OFF")
    toggled_tabs$toggled[2:3] <<- FALSE
  }
})

output$modelSelect <- shiny::renderUI({
  if (!is.null(the_model_folder())) {
    models <- list.files(paste0(the_model_folder(), "/runs/obb/"))
    shiny::tagList(
      hr(),
      shiny::selectInput(
        "model_x",
        "Select trained model:",
        paste0(models, "/weights/best.pt"),
        width = "100%"
      )
    )
  }
})


# Status
output$yolo_status <- shiny::renderUI({
  if (!yolo_installed) {
    p(
      "No YOLO installation was detected.",
      tags$br(),
      "Please run install_yolo() in the console.",
      class = "bad"
    )
  } else if (is.null(the_model_folder())) {
    p("Dataset missing (and required).", class = "bad")
  }
})

output$video_status <- shiny::renderUI({
  if (refresh_display() > -1 & !trackRcv::is_video_capture(the_video)) {
    p("Video missing (and required).", class = "bad")
  } else if (!trackRcv::is_video_capture(the_video)) {
    p("Incompatible videos.", class = "bad")
  }
})


# Read frame
shiny::observeEvent(input$leftKey, {
  if (trackRcv::is_video_capture(the_video)) {
    vals <- input$video_controls_x

    if (vals[2] > vals[1]) {
      vals[2] <- vals[2] - 1
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    }
  }
})

shiny::observeEvent(input$rightKey, {
  if (trackRcv::is_video_capture(the_video)) {
    vals <- input$video_controls_x

    if (vals[2] < vals[3]) {
      vals[2] <- vals[2] + 1
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    }
  }
})

shiny::observeEvent(input$downKey, {
  if (trackRcv::is_video_capture(the_video)) {
    vals <- input$video_controls_x

    if (vals[2] >= (vals[1] + fps(the_video))) {
      vals[2] <- vals[2] - fps(the_video)
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    } else {
      vals[2] <- vals[1]
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    }
  }
})

shiny::observeEvent(input$upKey, {
  if (trackRcv::is_video_capture(the_video)) {
    vals <- input$video_controls_x

    if (vals[2] <= (vals[3] - fps(the_video))) {
      vals[2] <- vals[2] + fps(the_video)
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    } else {
      vals[2] <- vals[3]
      shinyWidgets::updateNoUiSliderInput(
        session,
        "video_controls_x",
        value = vals
      )
    }
  }
})

shiny::observeEvent(input$video_controls_x, {
  if (trackRcv::is_video_capture(the_video)) {
    the_frame(input$video_controls_x[2])
  }
})

shiny::observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    the_image <<- trackRcv::read_frame(the_video, the_frame())
    refresh_display(refresh_display() + 1)
  }
})


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (trackRcv::is_image(the_image)) {
      to_display <<- the_image$copy()

      if (trackRcv::is_image(the_mask)) {
        to_display <<- cv2$multiply(
          to_display,
          cv2$divide(cv2$compare(the_mask, 0, 1L), 255L)
        )
      }
    } else {
      to_display <<- black_screen$copy()
    }

    print_display(print_display() + 1)
  }
})

output$display <- shiny::renderUI({
  if (print_display() > 0) {
    if (trackRcv::is_image(to_display)) {
      shiny::tags$img(
        src = paste0(
          "data:image/jpg;base64,",
          reticulate::py_to_r(
            base64$b64encode(cv2$imencode(".jpg", to_display)[1])$decode(
              "utf-8"
            )
          )
        ),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    } else {
      shiny::tags$img(
        src = paste0(
          "data:image/jpg;base64,",
          reticulate::py_to_r(
            base64$b64encode(cv2$imencode(".jpg", black_screen)[1])$decode(
              "utf-8"
            )
          )
        ),
        width = "100%",
        id = "display_img",
        draggable = "false"
      )
    }
  }
})

session$onFlushed(
  function() {
    js$uishape("display_img")
  },
  once = TRUE
)

shiny::observeEvent(input$win_resize, {
  js$uishape("display_img")
})

shiny::observeEvent(input$main, {
  refresh_display(refresh_display() + 1)
})


# Load YOLO dataset
shinyFiles::shinyDirChoose(
  input,
  "dataset_x",
  roots = volumes,
  session = session
)

shiny::observeEvent(input$dataset_x, {
  path <- shinyFiles::parseDirPath(volumes, input$dataset_x)
  if (length(path) > 0) {
    check <- any(grepl("train", list.files(paste0(path, "/runs/obb/"))))

    if (check) {
      the_model_folder(path)
    } else {
      the_model_folder(NULL)
      shiny::showNotification(
        "No trained model was found in this dataset. Choose another one.",
        id = "yolo",
        type = "error"
      )
    }
  }
})

shiny::observeEvent(input$model_x, {
  if (file.exists(paste0(the_model_folder(), "/runs/obb/", input$model_x))) {
    the_model <<- ultralytics$YOLO(normalizePath(paste0(
      the_model_folder(),
      "/runs/obb/",
      input$model_x
    )))
  }
})

# Load video
shinyFiles::shinyFileChoose(
  input,
  "video_file",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$video_file, {
  path <- shinyFiles::parseFilePaths(volumes, input$video_file)
  if (nrow(path) > 0) {
    video_path(normalizePath(path$datapath, mustWork = FALSE))
  }
})

shiny::observeEvent(video_path(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(
        video_path(),
        stringr::fixed(sapply(volumes, normalizePath))
      ),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      }
    )
  )
  volume <- volumes[ix]

  if (length(volume) > 0) {
    dir <- dirname(video_path())
    default_root(names(volumes)[ix])
    # default_path(gsub(volume, "", dir))
    default_path(gsub(paste0(".*", volume), "", dir))
  }
})

shiny::observeEvent(video_path(), {
  to_check <- cv2$VideoCapture(video_path())

  if (reticulate::py_to_r(to_check$isOpened())) {
    if (!is.na(trackRcv::n_frames(to_check))) {
      the_video <<- to_check
      the_image <<- the_video$read()[1]

      if (!trackRcv::is_image(the_mask)) {
        the_mask <<- reticulate::np_array(
          array(
            1L,
            c(trackRcv::n_row(the_image), trackRcv::n_col(the_image), 3)
          ),
          dtype = "uint8"
        )
      }

      if (
        !all(
          unlist(reticulate::py_to_r(the_mask$shape)) ==
            unlist(reticulate::py_to_r(the_image$shape))
        )
      ) {
        the_mask <<- reticulate::np_array(
          array(
            1L,
            c(trackRcv::n_row(the_image), trackRcv::n_col(the_image), 3)
          ),
          dtype = "uint8"
        )
      }

      refresh_video(refresh_video() + 1)
      refresh_display(refresh_display() + 1)
    }
  }
})


# Load optional mask
shinyFiles::shinyFileChoose(
  input,
  "mask_file",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$mask_file, {
  path <- shinyFiles::parseFilePaths(volumes, input$mask_file)
  if (nrow(path) > 0) {
    mask_path(normalizePath(path$datapath, mustWork = FALSE))
    refresh_mask(refresh_mask() + 1)
  }
})

shiny::observeEvent(refresh_mask(), {
  if (refresh_mask() > 0) {
    to_check <- cv2$imread(mask_path())

    if (trackRcv::is_image(to_check)) {
      if (
        !all(
          unlist(reticulate::py_to_r(to_check$shape)) ==
            unlist(reticulate::py_to_r(the_image$shape))
        )
      ) {
        shinyalert::shinyalert(
          "Error:",
          "The video and mask do not have the same dimensions.",
          type = "error",
          animation = FALSE,
          closeOnClickOutside = TRUE
        )
        the_mask <<- reticulate::np_array(
          array(
            1L,
            c(trackRcv::n_row(the_image), trackRcv::n_col(the_image), 3)
          ),
          dtype = "uint8"
        )
      } else {
        the_mask <<- cv2$divide(cv2$compare(to_check, 0, 1L), 255L)
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(
            mask_path(),
            stringr::fixed(sapply(volumes, normalizePath))
          ),
          function(l) {
            if (nrow(l) > 0) {
              diff(l[1, ])
            } else {
              NA
            }
          }
        )
      )
      volume <- volumes[ix]

      if (length(volume) > 0) {
        dir <- dirname(mask_path())
        default_root(names(volumes)[ix])
        default_path(gsub(paste0(".*", volume), "", dir))
      }

      refresh_display(refresh_display() + 1)
    }
  }
})
