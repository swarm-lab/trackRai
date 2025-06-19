# Display
output$display_frame <- shiny::renderUI({
  if (refresh_frame() > 0) {
    if (trackRcv::is_image(the_frame)) {
      to_display <<- cv2$multiply(
        the_frame,
        cv2$divide(cv2$compare(the_mask, 0, 1L), 255L)
      )

      pred <- the_model()(
        source = to_display,
        imgsz = c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)),
        conf = input$conf_x,
        iou = input$iou_x,
        max_det = as.integer(input$max_objects_x),
        show = FALSE,
        verbose = FALSE,
        device = device
      )
      obb <- pred[0]$obb$xyxyxyxy$cpu()$numpy()
      obb <- np$int_(obb)
      sc <- max(
        c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)) / 720
      )

      for (i in seq_len(py_to_r(obb$shape[0]))) {
        .drawContour(
          to_display,
          list(obb[i - 1]),
          color = c(0L, 224L, 0L),
          contrast = c(255, 255, 255),
          thickness = as.integer(max(0.5, sc))
        )
        
        # to_display <<- cv2$drawContours(
        #   to_display,
        #   list(obb[i - 1]),
        #   0L,
        #   c(255L, 255L, 255),
        #   as.integer(max(0.5, 2 * sc))
        # )
        # to_display <<- cv2$drawContours(
        #   to_display,
        #   list(obb[i - 1]),
        #   0L,
        #   c(0L, 224L, 0L),
        #   as.integer(max(0.5, sc))
        # )
      }

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
})


# Status
output$video_status <- shiny::renderUI({
  if (refresh_frame() > -1 & !trackRcv::is_video_capture(the_video)) {
    p("Video missing (and required).", class = "bad")
  } else if (!trackRcv::is_video_capture(the_video)) {
    p("Incompatible videos.", class = "bad")
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
    default_path(gsub(paste0(".*", volume), "", dir))
  }
})

shiny::observeEvent(video_path(), {
  to_check <- cv2$VideoCapture(video_path())

  if (reticulate::py_to_r(to_check$isOpened())) {
    if (!is.na(trackRcv::n_frames(to_check))) {
      the_video <<- to_check
      the_frame <<- to_check$read()[1]

      if (!trackRcv::is_image(the_mask)) {
        the_mask <<- reticulate::np_array(
          array(
            1L,
            c(trackRcv::n_row(the_frame), trackRcv::n_col(the_frame), 3)
          ),
          dtype = "uint8"
        )
      }

      if (
        !all(
          unlist(reticulate::py_to_r(the_mask$shape)) ==
            unlist(reticulate::py_to_r(the_frame$shape))
        )
      ) {
        the_mask <<- reticulate::np_array(
          array(
            1L,
            c(trackRcv::n_row(the_frame), trackRcv::n_col(the_frame), 3)
          ),
          dtype = "uint8"
        )
      }

      refresh_video(refresh_video() + 1)
      refresh_frame(refresh_frame() + 1)
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
            unlist(reticulate::py_to_r(the_frame$shape))
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
            c(trackRcv::n_row(the_frame), trackRcv::n_col(the_frame), 3)
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
      dir <- dirname(mask_path())
      default_root(names(volumes)[ix])
      default_path(gsub(paste0(".*", volume), "", dir))

      refresh_frame(refresh_frame() + 1)
    }
  }
})


# Predict
shiny::observeEvent(the_model_folder(), {
  if (is.null(the_model_folder())) {
    the_model(NULL)
  } else {
    the_model(ultralytics$YOLO(normalizePath(
      paste0(the_model_folder(), "/weights/best.pt"),
      mustWork = FALSE
    )))
  }
})

shiny::observeEvent(input$video_controls_x, {
  if (trackRcv::is_video_capture(the_video)) {
    the_frame <<- read_frame(the_video, input$video_controls_x[1])
    refresh_frame(refresh_frame() + 1)
  }
})
