# Globals and reactives
volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
video_range <- c(0, 0)
the_video <- NULL
the_image <- NULL

default_root <- shiny::reactiveVal()
default_path <- shiny::reactiveVal("")
video_path <- shiny::reactiveVal()
output_path <- shiny::reactiveVal()
the_frame <- shiny::reactiveVal()
refresh_video <- shiny::reactiveVal(0)
roi <- shiny::reactiveVal(c(1, 1920, 1, 1080))
collect_roi <- shiny::reactiveVal(0)
stop_roi_collection <- shiny::reactiveVal(0)


# UI
output$video_status <- shiny::renderUI({
  if (refresh_display() > -1 & !trackRcv::is_video_capture(the_video)) {
    shiny::p("Video missing (and required).", class = "bad")
  } else if (!trackRcv::is_video_capture(the_video)) {
    shiny::p("Incompatible videos.", class = "bad")
  } else {
    NULL
  }
})

shiny::observeEvent(refresh_display(), {
  if (!trackRcv::is_video_capture(the_video)) {
    shinyjs::hide("output_controls")
  } else {
    shinyjs::show("output_controls")
  }
})


# Display
shiny::observeEvent(input$main, {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(roi(), {
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (trackRcv::is_image(the_image)) {
      to_display <<- the_image$copy()
      sc <- max(
        c(trackRcv::n_row(to_display), trackRcv::n_col(to_display)) / 720
      )

      mean_color <- unlist(reticulate::py_to_r(cv2$mean(to_display)))[3:1]
      d <- apply(.shades, 2, function(x) sum((x - mean_color)^2))
      ix <- which.max(d)

      .drawPolyLine(
        to_display,
        rbind(
          c(roi()[1] - 1, roi()[3] - 1),
          c(roi()[2] - 1, roi()[3] - 1),
          c(roi()[2] - 1, roi()[4] - 1),
          c(roi()[1] - 1, roi()[4] - 1)
        ),
        closed = TRUE,
        color = .shades[3:1, ix],
        contrast = c(255, 255, 255),
        thickness = max(1, round(sc)),
        outline = max(1, round(sc))
      )
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
    js$imgshape("display_img")
  },
  once = FALSE
)

shiny::observeEvent(input$win_resize, {
  js$uishape("display_img")
  js$imgshape("display_img")
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
      if (trackRcv::n_frames(to_check) > 1) {
        the_video <<- to_check
        the_image <<- the_video$read()[1]
        roi(c(1, trackRcv::n_col(the_image), 1, trackRcv::n_row(the_image)))
        refresh_video(refresh_video() + 1)
        refresh_display(refresh_display() + 1)
      }
    }
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

    if (vals[2] >= (vals[1] + trackRcv::fps(the_video))) {
      vals[2] <- vals[2] - trackRcv::fps(the_video)
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

    if (vals[2] <= (vals[3] - trackRcv::fps(the_video))) {
      vals[2] <- vals[2] + trackRcv::fps(the_video)
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

shiny::observeEvent(input$main, {
  if (trackRcv::is_video_capture(the_video)) {
    if (input$main %in% c("1", "2")) {
      the_frame(input$video_controls_x[2])
    }
  }
})

shiny::observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    the_image <<- trackRcv::read_frame(the_video, the_frame())
    refresh_display(refresh_display() + 1)
  }
})


# ROI
shiny::observeEvent(input$roi_left, {
  if (trackRcv::is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1, "OFF")

    shiny::showNotification(
      "Click a point on the left boundary of the ROI. Esc to cancel.",
      id = "roi_notif",
      duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_roi(1)
  }
})

shiny::observeEvent(input$roi_right, {
  if (trackRcv::is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1, "OFF")

    shiny::showNotification(
      "Click a point on the right boundary of the ROI. Esc to cancel.",
      id = "roi_notif",
      duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_roi(2)
  }
})

shiny::observeEvent(input$roi_top, {
  if (trackRcv::is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1, "OFF")

    shiny::showNotification(
      "Click a point on the top boundary of the ROI. Esc to cancel.",
      id = "roi_notif",
      duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_roi(3)
  }
})

shiny::observeEvent(input$roi_bottom, {
  if (trackRcv::is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1, "OFF")

    shiny::showNotification(
      "Click a point on the bottom boundary of the ROI. Esc to cancel.",
      id = "roi_notif",
      duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_roi(4)
  }
})

shinyjs::onevent("click", "display_img", function(props) {
  px <- trackRcv::n_col(to_display) *
    ((props$offsetX -
      (input$display_img_uiwidth - input$display_img_imgwidth) / 2) /
      input$display_img_imgwidth)
  py <- trackRcv::n_row(to_display) *
    (props$offsetY / input$display_img_imgheight)

  if (collect_roi() > 0) {
    tmp <- roi()

    if (collect_roi() == 1) {
      if (round(px) < tmp[2]) {
        tmp[1] <- round(px)
      }
    } else if (collect_roi() == 2) {
      if (round(px) > tmp[1]) {
        tmp[2] <- round(px)
      }
    } else if (collect_roi() == 3) {
      if (round(py) < tmp[4]) {
        tmp[3] <- round(py)
      }
    } else if (collect_roi() == 4) {
      if (round(py) > tmp[3]) {
        tmp[4] <- round(py)
      }
    }

    roi(tmp)
    stop_roi_collection(stop_roi_collection() + 1)
  }

  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$escKey, {
  if (collect_roi() > 0) {
    stop_roi_collection(stop_roi_collection() + 1)
  }
})

shiny::observeEvent(stop_roi_collection(), {
  shiny::removeNotification(id = "roi_notif")
  .toggleInputs(input, "ON")
  .toggleTabs(1, "ON")
  shinyjs::removeClass("display", "active_display")
  collect_roi(0)
  refresh_display(refresh_display() + 1)
})


# Export video
shinyFiles::shinyFileSave(
  input,
  "export",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = paste0(
    default_path(),
    "/",
    tools::file_path_sans_ext(basename(video_path())),
    "_optimized.mp4"
  )
)

shiny::observeEvent(input$export, {
  path <- shinyFiles::parseSavePath(volumes, input$export)
  if (nrow(path) > 0) {
    output_path(path$datapath)
  }
})

shiny::observeEvent(output_path(), {
  if (!is.null(output_path()) & trackRcv::is_video_capture(the_video)) {
    shinyjs::showElement("curtain")
    shiny::showNotification(
      "Generating optimized video.",
      id = "optim",
      duration = NULL
    )

    the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls_x[1] - 1)

    if (input$rescale == "1:1") {
      x <- roi()[1] - 1
      y <- roi()[3] - 1
      w <- (roi()[2] - roi()[1]) + 1
      h <- (roi()[4] - roi()[3]) + 1
      proc <- the_video$read()[1][y:(y + h), x:(x + w)]
    } else {
      r <- eval(parse(text = gsub(":", "/", input$rescale)))
      x <- round((roi()[1] - 1) * r)
      y <- round((roi()[3] - 1) * r)
      w <- round(((roi()[2] - roi()[1]) + 1) * r)
      h <- round(((roi()[4] - roi()[3]) + 1) * r)
      proc <- cv2$resize(
        the_video$read()[1],
        reticulate::py_none(),
        fx = r,
        fy = r
      )[y:(y + h), x:(x + w)]
    }

    top <- ceiling((ceiling(h / 32) * 32 - h) / 2)
    bottom <- floor((ceiling(h / 32) * 32 - h) / 2)
    left <- ceiling((ceiling(w / 32) * 32 - w) / 2)
    right <- floor((ceiling(w / 32) * 32 - w) / 2)

    prepped <- cv2$copyMakeBorder(
      proc,
      as.integer(top),
      as.integer(bottom),
      as.integer(left),
      as.integer(right),
      cv2$BORDER_CONSTANT,
      NULL,
      0L
    )

    vw <- cv2$VideoWriter(
      normalizePath(
        paste0(output_path()),
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
          paste0(output_path()),
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

    the_video$set(cv2$CAP_PROP_POS_FRAMES, input$video_controls_x[1] - 1)

    for (i in seq_len(n)) {
      if (input$rescale == "1:1") {
        proc <- the_video$read()[1][y:(y + h), x:(x + w)]
      } else {
        proc <- cv2$resize(
          the_video$read()[1],
          reticulate::py_none(),
          fx = r,
          fy = r
        )[y:(y + h), x:(x + w)]
      }

      prepped <- cv2$copyMakeBorder(
        proc,
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

    output_path(NULL)
    shiny::removeNotification(id = "optim")
    shinyjs::hideElement("curtain")
  }
})
