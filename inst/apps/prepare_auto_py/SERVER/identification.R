# Display
shiny::observeEvent(input$id_controls, {
  if (input$main == "5") {
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$rangeWidth_x, {
  if (!is.null(the_stats())) {
    dt <- the_stats()
    rw <- input$rangeWidth_x
    dt[, select_w := (width >= rw[1]) & (width <= rw[2])]
    # dt[, select := select_w & select_h]
    the_stats(dt)
  }

  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$rangeHeight_x, {
  if (!is.null(the_stats())) {
    dt <- the_stats()
    rh <- input$rangeHeight_x
    dt[, select_h := (height >= rh[1]) & (height <= rh[2])]
    # dt[, select := select_w & select_h]
    the_stats(dt)
  }

  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$shapeBuffer_x, {
  the_stats(NULL)
  shinyWidgets::updateNumericRangeInput(session, "rangeWidth_x",
    value = c(0, 0)
  )
  shinyWidgets::updateNumericRangeInput(session, "rangeHeight_x",
    value = c(0, 0)
  )
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(input$nIDFrames_x, {
  the_stats(NULL)

  if (trackRai::is_video_capture(the_video)) {
    n <- input$video_controls[3] - input$video_controls[1] + 1
    step <- floor(n / input$nIDFrames_x)
    id_frames(seq.int(input$video_controls[1], input$video_controls[3], step)[1:input$nIDFrames_x])
  }

  shinyWidgets::updateNumericRangeInput(session, "rangeWidth_x",
    value = c(0, 0)
  )
  shinyWidgets::updateNumericRangeInput(session, "rangeHeight_x",
    value = c(0, 0)
  )
  refresh_display(refresh_display() + 1)
})

shiny::observeEvent(refresh_display(), {
  if (input$main == "5") {
    to_display <<- the_image$copy()
    sc <- max(c(trackRai::n_row(to_display), trackRai::n_col(to_display)) / 720)

    if (!is.null(the_stats())) {
      dt <- the_stats()[frame == the_frame()]
      dt[, select := (select_h & select_w & mod != 2) | (mod == 1)]

      for (j in 1:nrow(dt)) {
        good <- dt[j, ]$select # & !dt[j, ]$ignore
        l <- list(
          c(dt[j, ]$x, dt[j, ]$y),
          c(dt[j, ]$width, dt[j, ]$height),
          dt[j, ]$angle
        )
        box <- cv2$boxPoints(reticulate::r_to_py(l))
        box <- np$int_(box)
        cv2$drawContours(
          to_display, list(box), 0L, c(255L, 255L, 255),
          as.integer(max(1, round(2 * sc)))
        )
        cv2$drawContours(
          to_display, list(box), 0L,
          if (good) c(0L, 224L, 0L) else c(5L, 80L, 255L),
          as.integer(max(1, round(sc)))
        )
      }

      shinyjs::addClass("display", "active_display")
    } else {
      background <- the_background$copy()
      if (input$dark_button_x == "Darker") {
        background <- cv2$bitwise_not(background)
      }

      mask <- cv2$compare(the_mask, 0, 1L)
      mask <- cv2$divide(mask, 255)

      frame <- the_image$copy()

      if (input$dark_button_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$dark_button_x == "A bit of both") {
        dif <- cv2$absdiff(frame, background)
      } else {
        dif <- cv2$subtract(frame, background)
      }

      dif <- cv2$multiply(dif, mask)
      dif_gray <- cv2$cvtColor(dif, cv2$COLOR_BGR2GRAY)
      bw <- cv2$compare(dif_gray, input$threshold_x, 2L)
      k <- np$ones(c(3L, 3L), dtype = np$uint8)
      k[1, 1] <- 0L
      neighbors <- cv2$filter2D(bw, -1L, k)
      bw <- np$where((bw == 255) & (neighbors > 0), 255, 0)$astype(np$uint8)
      bw <- cv2$Canny(bw, 100, 200)
      k <- cv2$getStructuringElement(
        cv2$MORPH_RECT,
        as.integer(c(
          input$shapeBuffer_x * 2,
          input$shapeBuffer_x * 2
        )) + 1L
      )
      bw <- cv2$dilate(bw, k)

      cc <- cv2$connectedComponentsWithStats(bw)
      nz <- cv2$findNonZero(cc[1])
      labs <- cc[1][cc[1]$nonzero()]
      ulabs <- np$unique(labs)

      to_display <<- the_image$copy()
      sc <- max(c(trackRai::n_row(to_display), trackRai::n_col(to_display)) / 720)

      for (j in seq_along(ulabs)) {
        bb <- reticulate::py_to_r(cc[2][j])
        valid <- bb[5] > 4

        if (valid) {
          ell_py <- cv2$fitEllipse(nz[labs == ulabs[j - 1]])
          box <- cv2$boxPoints(ell_py)
          box <- np$int_(box)
          cv2$drawContours(
            to_display, list(box), 0L, c(255L, 255L, 255),
            as.integer(max(1, round(2 * sc)))
          )
          cv2$drawContours(
            to_display, list(box), 0L,
            c(5L, 80L, 255L),
            as.integer(max(1, round(sc)))
          )
        }
      }
      
      shinyjs::removeClass("display", "active_display")
    }

    print_display(print_display() + 1)
  }
})


# UI
shiny::observe({
  if (is.null(the_stats())) {
    toggleTabs(6, "OFF")
    toggled_tabs$toggled[6] <<- FALSE
  } else {
    if (toggled_tabs$toggled[6] == FALSE) {
      toggleTabs(6, "ON")
      toggled_tabs$toggled[6] <<- TRUE
    }
  }
})

shiny::observeEvent(input$autoSelect_x, {
  if (input$autoSelect_x) {
    shinyjs::disable("rangeWidth_x")
    shinyjs::disable("rangeHeight_x")
    refresh_stats(refresh_stats() + 1)
  } else {
    shinyjs::enable("rangeWidth_x")
    shinyjs::enable("rangeHeight_x")
  }
})


# Compute object statistics
shiny::observeEvent(input$computeStats_x, {
  if (trackRai::is_video_capture(the_video) & trackRai::is_image(the_background) & trackRai::is_image(the_mask)) {
    shinyjs::showElement("curtain")
    shiny::showNotification("Detecting objects.",
      id = "stats",
      duration = NULL
    )

    frame_pos <- id_frames()

    pb <- shiny::Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- the_background$copy()
    if (input$dark_button_x == "Darker") {
      background <- cv2$bitwise_not(background)
    }

    mask <- cv2$compare(the_mask, 0, 1L)
    mask <- cv2$divide(mask, 255)

    obb <- list()
    the_subs <<- list()
    the_submasks <<- list()

    for (i in 1:n) {
      the_video$set(cv2$CAP_PROP_POS_FRAMES, frame_pos[i] - 1)
      frame <- the_video$read()[1]

      if (input$dark_button_x == "Darker") {
        frame <- cv2$bitwise_not(frame)
      }

      if (input$dark_button_x == "A bit of both") {
        dif <- cv2$absdiff(frame, background)
      } else {
        dif <- cv2$subtract(frame, background)
      }

      dif <- cv2$multiply(dif, mask)
      dif_gray <- cv2$cvtColor(dif, cv2$COLOR_BGR2GRAY)
      bw <- cv2$compare(dif_gray, input$threshold_x, 2L)
      k <- np$ones(c(3L, 3L), dtype = np$uint8)
      k[1, 1] <- 0L
      neighbors <- cv2$filter2D(bw, -1L, k)
      bw <- np$where((bw == 255) & (neighbors > 0), 255, 0)$astype(np$uint8)
      bw <- cv2$Canny(bw, 100, 200)
      k <- cv2$getStructuringElement(
        cv2$MORPH_RECT,
        as.integer(c(
          input$shapeBuffer_x * 2,
          input$shapeBuffer_x * 2
        )) + 1L
      )
      bw <- cv2$dilate(bw, k)

      cc <- cv2$connectedComponentsWithStats(bw)
      nz <- cv2$findNonZero(cc[1])
      labs <- cc[1][cc[1]$nonzero()]
      ulabs <- np$unique(labs)

      for (j in seq_along(ulabs)) {
        bb <- reticulate::py_to_r(cc[2][j])
        valid <- bb[5] > 4

        if (valid) {
          ell <- cv2$fitEllipse(nz[labs == ulabs[j - 1]])
          tmp <- as.list(unlist(reticulate::py_to_r(ell)))
          l <- length(obb) + 1
          obb[[l]] <- data.table::as.data.table(tmp)
          obb[[l]]$frame <- frame_pos[i]

          sub <- dif[bb[2]:(bb[2] + bb[4]), bb[1]:(bb[1] + bb[3])]
          submask <- cv2$cvtColor(
            cv2$compare(cc[1][bb[2]:(bb[2] + bb[4]), bb[1]:(bb[1] + bb[3])], 0, 1L), cv2$COLOR_GRAY2BGR
          )
          the_submasks <<- c(the_submasks, submask)
          the_subs <<- c(the_subs, cv2$multiply(sub, cv2$divide(cv2$compare(submask, 0, 1L), 255)))
        }
      }

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

    dt <- data.table::rbindlist(obb)
    names(dt) <- c("x", "y", "width", "height", "angle", "frame")

    if (input$autoSelect_x == TRUE) {
      d <- sqrt((dt$height - median(dt$height))^2 +
        (dt$width - median(dt$width))^2)
      k <- kmeans(log(d + 1), 2)
      ix <- which.min(k$centers)
      rw <- round(range(dt$width[k$cluster == ix], na.rm = TRUE))
      dt[, select_w := (width >= rw[1]) & (width <= rw[2])]
      rh <- round(range(dt$height[k$cluster == ix], na.rm = TRUE))
      dt[, select_h := (height >= rh[1]) & (height <= rh[2])]
      shinyWidgets::updateNumericRangeInput(session, "rangeWidth_x", value = rw)
      shinyWidgets::updateNumericRangeInput(session, "rangeHeight_x", value = rh)
    } else {
      rw <- round(range(dt$width[k$cluster == ix], na.rm = TRUE))
      dt[, select_w := (width >= rw[1]) & (width <= rw[2])]
      rh <- round(range(dt$height[k$cluster == ix], na.rm = TRUE))
      dt[, select_h := (height >= rh[1]) & (height <= rh[2])]
    }

    # dt[, select := select_w & select_h]
    # dt[, ignore := FALSE]
    dt[, mod := 0]
    the_stats(dt)

    pb$close()

    shiny::removeNotification(id = "stats")
    shinyjs::hideElement("curtain")
  }
})

shiny::observeEvent(refresh_stats(), {
  if (!is.null(the_stats())) {
    if (input$autoSelect_x == TRUE) {
      dt <- the_stats()
      d <- sqrt((dt$height - median(dt$height))^2 +
        (dt$width - median(dt$width))^2)
      k <- kmeans(log(d + 1), 2)
      ix <- which.min(k$centers)
      shinyWidgets::updateNumericRangeInput(session, "rangeWidth_x",
        value = round(range(dt$width[k$cluster == ix], na.rm = TRUE))
      )
      shinyWidgets::updateNumericRangeInput(session, "rangeHeight_x",
        value = round(range(dt$height[k$cluster == ix], na.rm = TRUE))
      )
    }
  }
})

.point_in_rectangle <- function(x, y, rect) {
  l <- list(c(rect[1], rect[2]), c(rect[3], rect[4]), rect[5])
  box <- reticulate::py_to_r(cv2$boxPoints(reticulate::r_to_py(l)))
  pracma::inpolygon(x, y, box[, 1], box[, 2], TRUE)
}

# shinyjs::onevent("click", "display_img", function(props) {
#   if (input$main == "5" & !is.null(the_stats())) {
#     px <- trackRai::n_col(to_display) * (props$offsetX / input$display_img_width)
#     py <- trackRai::n_row(to_display) * (props$offsetY / input$display_img_height)

#     dt <- the_stats()
#     in_rect <- dt[frame == the_frame(),
#       .(test = .point_in_rectangle(px, py, unlist(.SD))),
#       by = .I, .SDcols = c("x", "y", "width", "height", "angle")
#     ]

#     if (any(in_rect$test)) {
#       ix <- which(in_rect$test)

#       for (i in ix) {
#         mod <- dt[frame == the_frame()]$mod[i]
#         if (mod == 0) {
#           if (dt[frame == the_frame()]$select_w[i] & dt[frame == the_frame()]$select_h[i]) {
#             dt[frame == the_frame()]$mod[i] <- 2
#           } else {
#             dt[frame == the_frame()]$mod[i] <- 1
#           }
#         } else {
#           if (mod == 1) {
#             dt[frame == the_frame()]$mod[i] <- 2
#           } else {
#             dt[frame == the_frame()]$mod[i] <- 1
#           }
#         }
#       }
#     }

#     the_stats(dt)
#     refresh_display(refresh_display() + 1)
#   }
# })


# Display statistics
output$stats <- plotly::renderPlotly(
  if (!is.null(the_stats()) & refresh_display() > 1) {
    dt <- the_stats()
    dt[, select := (select_h & select_w & mod != 2) | (mod == 1)]
    plotly::plot_ly(dt,
      x = ~height, y = ~width, color = ~select,
      type = "scatter", mode = "markers",
      colors = c("#FF5005", "#00e000")
    ) %>%
      plotly::layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height"),
        yaxis = list(title = "Width"),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = FALSE)
  } else {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        margin = list(l = 0, r = 0, t = 0, b = 0, pad = 5),
        xaxis = list(title = "Height", range = c(0, 1), dtick = 0.25),
        yaxis = list(title = "Width", range = c(0, 1), dtick = 0.25)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }
)

output$blob_stats <- shiny::renderUI({
  if (!is.null(the_stats()) & refresh_display() > 1) {
    shiny::tagList(
      shiny::hr(),
      shiny::p("Number of detected objects: ", shiny::strong(nrow(the_stats()))),
      shiny::p("Number of selected objects: ", shiny::strong(sum(the_stats()$select)))
    )
  }
})
