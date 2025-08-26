# Globals and reactives
scale_coords <- NULL

scale_px <- shiny::reactiveVal()
scale_real <- shiny::reactiveVal()
unit_real <- shiny::reactiveVal()
collect_scale <- shiny::reactiveVal(0)
stop_scale_collection <- shiny::reactiveVal(0)
scale_modal <- shiny::reactiveVal(0)
origin <- shiny::reactiveVal(c(1, 1))
collect_origin <- shiny::reactiveVal(0)
stop_origin_collection <- shiny::reactiveVal(0)


# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "2") {
    to_display <<- the_image$copy()
    sc <- max(c(n_row(to_display), n_col(to_display)) / 720)
    r <- 0.01 * min(n_row(to_display), n_col(to_display))

    if (!is.null(origin())) {
      x <- origin()[1]
      y <- origin()[2]

      .drawPolyLine(
        to_display,
        rbind(c(x, y + 50), c(x, y), c(x + 50, y)),
        closed = FALSE,
        color = c(255, 255, 255),
        contrast = c(0, 0, 0),
        thickness = as.integer(max(1.5, round(sc))),
        outline = as.integer(max(1.5, round(sc)))
      )

      .drawCircle(
        to_display,
        origin()[1],
        origin()[2],
        radius = r,
        color = c(255, 255, 255),
        contrast = c(0, 0, 0),
        thickness = as.integer(max(1.5, round(sc)))
      )
    }

    if (!is.null(scale_coords)) {
      if (nrow(scale_coords) > 1) {
        .drawPolyLine(
          to_display,
          scale_coords,
          closed = FALSE,
          color = c(255, 255, 255),
          contrast = c(0, 0, 0),
          thickness = as.integer(max(1.5, round(sc))),
          outline = as.integer(max(1.5, round(sc)))
        )
      }

      for (i in seq_len(nrow(scale_coords))) {
        .drawCircle(
          to_display,
          scale_coords[i, 1],
          scale_coords[i, 2],
          radius = r,
          color = c(255, 255, 255),
          contrast = c(0, 0, 0),
          thickness = as.integer(max(1.5, round(sc)))
        )
      }
    }

    print_display(print_display() + 1)
  }
})

output$scale_status <- shiny::renderUI({
  if (!is.null(origin)) {
    origin_st <- paste0(
      "Origin: [",
      round(origin()[1]),
      ",",
      round(origin()[2]),
      "]. "
    )
  } else {
    origin_st <- "No set origin (optional). "
  }

  scale_st <- "No set scale (optional)."

  if (!is.null(scale_px()) & !is.null(scale_real())) {
    if (!is.na(scale_real())) {
      scale_st <- paste0(
        "1 ",
        unit_real(),
        " = ",
        round(scale_px() / scale_real(), 2),
        " pixels."
      )
    }
  }

  shiny::p(paste0(origin_st, scale_st), style = "text-align: center;")
})

shinyjs::onevent("click", "display_img", function(props) {
  px <- trackRcv::n_col(to_display) *
    ((props$offsetX -
      (input$display_img_uiwidth - input$display_img_imgwidth) / 2) /
      input$display_img_imgwidth)
  py <- trackRcv::n_row(to_display) *
    (props$offsetY / input$display_img_imgheight)

  if (collect_origin() > 0) {
    origin(c(px, py))
    stop_origin_collection(stop_origin_collection() + 1)
    refresh_display(refresh_display() + 1)
  } else if (collect_scale() > 0) {
    scale_coords <<- rbind(scale_coords, c(px, py))

    if (nrow(scale_coords) >= 2) {
      stop_scale_collection(stop_scale_collection() + 1)
    }

    refresh_display(refresh_display() + 1)
  }
})


# Origin
shiny::observeEvent(input$origin_button, {
  if (is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1:5, "OFF")
    shiny::showNotification(
      "Select a point to set the origin.",
      id = "origin_notif",
      duration = NULL,
      type = "message"
    )
    shinyjs::addClass("display", "active_display")
    collect_origin(1)
  }
})

shiny::observeEvent(stop_origin_collection(), {
  if (collect_origin() > 0) {
    shiny::removeNotification(id = "origin_notif")
    .toggleInputs(input, "ON")
    .toggleTabs(1:5, "ON")
    shinyjs::removeClass("display", "active_display")
    collect_origin(0)
    refresh_display(refresh_display() + 1)
  }
})

# Scale
shiny::observeEvent(input$scale_button, {
  if (is_image(the_image)) {
    .toggleInputs(input, "OFF")
    .toggleTabs(1:5, "OFF")
    shiny::showNotification(
      "Select 2 reference points for calculating the scale.",
      id = "scale_notif",
      duration = NULL,
      type = "message"
    )
    scale_coords <<- NULL
    shinyjs::addClass("display", "active_display")
    collect_scale(1)
  }
})

shiny::observeEvent(stop_scale_collection(), {
  if (collect_scale() > 0) {
    scale_px(sqrt(diff(scale_coords[, 1])^2 + diff(scale_coords[, 2])^2))
    shiny::removeNotification(id = "scale_notif")
    .toggleInputs(input, "ON")
    .toggleTabs(1:5, "ON")
    shinyjs::removeClass("display", "active_display")
    collect_scale(0)
    scale_modal(scale_modal() + 1)
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(scale_modal(), {
  if (!is.null(scale_px()) & scale_modal() > 0) {
    units <- c("µm", "mm", "cm", "dm", "m", "km", "parsec")

    shiny::showModal(
      shiny::modalDialog(
        title = "Set scale",
        easyClose = TRUE,

        shiny::tags$table(
          style = "width: 100%;",
          shiny::tags$tr(
            shiny::tags$td(
              shiny::numericInput(
                "scale_real_x",
                "Distance between the 2 reference points",
                NA,
                0,
                Inf,
                width = "100%"
              )
            ),
            shiny::tags$td(style = "width: 10px;"),
            shiny::tags$td(
              shiny::selectInput(
                "unit_real_x",
                "Unit",
                units,
                selected = "cm",
                width = "100px"
              ),
              style = "padding-top: 4px;"
            )
          )
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("scale_set", "Set Scale")
        )
      )
    )
  }
})

shiny::observeEvent(input$scale_set, {
  shiny::removeModal(session)
})

shiny::observeEvent(input$scale_real_x, {
  scale_real(input$scale_real_x)
})

shiny::observeEvent(input$unit_real_x, {
  unit_real(input$unit_real_x)
})
