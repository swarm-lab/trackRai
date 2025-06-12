# Display
shiny::observeEvent(refresh_display(), {
  if (input$main == "3") {
    if (!is_image(the_mask) & is_image(the_background)) {
      the_mask <<- reticulate::np_array(
        array(1L, c(n_row(the_background), n_col(the_background), 3)),
        dtype = "uint8"
      )
    }

    if (is_image(the_mask)) {
      gray <- cv2$cvtColor(the_mask, cv2$COLOR_BGR2GRAY)
      green <- gray$copy()
      green[green > 0] <- 255L
      red <- cv2$bitwise_not(green)
      blue <- np_array(array(0, c(n_row(the_background), n_col(the_background), 1)), "uint8")

      to_display <<- cv2$addWeighted(cv2$merge(c(blue, green, red)), 0.25, the_background, 0.75, 0.0)

      sc <- max(c(n_row(to_display), n_col(to_display)) / 720)
      r <- 0.01 * min(n_row(to_display), n_col(to_display))
      font_scale <- as.integer(max(1, round(sc)))
      font_thickness <- as.integer(max(2, round(1.5 * sc)))

      k1 <- cv2$getStructuringElement(cv2$MORPH_CROSS, c(5L, 5L))
      k2 <- cv2$getStructuringElement(
        cv2$MORPH_CROSS,
        c(as.integer(2 + max(1, 0.5 * sc)), as.integer(2 + max(1, 0.5 * sc)))
      )

      h <- np$bincount(gray$ravel(), minlength = 256L)
      h[0] <- 0L
      vals <- reticulate::py_to_r(np$where(h))[[1]]

      for (i in seq_along(vals)) {
        bw <- (gray == vals[i])$astype("uint8")
        green <- bw * 255L
        red <- cv2$bitwise_not(green)
        m1 <- cv2$dilate(green, k1)
        m2 <- cv2$dilate(red, k1)
        m <- cv2$dilate(cv2$bitwise_and(m1, m2), k2)
        to_display <<- cv2$add(to_display, cv2$cvtColor(m, cv2$COLOR_GRAY2BGR))

        cc <- cv2$connectedComponents(bw)[1]
        n <- reticulate::py_to_r(cc$max())

        for (j in seq_len(n)) {
          indices <- np$where(cc == j)
          lab <- as.character(vals[i])
          lab_size <- reticulate::py_to_r(
            cv2$getTextSize(
              lab, cv2$FONT_HERSHEY_SIMPLEX, font_scale, font_thickness
            )[0]
          )
          x <- reticulate::py_to_r(indices[1]$mean()) - lab_size[[1]] / 2
          y <- reticulate::py_to_r(indices[0]$mean()) + lab_size[[2]] / 2

          to_display <<- cv2$putText(
            to_display, lab, as.integer(c(x, y)), cv2$FONT_HERSHEY_SIMPLEX,
            font_scale, c(255L, 255L, 255L), font_thickness, cv2$LINE_AA
          )
        }
      }

      if (collect_mask() == 1) {
        if (nrow(mask_coords) > 1) {
          cv2$polylines(
            to_display, array(as.integer(mask_coords), c(1, dim(mask_coords))),
            TRUE, c(255, 255, 255), as.integer(max(1, 1.5 * sc))
          )
        }
      }

      if (collect_mask() > 0) {
        for (i in seq_len(nrow(mask_coords))) {
          cv2$circle(
            to_display, as.integer(mask_coords[i, ]), as.integer(r * 1.5),
            c(255L, 255L, 255L), -1L
          )
          cv2$circle(
            to_display, as.integer(mask_coords[i, ]), as.integer(r),
            c(0L, 0L, 255L), -1L
          )
        }
      }
    } else {
      to_display <<- black_screen$copy()
    }

    print_display(print_display() + 1)
  }
})


# UI
shiny::observeEvent(refresh_display(), {
  if (!is_image(the_mask)) {
    toggleTabs(4:6, "OFF")
    toggled_tabs$toggled[4:6] <<- FALSE
  } else {
    if (toggled_tabs$toggled[4] == FALSE) {
      toggleTabs(4, "ON")
      toggled_tabs$toggled[4] <<- TRUE
    }
  }
})


# Load existing mask
shinyFiles::shinyFileChoose(input, "maskFile_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$maskFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$maskFile_x)
  if (nrow(path) > 0) {
    mask_path(normalizePath(path$datapath, mustWork = FALSE))
    refresh_mask(refresh_mask() + 1)
  }
})

shiny::observeEvent(refresh_mask(), {
  if (refresh_mask() > 0) {
    to_check <- cv2$imread(mask_path())

    if (is_image(to_check)) {
      if (!all(unlist(reticulate::py_to_r(to_check$shape)) == unlist(reticulate::py_to_r(the_background$shape)))) {
        shinyalert::shinyalert("Error:",
          "The video and mask do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        the_mask <<- NULL
      } else {
        the_mask <<- to_check$copy()
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(mask_path(), stringr::fixed(sapply(volumes, normalizePath))),
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

      refresh_display(refresh_display() + 1)
    }
  }
})


# Create mask
shiny::observeEvent(input$polyButton_x, {
  if (is_image(the_mask)) {
    toggleInputs(input, "OFF")
    toggleTabs(1:2, "OFF")
    shiny::showNotification("Click to draw the polygonal ROI. Return to stop.
      Esc to cancel.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_mask(1)
  }
})

shiny::observeEvent(input$ellButton_x, {
  if (is_image(the_mask)) {
    toggleInputs(input, "OFF")
    toggleTabs(1:2, "OFF")
    shiny::showNotification("Click to select 5 points along the periphery of the
      ellipse/circle ROI. Esc to cancel.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )

    shinyjs::addClass("display", "active_display")
    collect_mask(2)
  }
})

shiny::observeEvent(input$retKey, {
  if (collect_mask() > 0) {
    stop_mask_collection(stop_mask_collection() + 1)
  }
})

shiny::observeEvent(input$escKey, {
  if (collect_mask() > 0) {
    mask_coords <<- NULL
    stop_mask_collection(stop_mask_collection() + 1)
  }
})

shiny::observeEvent(stop_mask_collection(), {
  if (collect_mask() > 0) {
    if (!is.null(mask_coords)) {
      if (collect_mask() == 1) {
        if (nrow(mask_coords) > 2) {
          polyMask <- reticulate::np_array(
            array(0L, c(n_row(the_mask), n_col(the_mask), 3)),
            dtype = "uint8"
          )
          cv2$fillPoly(
            polyMask,
            pts = array(as.integer(mask_coords), c(1, dim(mask_coords))),
            color = c(255, 255, 255)
          )
          if (input$incButton_x == "Including") {
            the_mask[polyMask > 0] <<- as.integer(input$roi_x)
          } else if (input$incButton_x == "Excluding") {
            the_mask[polyMask > 0] <<- 0L
          }
        }
      } else if (collect_mask() == 2) {
        if (nrow(mask_coords) == 5) {
          ellMask <- reticulate::np_array(
            array(0L, c(n_row(the_mask), n_col(the_mask), 3)),
            dtype = "uint8"
          )
          ell <- optim_ellipse(mask_coords[, 1], mask_coords[, 2])
          ellMask <- cv2$ellipse(
            ellMask, as.integer(c(ell[1], ell[2])), as.integer(c(ell[3], ell[4]) / 2),
            ell[5], 0, 360, c(255L, 255L, 255L), -1L
          )
          if (input$incButton_x == "Including") {
            the_mask[ellMask > 0] <<- as.integer(input$roi_x)
          } else if (input$incButton_x == "Excluding") {
            the_mask[ellMask > 0] <<- 0L
          }
        }
      }
    }

    removeNotification(id = "mask_notif")
    toggleInputs(input, "ON")
    toggleTabs(1:2, "ON")
    shinyjs::removeClass("display", "active_display")
    collect_mask(0)
    mask_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$incButton_x, {
  if (input$incButton_x == "Including") {
    shinyjs::enable("roi_x")
  } else {
    shinyjs::disable("roi_x")
  }
})

shiny::observeEvent(input$includeAll_x, {
  if (is_image(the_mask)) {
    the_mask <<- reticulate::np_array(
      array(1L, c(n_row(the_background), n_col(the_background), 3)),
      dtype = "uint8"
    )
    refresh_display(refresh_display() + 1)
  }
})

shiny::observeEvent(input$excludeAll_x, {
  if (is_image(the_mask)) {
    the_mask <<- reticulate::np_array(
      array(0L, c(n_row(the_background), n_col(the_background), 3)),
      dtype = "uint8"
    )
    refresh_display(refresh_display() + 1)
  }
})


# Save mask
shinyFiles::shinyFileSave(input, "save_mask_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

shiny::observeEvent(input$save_mask_x, {
  path <- shinyFiles::parseSavePath(volumes, input$save_mask_x)

  if (is_image(the_mask) & nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    cv2$imwrite(path, the_mask)
    mask_path(path)
  }
})
