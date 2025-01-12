# UI
shiny::observeEvent(refreshMask(), {
  if (!trackRai::is_image(theMask)) {
    toggleTabs(4:6, "OFF")
    toggledTabs$toggled[4:6] <<- FALSE
  } else {
    if (toggledTabs$toggled[4] == FALSE) {
      toggleTabs(4, "ON")
      toggledTabs$toggled[4] <<- TRUE
    }
  }
})


# Events
shinyFiles::shinyFileChoose(input, "maskFile_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$maskFile_x, {
  path <- shinyFiles::parseFilePaths(volumes, input$maskFile_x)
  if (nrow(path) > 0) {
    theMaskPath(normalizePath(path$datapath, mustWork = FALSE))
    refreshMask(refreshMask() + 1)
  }
})

shiny::observeEvent(refreshMask(), {
  if (refreshMask() > 0) {
    toCheck <- cv2$imread(theMaskPath())

    if (trackRai::is_image(toCheck)) {
      if (!all(unlist(reticulate::py_to_r(toCheck$shape)) == unlist(reticulate::py_to_r(theImage$shape)))) {
        shinyalert::shinyalert("Error:",
          "The video and mask do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        theMask <<- NULL
      } else {
        theMask <<- toCheck$copy()
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(theMaskPath(), volumes),
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
      dir <- dirname(theMaskPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))

      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

shiny::observeEvent(refreshDisplay(), {
  if (input$main == "3") {
    if (!trackRai::is_image(theMask) & trackRai::is_image(theImage)) {
      theMask <<- reticulate::np_array(
        array(1L, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 3)),
        dtype = "uint8"
      )
    }

    if (trackRai::is_image(theMask)) {
      gray <- cv2$cvtColor(theMask, cv2$COLOR_BGR2GRAY)
      green <- gray$copy()
      green[green > 0] <- 255L
      red <- cv2$bitwise_not(green)
      blue <- np_array(array(0, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 1)), "uint8")

      toDisplay <<- cv2$addWeighted(cv2$merge(c(blue, green, red)), 0.25, theImage, 0.75, 0.0)

      sc <- max(c(trackRai::n_row(toDisplay), trackRai::n_col(toDisplay)) / 720)
      r <- 0.01 * min(trackRai::n_row(toDisplay), trackRai::n_col(toDisplay))
      font_scale <- as.integer(sc)
      font_thickness <- as.integer(max(1, 1.5 * sc))

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
        toDisplay <<- cv2$add(toDisplay, cv2$cvtColor(m, cv2$COLOR_GRAY2BGR))

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
  
          toDisplay <<- cv2$putText(
            toDisplay, lab, as.integer(c(x, y)), cv2$FONT_HERSHEY_SIMPLEX,
            font_scale, c(255L, 255L, 255L), font_thickness, cv2$LINE_AA
          )
        }
      }

      if (collectMask() == 1) {
        if (nrow(maskCoords) > 1) {
          cv2$polylines(
            toDisplay, array(as.integer(maskCoords), c(1, dim(maskCoords))),
            TRUE, c(255, 255, 255), as.integer(max(1, 1.5 * sc))
          )
        }
      }

      if (collectMask() > 0) {
        for (i in seq_len(nrow(maskCoords))) {
          cv2$circle(
            toDisplay, as.integer(maskCoords[i, ]), as.integer(r * 1.5),
            c(255L, 255L, 255L), -1L
          )
          cv2$circle(
            toDisplay, as.integer(maskCoords[i, ]), as.integer(r),
            c(0L, 0L, 255L), -1L
          )
        }
      }
    } else {
      toDisplay <<- black_screen$copy()
    }

    printDisplay(printDisplay() + 1)
  }
})

shiny::observeEvent(input$polyButton_x, {
  if (trackRai::is_image(theMask)) {
    toggleInputs(input, "OFF")
    toggleTabs(1:2, "OFF")
    shiny::showNotification("Click to draw the polygonal ROI. Return to stop.
      Esc to cancel.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )

    collectMask(1)
  }
})

shiny::observeEvent(input$ellButton_x, {
  if (trackRai::is_image(theMask)) {
    toggleInputs(input, "OFF")
    toggleTabs(1:2, "OFF")
    shiny::showNotification("Click to select 5 points along the periphery of the
      ellipse/circle ROI. Esc to cancel.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )

    collectMask(2)
  }
})

shinyjs::onevent("click", "displayImg", function(props) {
  if (collectMask() > 0) {
    x <- trackRai::n_col(toDisplay) * (props$offsetX / input$displayImg_width)
    y <- trackRai::n_row(toDisplay) * (props$offsetY / input$displayImg_height)
    maskCoords <<- rbind(maskCoords, c(x, y))

    if (collectMask() == 2 & nrow(maskCoords) >= 5) {
      stopMaskCollection(stopMaskCollection() + 1)
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(input$retKey, {
  if (collectMask() > 0) {
    stopMaskCollection(stopMaskCollection() + 1)
  }
})

shiny::observeEvent(input$escKey, {
  if (collectMask() > 0) {
    maskCoords <<- NULL
    stopMaskCollection(stopMaskCollection() + 1)
  }
})

shiny::observeEvent(stopMaskCollection(), {
  if (collectMask() > 0) {
    if (!is.null(maskCoords)) {
      if (collectMask() == 1) {
        if (nrow(maskCoords) > 2) {
          polyMask <- reticulate::np_array(
            array(0L, c(trackRai::n_row(theMask), trackRai::n_col(theMask), 3)),
            dtype = "uint8"
          )
          cv2$fillPoly(
            polyMask,
            pts = array(as.integer(maskCoords), c(1, dim(maskCoords))),
            color = c(255, 255, 255)
          )
          if (input$incButton_x == "Including") {
            theMask[polyMask > 0] <<- as.integer(input$roi_x)
          } else if (input$incButton_x == "Excluding") {
            theMask[polyMask > 0] <<- 0L
          }
        }
      } else if (collectMask() == 2) {
        if (nrow(maskCoords) == 5) {
          ellMask <- reticulate::np_array(
            array(0L, c(trackRai::n_row(theMask), trackRai::n_col(theMask), 3)),
            dtype = "uint8"
          )
          ell <- trackRai::optimEllipse(maskCoords[, 1], maskCoords[, 2])
          ellMask <- cv2$ellipse(
            ellMask, as.integer(c(ell[1], ell[2])), as.integer(c(ell[3], ell[4]) / 2),
            ell[5], 0, 360, c(255L, 255L, 255L), -1L
          )
          if (input$incButton_x == "Including") {
            theMask[ellMask > 0] <<- as.integer(input$roi_x)
          } else if (input$incButton_x == "Excluding") {
            theMask[ellMask > 0] <<- 0L
          }
        }
      }
    }

    removeNotification(id = "mask_notif")
    toggleInputs(input, "ON")
    toggleTabs(1:2, "ON")
    collectMask(0)
    maskCoords <<- NULL
    refreshDisplay(refreshDisplay() + 1)
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
  if (trackRai::is_image(theMask)) {
    theMask <<- reticulate::np_array(
      array(1L, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 3)),
      dtype = "uint8"
    )
    refreshDisplay(refreshDisplay() + 1)
  }
})

shiny::observeEvent(input$excludeAll_x, {
  if (trackRai::is_image(theMask)) {
    theMask <<- reticulate::np_array(
      array(0L, c(trackRai::n_row(theImage), trackRai::n_col(theImage), 3)),
      dtype = "uint8"
    )
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFiles::shinyFileSave(input, "saveMask_x",
  roots = volumes, session = session,
  defaultRoot = defaultRoot(), defaultPath = defaultPath()
)

shiny::observeEvent(input$saveMask_x, {
  path <- shinyFiles::parseSavePath(volumes, input$saveMask_x)

  if (trackRai::is_image(theMask) & nrow(path) > 0) {
    path <- normalizePath(path$datapath, mustWork = FALSE)
    cv2$imwrite(path, theMask)
    theMaskPath(path)
  }
})
