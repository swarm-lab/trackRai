drawBoxes <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    shade <- pals::alphabet()[(BY$id %% length(col)) + 1]
    box <- reticulate::r_to_py(
      simplify2array(
        list(
          as.matrix(SD[, c("x1", "x2", "x3", "x4")]),
          as.matrix(SD[, c("y1", "y2", "y3", "y4")])
        )
      )
    )
    cv2$drawContours(
      img, list(np$int_(box)), 0L, 
      as.integer(col2rgb(shade, FALSE)[3:1, , drop = FALSE]),
      as.integer(linewidth)
    )
  }
  NULL
}

drawTracks <- function(img, SD, BY, linewidth = 1L) {
  if (nrow(SD) > 0) {
    shade <- pals::alphabet()[(BY$id %% length(col)) + 1]
    trace <- reticulate::r_to_py(as.matrix(SD))
    cv2$polylines(
      img, list(np$int_(trace)), 0L, 
      as.integer(col2rgb(shade, FALSE)[3:1, , drop = FALSE]),
      as.integer(linewidth)
    )
  }
  NULL
}
