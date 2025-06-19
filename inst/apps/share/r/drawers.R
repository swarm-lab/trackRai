.shades <- grDevices::col2rgb(pals::alphabet())[3:1, ]
mode(.shades) <- "integer"

.drawText <- function(
  img,
  txt,
  x,
  y,
  scale = 1,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  cv2$putText(
    img,
    as.character(txt),
    as.integer(c(x, y)),
    cv2$FONT_HERSHEY_SIMPLEX,
    scale,
    as.integer(contrast),
    as.integer(max(1, round(2 * thickness))),
    cv2$LINE_AA
  )

  cv2$putText(
    img,
    as.character(txt),
    as.integer(c(x, y)),
    cv2$FONT_HERSHEY_SIMPLEX,
    scale,
    as.integer(color),
    as.integer(max(1, round(thickness))),
    cv2$LINE_AA
  )
  NULL
}

.drawTag <- function(
  img,
  txt,
  x,
  y,
  scale = 1,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  txt_size <- reticulate::py_to_r(
    cv2$getTextSize(
      as.character(txt),
      cv2$FONT_HERSHEY_SIMPLEX,
      scale,
      as.integer(max(1, round(2 * thickness)))
    )
  )

  .drawText(
    img, 
    txt,
    (x - txt_size[[1]][[1]] / 2) + 2,
    (y + txt_size[[1]][[2]] / 2) - 1,
    scale,
    color,
    contrast,
    thickness
  )
  NULL
}

.drawCircle <- function(
  img,
  x,
  y,
  radius = 1,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  cv2$circle(
    img,
    as.integer(c(x, y)),
    as.integer(radius),
    as.integer(contrast),
    as.integer(max(1, round(2 * thickness)))
  )

  cv2$circle(
    img,
    as.integer(c(x, y)),
    as.integer(radius),
    as.integer(color),
    -1L
  )
  NULL
}

.drawPolyLine <- function(
  img,
  m,
  closed = FALSE,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  cv2$polylines(
    img,
    array(as.integer(m), c(1, dim(m))),
    closed,
    as.integer(contrast),
    as.integer(max(1, round(2 * thickness)))
  )

  cv2$polylines(
    img,
    array(as.integer(m), c(1, dim(m))),
    closed,
    as.integer(color),
    as.integer(max(1, round(thickness)))
  )
  NULL
}

.drawContour <- function(
  img,
  ct,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  cv2$drawContours(
    img,
    ct,
    -1L,
    as.integer(contrast),
    as.integer(max(1, round(2 * thickness)))
  )

  cv2$drawContours(
    img,
    ct,
    -1L,
    as.integer(color),
    as.integer(max(1, round(thickness)))
  )
  NULL
}

.drawBox <- function(
  img,
  x,
  y,
  width,
  height,
  angle,
  color = c(255, 255, 255),
  contrast = c(0, 0, 0),
  thickness = 1
) {
  box <- cv2$boxPoints(reticulate::r_to_py(list(
    c(x, y),
    c(width, height),
    angle
  )))
  box <- np$int_(box)

  cv2$drawContours(
    img,
    list(reticulate::r_to_py(box)),
    -1L,
    as.integer(contrast),
    as.integer(max(1, round(2 * thickness)))
  )

  cv2$drawContours(
    img,
    list(reticulate::r_to_py(box)),
    -1L,
    as.integer(color),
    as.integer(max(1, round(thickness)))
  )
  NULL
}

.point_in_rectangle <- function(x, y, rect) {
  l <- list(c(rect[1], rect[2]), c(rect[3], rect[4]), rect[5])
  box <- reticulate::py_to_r(cv2$boxPoints(reticulate::r_to_py(l)))
  pracma::inpolygon(x, y, box[, 1], box[, 2], TRUE)
}