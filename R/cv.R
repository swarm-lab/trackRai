#' @export
is_video_capture <- function(x) {
  inherits(x, c("cv2.VideoCapture", "python.builtin.object"))
}

#' @export
n_frames <- function(x) {
  if (is_video_capture(x)) {
    reticulate::py_to_r(x$get(cv2$CAP_PROP_FRAME_COUNT))
  } else {
    NA
  }
}

#' @export 
read_frame <- function(x, i) {
  if (is_video_capture(x)) {
    x$set(cv2$CAP_PROP_POS_FRAMES, i-1)
    x$read()[1]
  } else {
    stop("This is not a Python/OpenCV VideoCapture object.")
  }
}

#' @export
is_image <- function(x) {
  inherits(x, c("numpy.ndarray", "python.builtin.object"))
}

#' @export
n_row <- function(x) {
  if (is_image(x)) {
    reticulate::py_to_r(x$shape[0])
  } else {
    NULL
  }
}

#' @export
n_col <- function(x) {
  if (is_image(x)) {
    reticulate::py_to_r(x$shape[1])
  } else {
    NULL
  }
}