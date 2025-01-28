#' @title Test for a Python VideoCapture Object
#'
#' @description This function tests whether an object is a Python VideoCapture 
#'  object.
#'
#' @param x Any R object.
#'
#' @return A logical indicating whether the object is a Python VideoCapture 
#'  object (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
is_video_capture <- function(x) {
  inherits(x, c("cv2.VideoCapture", "python.builtin.object"))
}


#' @title The Number of Frames in a Python VideoCapture Object
#'
#' @description This function returns the number of frames present in a Python 
#'  VideoCapture object.
#'
#' @param x A Python VideoCapture object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
n_frames <- function(x) {
  if (is_video_capture(x)) {
    reticulate::py_to_r(x$get(cv2$CAP_PROP_FRAME_COUNT))
  } else {
    NA
  }
}


#' @title Read Specific Frame in a Python VideoCapture Object
#'
#' @description This function reads a specific frame of a Python VideoCapture 
#'  object and returns it as Numpy array.
#' 
#' @param x A Python VideoCapture object.
#' 
#' @param i The 1-indexed frame to be read. 
#'
#' @return A Numpy array.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export 
read_frame <- function(x, i) {
  if (is_video_capture(x)) {
    x$set(cv2$CAP_PROP_POS_FRAMES, i-1)
    x$read()[1]
  } else {
    stop("This is not a Python/OpenCV VideoCapture object.")
  }
}


#' @title Test for a Numpy Array
#'
#' @description This function tests whether an object is a Numpy array.
#' 
#' @param x Any R object.
#'
#' @return A logical indicating whether the object is a Numpy array (TRUE) or 
#'  not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export 
is_image <- function(x) {
  inherits(x, c("numpy.ndarray", "python.builtin.object"))
}


#' @title The Number of Rows in a Numpy Array
#'
#' @description This function returns the number of rows present in a Numpy 
#'  array.
#'
#' @param x A Numpy array.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
n_row <- function(x) {
  if (is_image(x)) {
    reticulate::py_to_r(x$shape[0])
  } else {
    NULL
  }
}


#' @title The Number of Columns in a Numpy Array
#'
#' @description This function returns the number of columns present in a Numpy 
#'  array.
#'
#' @param x A Numpy array.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
#' @export
n_col <- function(x) {
  if (is_image(x)) {
    reticulate::py_to_r(x$shape[1])
  } else {
    NULL
  }
}