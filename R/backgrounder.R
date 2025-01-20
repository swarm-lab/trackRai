#' @title Reconstruct the Background of a Video
#'
#' @description This function attempts to reconstruct the background of a video
#'  that was filmed from a fixed point of view.
#'
#' @param video A Python/OpenCV VideoCapture object.
#'
#' @param n The number of images of the video that will be used to reconstruct
#'  the background.
#'
#' @param method The name of a method to reconstruct the background. There are
#'  currently 4 methods available: "mean" (uses the average value of each pixel
#'  as background), "median" (uses the median value of each pixel as background), 
#'  "min" (uses the minimum value of each pixel as background), "max" (uses the 
#'  maximum value of each pixel as background), and "quant" (uses an arbitrary 
#'  quantile value of each pixel as background).
#'
#' @param prob If \code{method = "quant"}, the quantile value to use.
#'
#' @param start,end The start and end frames of the video to use for the
#'  background reconstruction. If not set, the first and last frames will be
#'  used.
#'
#' @return A Python/Numpy array.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
backgrounder <- function(video, n = 10, method = "median", prob = 0.025,
                         start = NULL, end = NULL) {
  if (!inherits(video, c("cv2.VideoCapture", "python.builtin.object"))) {
    stop("This is not a Python/OpenCV VideoCapture object.")
  }

  n_frames <- reticulate::py_to_r(video$get(cv2$CAP_PROP_FRAME_COUNT))
  if (n > n_frames) {
    stop("n should be smaller than the total number of frames in the video.")
  }

  if (is.null(start)) {
    start <- 1
  }

  if (is.null(end)) {
    end <- n_frames - 1
  }

  if (shiny::isRunning()) {
    shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)
  } else {
    message("Loading images in memory.")
  }

  frames <- as.integer(seq.int(start, end, length.out = n))

  l <- lapply(frames, function(i) {
    video$set(cv2$CAP_PROP_POS_FRAMES, i-1)
    reticulate::py_to_r(video$read()[1]) * 1.0
  })

  if (shiny::isRunning()) {
    shiny::removeNotification(id = "load")
    shiny::showNotification("Calculating background.", id = "calc", duration = NULL)
  } else {
    message("Calculating background.")
  }

  med <- array(0.0, dim = dim(l[[1]]))

  for (i in 1:3) {
    if (shiny::isRunning()) {
      shiny::showNotification(paste0("Processing ", c("red", "green", "blue")[i], " channel."),
        id = "layer", duration = NULL
      )
    } else {
      message(paste0("Processing ", c("red", "green", "blue")[i], " channel."))
    }

    if (method == "mean") {
      med[, , i] <- Rfast::rowmeans(sapply(l, function(x) x[][, , i]))
    } else if (method == "median") {
      med[, , i] <- Rfast::rowMedians(sapply(l, function(x) x[][, , i]))
    } else if (method == "min") {
      med[, , i] <- Rfast::rowMins(sapply(l, function(x) x[][, , i]), value = TRUE)
    } else if (method == "max") {
      med[, , i] <- Rfast::rowMaxs(sapply(l, function(x) x[][, , i]), value = TRUE)
    } else if (method == "quant") {
      med[, , i] <- Rfast2::rowQuantile(sapply(l, function(x) x[][, , i]), probs = prob)
    } else {
      stop("'method' should be one of 'mean', 'median', 'min', 'max', or 'quant'")
    }

    if (shiny::isRunning()) {
      shiny::removeNotification(id = "layer")
    }
  }
  mode(med) <- "integer"
  out <- reticulate::r_to_py(med)

  if (shiny::isRunning()) {
    shiny::removeNotification(id = "calc")
  }

  out
}
