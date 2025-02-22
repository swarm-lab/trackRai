#' @title Dataset Preparation
#'
#' @description This function launches a Shiny app to help with the preparation
#'  of a training dataset for YOLO.
#'
#' @param ... Parameters to be passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' prepare()
#' }
#'
#' @export
prepare <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/prepare_auto_py"), ...)
}
# prepare <- function(auto = TRUE, ...) {
#   if (auto) {
#     shiny::runApp(paste0(find.package("trackRai"), "/apps/prepare_auto_py"), ...)
#   } else {
#     shiny::runApp(paste0(find.package("trackRai"), "/apps/prepare_manual_py"), ...)
#   }
# }


#' @title Model Training
#'
#' @description This function launches a Shiny app to help with the training of 
#'  a YOLO model.
#'
#' @param ... Parameters to be passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' train()
#' }
#'
#' @export
train <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/train_py"), ...)
}


#' @title Video Tracking
#'
#' @description This function launches a Shiny app to help with the tracking of 
#'  objects in a video using a trained YOLO model. 
#'
#' @param ... Parameters to be passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' track()
#' }
#'
#' @export
track <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/track_py"), ...)
}


#' @title Track Visualization
#'
#' @description This function launches a Shiny app to help with the 
#'  visualization the tracking results obtained with \link{track}.
#'
#' @param ... Parameters to be passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' visualize()
#' }
#'
#' @export
visualize <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/visualize_py"), ...)
}
