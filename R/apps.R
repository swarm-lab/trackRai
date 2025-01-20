#' @export
prepare <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/prepare_py"), ...)
}

#' @export
train <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/train_py"), ...)
}

#' @export
track <- function(...) {
  shiny::runApp(paste0(find.package("trackRai"), "/apps/track_py"), ...)
}
