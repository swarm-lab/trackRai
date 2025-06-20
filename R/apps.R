#' @title App Launcher
#'
#' @description This function starts an app launcher that gives the user access
#'  to all the Shiny apps provided with trackRai. 
#'
#' @param ... Parameters to be passed to [shiny::runApp()].
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso [shiny::runApp()]
#'
#' @examples
#' \dontrun{
#' trackRai()
#' }
#'
#' @export
trackRai <- function(...) {
  if (yolo_installed()) {
    shiny::shinyOptions(shiny.launch.browser = list(...)$launch.browser)
    shiny::runApp(paste0(find.package("trackRai"), "/apps/launcher"), ...)
  } else {
    stop("YOLO was not detected. Install it with `trackRai::install_yolo()`.")
  }
}
