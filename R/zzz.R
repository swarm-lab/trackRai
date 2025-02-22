#' @importFrom autothresholdr auto_thresh
#' @importFrom data.table fread
#' @importFrom pals alphabet
#' @importFrom plotly plot_ly 
#' @importFrom pracma inpolygon
#' @importFrom processx process
#' @importFrom shinyFiles parseFilePaths
#' @importFrom shinyWidgets verticalTabsetPanel
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs
#' @importFrom stringr str_locate_all

local <- new.env()

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  ultralytics <- reticulate::import("ultralytics", delay_load = TRUE)
  assign("ultralytics", value = ultralytics, envir = parent.env(local))

  torch <- reticulate::import("torch", delay_load = TRUE)
  assign("torch", value = torch, envir = parent.env(local))

  cv2 <- reticulate::import("cv2", convert = FALSE, delay_load = TRUE)
  assign("cv2", value = cv2, envir = parent.env(local))

  np <- reticulate::import("numpy", convert = FALSE, delay_load = TRUE)
  assign("np", value = np, envir = parent.env(local))

  base64 <- reticulate::import("base64", convert = FALSE, delay_load = TRUE)
  assign("base64", value = base64, envir = parent.env(local))
}
