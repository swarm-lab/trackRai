local <- new.env()

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  ultralytics <- reticulate::import("ultralytics", delay_load = TRUE)
  assign("ultralytics", value = ultralytics, envir = parent.env(local))

  torch <- reticulate::import("torch", delay_load = TRUE)
  assign("torch", value = torch, envir = parent.env(local))

  cv2 <- reticulate::import("cv2", convert = FALSE, delay_load = TRUE)
  assign("cv2", value = cv2, envir = parent.env(local))
}
