.yolo_path <- function() {
  if (reticulate::virtualenv_exists("trackRai")) {
    yolo_path <- paste0(dirname(reticulate::virtualenv_python("trackRai")), "/yolo")
    if (file.exists(yolo_path)) {
      yolo_path
    } else {
      NA
    }
  } else {
    NA
  }
}

#' @export
install_yolo <- function() {
  if (!reticulate::py_available(TRUE)) {
    answer <- utils::askYesNo(
      paste0(
        "\nNo suitable Python installation was found on this system.",
        "\nPython will be installed.",
        "\nWould you like to continue?"
      )
    )

    if (!is.na(answer)) {
      if (answer) {
        reticulate::install_python(version = "3.12:latest")
      }
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  }

  if (!reticulate::py_module_available("ultralytics")) {
    answer <- utils::askYesNo(
      paste0(
        "No YOLO installation was found on this system.",
        "\nYOLO will be installed.",
        "\nWould you like to continue?"
      )
    )

    if (!is.na(answer)) {
      reticulate::virtualenv_create(
        envname = "trackRai",
        version = "3.12",
        packages = c(
          "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  }

  data.frame(
    install_path = .yolo_path(),
    version = system(paste0(.yolo_path(), " version"), intern = TRUE)
  )
}
