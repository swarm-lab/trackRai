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

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::install_python(version = "3.12:latest")
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  }

  if (!reticulate::virtualenv_exists("trackRai")) {
    answer <- utils::askYesNo(
      paste0(
        "\nNo trackRai environment was found on this system.",
        "\nIt will be created with all the necessary packages.",
        "\nWould you like to continue?"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_create(
        envname = "trackRai",
        version = "3.12",
        packages = c(
          "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  } else if (!file.exists(paste0(dirname(reticulate::virtualenv_python("trackRai")), "/yolo"))) {
    answer <- utils::askYesNo(
      paste0(
        "\nNo YOLO installation was found on this system.",
        "\nIt will be installed with all the necessary dependencies.",
        "\nWould you like to continue?"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  } else {
    answer <- utils::askYesNo(
      paste0(
        "\nYOLO is already installed on this system.",
        "\nWould you like to try updating it?"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        ),
        pip_options = "--upgrade"
      )
    }
  }

  data.frame(
    install_path = .yolo_path(),
    version = system(paste0(.yolo_path(), " version"), intern = TRUE)
  )
}
