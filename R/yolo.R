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


#' @title Install and Update YOLO
#'
#' @description This function automates the installation/updating of YOLO and 
#'  all its Python dependencies in a dedicated Python environment for use with 
#'  the \link{trackRai} apps.
#'
#' @return If the installation/update completes successfully, a data frame 
#'  indicating the location of the YOLO installation and its version number.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
install_yolo <- function() {
  if (reticulate::py_discover_config()$version != "3.12.8") {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nThe Python version found on this system is not the recommended one.",
        "\nPython 3.12.8 will be installed.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::install_python(version = "3.12.8")
    } else {
      warning("\nContinuing installation of YOLO with a non-recommended version of Python.")
    }
  }

  reticulate::py_available(TRUE)

  if (!reticulate::virtualenv_exists("trackRai")) {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nNo trackRai environment was found on this system.",
        "\nIt will be created with all the necessary packages.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_create(
        envname = "trackRai",
        version = "3.12.8",
        packages = c(
          "numpy", "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  } else if (!file.exists(paste0(dirname(reticulate::virtualenv_python("trackRai")), "/yolo"))) {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nNo YOLO installation was found on this system.",
        "\nIt will be installed with all the necessary dependencies.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "numpy", "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.")
    }
  } else {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nYOLO is already installed on this system.",
        "\nWould you like to try updating it?",
        "\n",
        "\n------------------------------------------------------------"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "numpy", "opencv-python", "torch", "torchvision", "torchaudio", "ultralytics", "lap"
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
