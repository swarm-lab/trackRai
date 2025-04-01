.yolo_path <- function() {
  if (reticulate::virtualenv_exists("trackRai")) {
    paths <- list.files(
      dirname(reticulate::virtualenv_python("trackRai")),
      full.names = TRUE
    )
    test <- grepl("yolo", paths)

    if (any(test)) {
      paths[test]
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
#'  all its Python dependencies in a dedicated Python virtual environment for
#'  use with the \link{trackRai} apps.
#'
#' @param python_version A character string indicating the version of Python you
#'  would like YOLO to run on (default: "3.12.5"). YOLO is currently compatible
#'  with Python 3.8.0 to 3.12.8. Not all versions of Python will necessarily
#'  work on your system, but the chosen default works on most systems that
#'  we tested so far.
#'
#' @param cuda_win_version Windows-only. A character string indicating the
#'  version of CUDA installed on the computer. Valid values are: auto (the
#'  function tries to determine automatically the version of CUDA, the default),
#'  NA (YOLO will be installed without CUDA support), 11.8, 12.4, and 12.6. All
#'  other values will throw an error. Ignored on Mac and Linux computers
#'
#' @return If the installation/update completes successfully, a data frame
#'  indicating the location of the YOLO installation and its version number.
#'
#' @note
#' If the requested version of Python is not activated on your system, this
#'  function will attempt to install it first before creating the dedicated
#'  Python virtual environment.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso [remove_yolo()]
#'
#' @export
install_yolo <- function(python_version = "3.12.5", cuda_win_version = "auto") {
  if (reticulate::virtualenv_exists("trackRai")) {
    reticulate::use_virtualenv("trackRai", required = TRUE)
  } else if (reticulate::virtualenv_exists("r-reticulate")) {
    reticulate::use_virtualenv("r-reticulate", required = TRUE)
  }

  reticulate::py_available(TRUE)

  if (is.null(reticulate::py_discover_config())) {
    py_installed <- FALSE
  } else if (!grepl(python_version, reticulate::py_discover_config()$version_string)) {
    py_installed <- FALSE
  } else {
    py_installed <- TRUE
  }

  if (!py_installed) {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nThe Python version found on this system is not the requested one.",
        "\nPython ", python_version, " will be installed.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------",
        "\n"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::install_python(version = python_version)
    } else {
      warning("\nContinuing installation of YOLO with a non-requested version of Python.\n")
    }
  }

  if (Sys.info()[["sysname"]] == "Windows") {
    if (!is.na(cuda_win_version)) {
      if (cuda_win_version == "auto") {
        nvcc <- system("nvcc --version", intern = TRUE)
        cuda_win_version <- substr(
          gsub("[^0-9]", "", nvcc[grepl("Cuda compilation tools", nvcc)]), 1, 3
        )
      } else {
        cuda_win_version <- gsub("\\.", "", cuda_win_version)
      }

      if (cuda_win_version %in% c("118", "124", "126")) {
        pip_options <- paste0("--index-url https://download.pytorch.org/whl/cu", cuda_win_version)
      } else {
        stop("Incompatible CUDA version.")
      }
    } else {
      pip_options <- character()
    }
  } else {
    pip_options <- character()
  }

  if (!reticulate::virtualenv_exists("trackRai")) {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nNo trackRai environment was found on this system.",
        "\nIt will be created with all the necessary packages.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------",
        "\n"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_create(
        envname = "trackRai",
        version = python_version,
      )
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "torch", "torchvision", "torchaudio"
        ),
        pip_options = pip_options
      )
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "opencv-python", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.\n")
    }
  } else if (!any(grepl("yolo", list.files(dirname(reticulate::virtualenv_python("trackRai")))))) {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nNo YOLO installation was found on this system.",
        "\nIt will be installed with all the necessary dependencies.",
        "\nWould you like to continue?",
        "\n",
        "\n------------------------------------------------------------",
        "\n"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "torch", "torchvision", "torchaudio"
        ),
        pip_options = pip_options
      )
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "opencv-python", "ultralytics", "lap"
        )
      )
    } else {
      stop("\nYOLO was not installed on this system.\n")
    }
  } else {
    answer <- utils::askYesNo(
      paste0(
        "\n------------------------------------------------------------",
        "\n",
        "\nYOLO is already installed on this system.",
        "\nWould you like to try updating it?",
        "\n",
        "\n------------------------------------------------------------",
        "\n"
      )
    )

    if (is.na(answer)) {
      answer <- FALSE
    }

    if (answer) {
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "pip"
        ),
        pip_options = "--upgrade"
      )
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "torch", "torchvision", "torchaudio"
        ),
        pip_options = paste0("--upgrade ", pip_options)
      )
      reticulate::virtualenv_install(
        envname = "trackRai",
        packages = c(
          "opencv-python", "ultralytics", "lap"
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


#' @title Remove YOLO
#'
#' @description This function automates the removal of YOLO and all its Python
#'  dependencies from your system.
#'
#' @return Nothing.
#'
#' @note
#' The function will only remove the dedicated Python virtual environment from
#'  your system. If Python was installed during the execution of
#'  [install_yolo()], it will not be removed.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso [install_yolo()]
#'
#' @export
remove_yolo <- function() {
  reticulate::virtualenv_remove(envname = "trackRai")
}


#' @title Detect YOLO Installation
#'
#' @description This function detects whether YOLO is correctly installed on the
#'  system.
#'
#' @return A logical indicating the presence or absence of YOLO.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso [install_yolo()]
#'
#' @export
yolo_installed <- function() {
  !is.na(.yolo_path())
}