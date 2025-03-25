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
install_yolo <- function(python_version = "3.12.5") {
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
        )
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
        )
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
        pip_options = "--upgrade"
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