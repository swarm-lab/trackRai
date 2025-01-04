.yolo_path <- function() {
  if (reticulate::condaenv_exists("trackRai")) {
    yolo_path <- paste0(dirname(reticulate::conda_python("trackRai")), "/yolo")
    if (file.exists(yolo_path)) {
      yolo_path
    } else {
      NA
    }
  } else {
    NA
  }
}

install_yolo <- function() {
  conda_installed <- !is.na(tryCatch(reticulate::conda_version(), error = function(e) NA))

  if (!conda_installed) {
    answer <- utils::askYesNo(
      paste0(
        "No conda installation was found on this system. \nMiniconda will be installed at ",
        reticulate::miniconda_path(), ". \nDo you want to continue"
      )
    )

    if (!is.na(answer)) {
      if (answer) {
        reticulate::install_miniconda()
      }
    } else {
      stop("YOLO was not installed on this system.")
    }
  }

  if (is.na(.yolo_path())) {
    answer <- utils::askYesNo(
      paste0(
        "No YOLO installation was found on this system. \nWould you like to install it now?"
      )
    )

    nvidia <- as.numeric(system("nvidia-smi --list-gpus | wc -l", intern = TRUE)) > 0
    print(nvidia)

    if (!is.na(answer)) {
      if (answer) {
        if (nvidia) {
          res <- system("nvidia-smi --version", intern = TRUE)
          v <- strsplit(res[grepl("CUDA Version", res)], " ")[[1]]

          reticulate::conda_create(
            envname = "trackRai",
            channel = c("pytorch", "nvidia", "conda-forge"),
            packages = c(
              "pytorch", "torchvision", "torchaudio",
              paste0("pytorch-cuda=", v[length(v)]), "ultralytics"
            )
          )
        } else {
          reticulate::conda_create(
            envname = "trackRai",
            channel = c("pytorch", "conda-forge"),
            packages = c("pytorch::pytorch", "torchvision", "torchaudio", "ultralytics")
          )
        }
      }
    } else {
      stop("YOLO was not installed on this system.")
    }
  } else {
    answer <- utils::askYesNo(
      paste0(
        "A YOLO installation was found on this system. \nWould you like to update it now?"
      )
    )

    if (!is.na(answer)) {
      if (answer) {
        reticulate::conda_update()
      }
    } else {
      stop("YOLO was not updated on this system.")
    }
  }

  data.frame(
    install_path = .yolo_path(),
    version = system(paste0(.yolo_path(), " version"), intern = TRUE)
  )
}
