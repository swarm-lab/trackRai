install_yolo <- function(nvidia = FALSE) {
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

    if (!is.na(answer)) {
      if (answer) {
        if (nvidia) {
          conda_create(
            envname = "trackRai",
            channel = c("pytorch", "nvidia", "conda-forge"),
            packages = c("pytorch-cuda=11.8", "torchvision", "torchaudio", "ultralytics")
          )
        } else {
          conda_create(
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

.yolo_path <- function() {
  if (reticulate::condaenv_exists("trackRai")) {
    yolo_path <- paste0(dirname(conda_python("trackRai")), "/yolo")
    if (file.exists(yolo_path)) {
      yolo_path
    } else {
      NA
    }
  } else {
    NA
  }
}

yolo_train <- function(dataset, epochs = 100) {
  mem_folder <- getwd()
  setwd(dataset)

  background <- Rvision::image("background.png")
  n_gpus <- as.numeric(system("nvidia-smi --list-gpus | wc -l", intern = TRUE))

  if (n_gpus > 1) {
    dump <- system(
      paste0(
        .yolo_path(), " obb train data=dataset.yaml", 
        " model=yolo11m-obb.pt epochs=", epochs, 
        " imgsz=", ncol(background), " batch=8 single_cls=True",
        paste0(" device=", paste0((1:n_gpus) - 1, collapse = ","))
      )
    )
  } else {
    dump <- system(
      paste0(
        .yolo_path(), " obb train data=dataset.yaml", 
        " model=yolo11m-obb.pt epochs=", epochs, 
        " imgsz=", ncol(background), " batch=-1 single_cls=True"
      )
    )
  }

  setwd(mem_folder)
}
