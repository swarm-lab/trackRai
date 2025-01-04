yolo_installed <- !is.na(.yolo_path())
reticulate::use_condaenv("trackRai")
torch <- reticulate::import("torch")
n_gpus <- torch$cuda$device_count()
theRawProgress <- reactiveVal()
theProgress <- reactiveVal()
monitorProgress <- reactiveVal(FALSE)


# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
theYOLOPath <- reactiveVal()
theTempFile <- NULL


# UI
hideElement("curtain")
