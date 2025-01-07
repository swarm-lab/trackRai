# YOLO
reticulate::use_condaenv("trackRai")
yolo_installed <- !is.na(.yolo_path())
torch <- reticulate::import("torch")
n_gpus <- torch$cuda$device_count()
mps <- torch$backends$mps$is_available()
yolo_proc <- NULL
theModel <- reactiveVal()

# File manager
volumes <- c(Home = fs::path_home(), getVolumes()())
theYOLOPath <- reactiveVal()
theTempFile <- NULL

# Progress monitoring
theRawProgress <- reactiveVal()
theProgress <- reactiveVal()
monitorProgress <- reactiveVal(FALSE)

# UI
hideElement("curtain")
toggleTabs(2, "OFF")

# Chech
theVideo <- reactiveVal()
refreshFrame <- reactiveVal(0)
theFrame <- NULL
theTempFrame <- NULL
theTempPredict <- NULL