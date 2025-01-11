# File manager 
volumes <- c(Home = fs::path_home(), getVolumes()())
defaultRoot <- reactiveVal()
defaultPath <- reactiveVal("")
theBackgroundPath <- reactiveVal()
theMaskPath <- reactiveVal()
theYOLOPath <- reactiveVal()
tmpDir <- tempdir()

# UI
hideElement("curtain")
toggledTabs <- data.frame(
  tab = 1:6,
  toggled = c(TRUE, rep(FALSE, 5))
)

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
toDisplay <- NULL
refreshDisplay <- reactiveVal(0)
printDisplay <- reactiveVal(0)

# Video 
theVideoPath <- reactiveVal()
theVideo <- NULL
theFrame <- reactiveVal()
refreshVideo <- reactiveVal(0)
rangeMem <- c(NA, NA)
frameMem <- NA

# Image
theImage <- NULL

# Background
theBackground <- NULL
refreshBackground <- reactiveVal(0)
collectGhost <- reactiveVal(0)
stopGhostCollection <- reactiveVal(0)
ghostCoords <- NULL

# Mask
theMask <- NULL
refreshMask <- reactiveVal(0)
collectMask <- reactiveVal(0)
stopMaskCollection <- reactiveVal(0)
maskCoords <- NULL

# Objects
theStats <- reactiveVal()
refreshStats <- reactiveVal(0)
subs <- list()
submasks <- list()
theComposite <- NULL
