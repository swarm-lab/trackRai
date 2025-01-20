# File manager 
volumes <- c(Home = fs::path_home(), getVolumes()())
defaultRoot <- shiny::reactiveVal()
defaultPath <- shiny::reactiveVal("")
theBackgroundPath <- shiny::reactiveVal()
theMaskPath <- shiny::reactiveVal()
theYOLOPath <- shiny::reactiveVal()
tmpDir <- tempdir()

# UI
shinyjs::hideElement("curtain")
toggledTabs <- data.frame(
  tab = 1:6,
  toggled = c(TRUE, rep(FALSE, 5))
)

# Display
black_screen <- reticulate::r_to_py(
  array(0L, c(1080, 1920, 3))
)
toDisplay <- NULL
refreshDisplay <- shiny::reactiveVal(0)
printDisplay <- shiny::reactiveVal(0)

# Video 
theVideoPath <- shiny::reactiveVal()
theVideo <- NULL
theFrame <- shiny::reactiveVal()
refreshVideo <- shiny::reactiveVal(0)
rangeMem <- c(NA, NA)
frameMem <- NA

# Image
theImage <- NULL

# Background
theBackground <- NULL
refreshBackground <- shiny::reactiveVal(0)
collectGhost <- shiny::reactiveVal(0)
stopGhostCollection <- shiny::reactiveVal(0)
ghostCoords <- NULL

# Mask
theMask <- NULL
refreshMask <- shiny::reactiveVal(0)
collectMask <- shiny::reactiveVal(0)
stopMaskCollection <- shiny::reactiveVal(0)
maskCoords <- NULL

# Objects
theStats <- shiny::reactiveVal()
refreshStats <- shiny::reactiveVal(0)
subs <- list()
submasks <- list()
theComposite <- NULL
