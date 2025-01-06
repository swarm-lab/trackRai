verticalTabPanel(
  title = "2",
  box_height = "100%",
  p("Checking module", class = "module-title"),
  hr(),
  sliderInput("frame_x", "Frame", 0, 1, 0, 1),
  hr(),
  numericInput(
    "maxObjects_x", "Maximum number of objects to detect:",
    300, 1, NA, 1, "100%"
  ),
  hr()
)
