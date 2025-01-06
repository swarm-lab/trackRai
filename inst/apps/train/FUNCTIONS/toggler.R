# Toggle UI on and off during long operations
toggleInputs <- function(input, state = "OFF") {
  input_list <- reactiveValuesToList(input)
  to_toggle <- grepl("_x", names(input_list))
  input_list <- input_list[to_toggle]

  for (name in names(input_list)) {
    if (state == "OFF") {
      shinyjs::disable(name)
    } else {
      shinyjs::enable(name)
    }
  }
}

toggleTabs <- function(tabs = NULL, state = "OFF") {
  tab_list <- paste0("[data-value='", tabs, "']")

  for (tab in tabs) {
    if (state == "OFF") {
      shinyjs::disable(selector = paste0("[data-value='", tab, "']"))
    } else {
      shinyjs::enable(selector = paste0("[data-value='", tab, "']"))
    }
  }
}

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

rectPoints <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  x <- (x1 + x2 + x3 + x4) / 4
  y <- (y1 + y2 + y3 + y4) / 4  
  width <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  height <- sqrt((x3 - x2)^2 + (y3 - y2)^2)
    angle <- atan2(y2 - y1, x2 - x1) * (180 / pi)
  
  data.table::data.table(x = x, y = y, height = height, width = width, angle = angle)
}