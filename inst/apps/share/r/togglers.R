.toggleInputs <- function(input, state = "OFF") {
  input_list <- shiny::reactiveValuesToList(input)
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

.toggleTabs <- function(tabs = NULL, state = "OFF") {
  tab_list <- paste0("[data-value='", tabs, "']")

  for (tab in tabs) {
    if (state == "OFF") {
      shinyjs::disable(selector = paste0("[data-value='", tab, "']"))
    } else {
      shinyjs::enable(selector = paste0("[data-value='", tab, "']"))
    }
  }
}