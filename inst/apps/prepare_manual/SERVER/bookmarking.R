# Globals and reactives
do_bookmark_exclude <- shiny::reactiveVal(0)


# Reset state
shiny::observeEvent(input$reset, {
  url <- paste0(
    "http://",
    session$clientData$url_hostname,
    ":",
    session$clientData$url_port
  )
  js$replace(url)
})


# Save state
shiny::observeEvent(do_bookmark_exclude, {
  shiny::setBookmarkExclude(
    c(
      session$getBookmarkExclude(),
      names(input)[!grepl("*_x", names(input))]
    )
  )
})

shinyFiles::shinyFileSave(
  input,
  "save_state",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$save_state, {
  if (length(input$save_state) > 1) {
    session$doBookmark()
  }
})

shiny::onBookmark(function(state) {
  if (!is.null(default_root())) {
    state$values$default_root <- default_root()
  }
  if (nchar(default_path()) > 0) {
    state$values$default_path <- default_path()
  }
  if (!is.null(video_path())) {
    state$values$video_path <- video_path()
  }
  if (!is.null(obb)) {
    state$values$obb <- obb
  }
  if (length(tags()) > 0) {
    state$values$tags <- tags()
  }
})

shiny::onBookmarked(function(url) {
  state <- sub(".*(\\?_inputs_)", "", url)
  state_path <- shinyFiles::parseSavePath(volumes, input$save_state)
  saveRDS(state, state_path$datapath)
})


# Load state
shinyFiles::shinyFileChoose(
  input,
  "load_state",
  roots = volumes,
  session = session,
  defaultRoot = default_root(),
  defaultPath = default_path()
)

shiny::observeEvent(input$load_state, {
  settings_path <- shinyFiles::parseFilePaths(volumes, input$load_state)
  if (nrow(settings_path) > 0) {
    state <- tryCatch(readRDS(settings_path$datapath), error = function(e) NA)
    if (!is.na(state)) {
      url <- paste0(
        "http://",
        session$clientData$url_hostname,
        ":",
        session$clientData$url_port,
        "/?_inputs_",
        state
      )
      js$replace(url)
    }
  }
})

shiny::onRestore(function(state) {
  if (!is.null(state$values$default_root)) {
    default_root(state$values$default_root)
  }
  if (!is.null(state$values$default_path)) {
    default_path(state$values$default_path)
  }
  if (!is.null(state$values$video_path)) {
    video_path(state$values$video_path)
    refresh_video(refresh_video() + 1)
  }
  if (!is.null(state$values$obb)) {
    obb <<- data.table::as.data.table(state$values$obb)
  }
  if (!is.null(state$values$tags)) {
    tags(as.data.frame(state$values$tags))
  }
})

shiny::onRestored(function(state) {
  if (length(tags()) > 0) {
    shiny::updateSelectizeInput(
      session,
      "object_tag_x",
      choices = tags()$label
    )
  }
})
