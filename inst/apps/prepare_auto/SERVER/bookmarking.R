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
  if (!is.null(background_path())) {
    state$values$background_path <- background_path()
  }
  if (!is.null(mask_path())) {
    state$values$mask_path <- mask_path()
  }
  if (!is.null(yolo_path())) {
    state$values$yolo_path <- yolo_path()
  }
  state$values$video_range <- video_range
})

shiny::onBookmarked(function(url) {
  state_bookmark <- sub(".*(\\?_inputs_)", "", url)
  state_path <- shinyFiles::parseSavePath(volumes, input$save_state)
  save(state_bookmark, file = state_path$datapath)
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
    load(settings_path$datapath)
    if (!is.null(state_bookmark)) {
      url <- paste0(
        "http://",
        session$clientData$url_hostname,
        ":",
        session$clientData$url_port,
        "/?_inputs_",
        state_bookmark
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
  if (!is.null(state$values$background_path)) {
    background_path(state$values$background_path)
    refresh_background(refresh_background() + 1)
  }
  if (!is.null(state$values$mask_path)) {
    mask_path(state$values$mask_path)
    refresh_mask(refresh_mask() + 1)
  }
  if (!is.null(state$values$yolo_path)) {
    yolo_path(state$values$yolo_path)
  }
  if (!is.null(state$values$video_range)) {
    video_range <<- state$values$video_range
  }
})
