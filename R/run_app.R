#' Launch the epimodFBAfunctions GUI
#' @param launch.browser Whether to open in external browser
#' @export
run_app <- function(launch.browser = TRUE) {
  # path all’app dentro il package
  app_dir <- system.file("app", package = "epimodFBAfunctionsGUI")
  if (app_dir == "")
    stop("App directory not found, reinstall package.", call. = FALSE)

  # salva il WD originale (quello da cui chiami run_app)
  user_proj <- normalizePath(getwd())
  # lo esponiamo via opzioni di Shiny
  options(epimodFBAfunctionsGUI.user_proj = user_proj)

  shiny::runApp(app_dir, launch.browser = launch.browser)
}

