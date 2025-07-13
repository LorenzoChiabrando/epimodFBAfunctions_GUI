#' Launch the epimodFBAfunctions GUI
#' @param launch.browser Whether to open in external browser
#' @export
run_app <- function(launch.browser = TRUE) {
  app_dir <- system.file("app", package = "epimodFBAfunctionsGUI")
  if (app_dir == "") stop("App directory not found, reinstall package.", call. = FALSE)
  shiny::runApp(app_dir, launch.browser = launch.browser)
}

