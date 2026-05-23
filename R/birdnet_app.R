#' Launch shiny app
#' @export
birdnet_app <- function() {
  app_dir <- system.file("shiny/birdnet_analysis", package = "MonitoR")
  suppressMessages(shiny::runApp(app_dir, launch.browser = TRUE, quiet = T))
}

