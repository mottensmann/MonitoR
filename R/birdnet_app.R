#' Launch shiny app
#' @export
birdnet_app <- function() {
  app_dir <- system.file("shiny/birdnet_analysis", package = "MonitoR")
  shiny::runApp(app_dir, launch.browser = TRUE)
}


#Add a file browser input (using shinyFiles) so users can pick the data path interactively instead of typing it.
#Add a species filter to the detection table that syncs with the taxon selector for the activity plot.
