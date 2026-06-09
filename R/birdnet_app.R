#' Run the BirdNET Analysis Shiny App
#'
#' Launches the BirdNET Analysis App bundled with the MonitoR package.
#'
#' @param display Where to display the app. One of \code{"window"} (default),
#'   \code{"viewer"} (RStudio Viewer pane), or \code{"external"} (system
#'   browser).
#' @param port Port to run the app on. Defaults to a random available port.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return Called for its side effect. Blocks the R session while the app runs.
#' @export
#'
#' @examples
#' \dontrun{
#' run_birdnet_app()                      # default: new window
#' run_birdnet_app(display = "viewer")    # RStudio Viewer pane
#' run_birdnet_app(display = "external")  # system browser
#' }
birdnet_app <- function(display = c("window", "viewer", "external"),
                            port = NULL, ...) {

  display <- match.arg(display)

  app_dir <- system.file("shiny/PAM", package = "MonitoR")
  if (!nzchar(app_dir)) {
    stop("Could not find the Shiny app directory in the MonitoR package.",
         call. = FALSE)
  }

  launcher <- switch(display,
                     window   = getOption("shiny.launch.browser", TRUE),
                     viewer   = if (requireNamespace("rstudioapi", quietly = TRUE) &&
                                    rstudioapi::isAvailable()) {
                       rstudioapi::viewer
                     } else {
                       message("RStudio not detected, falling back to 'window'.")
                       getOption("shiny.launch.browser", TRUE)
                     },
                     external = TRUE
  )

  shiny::runApp(
    appDir         = app_dir,
    launch.browser = launcher,
    port           = port,
    ...
  )
}
