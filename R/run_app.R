#' Launch the multiROCplot Shiny application
#'
#' Starts the interactive Shiny interface bundled with the package. The app is a
#' thin UI layer over the exported functions ([read_data()],
#' [compute_univariate_tests()], [compute_roc_table()], [plot_roc_curves()],
#' [plot_group_boxplots()]), so the interactive tool and the programmatic API
#' share a single implementation.
#'
#' @param ... Additional arguments passed to [shiny::runApp()] (e.g. `port`,
#'   `launch.browser`, `host`).
#'
#' @return Called for its side effect of starting the app; does not return a
#'   meaningful value.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny-app", package = "multiROCplot")
  if (identical(app_dir, "")) {
    stop("Could not find the Shiny app directory. Try re-installing multiROCplot.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
