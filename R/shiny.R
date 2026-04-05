#' Launch the clinAnnotR Shiny app
#'
#' Opens an interactive browser-based interface for building clinical figures
#' without writing R code. Users can upload Excel files, map columns, define
#' cases and reference dates, configure lab panels, and download the finished
#' figure.
#'
#' @param ... Additional arguments forwarded to [shiny::runApp()].
#'
#' @return Called for its side effect (launches a Shiny app in the browser).
#'   Returns invisibly once the app is stopped.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to run the app. ",
      "Install it with: install.packages('shiny')",
      call. = FALSE
    )
  }
  app_dir <- system.file("shiny", "clinannotr", package = "clinAnnotR")
  if (!nzchar(app_dir)) {
    stop("Shiny app directory not found inside the clinAnnotR package.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
