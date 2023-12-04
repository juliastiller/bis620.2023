#' @title Run Shiny App
#' @description Launches the Clinical Trials Query Shiny app.
#' @export
run_shiny_app <- function() {
  shiny::runApp(system.file("shiny", package = "bis620.2023"))
}
