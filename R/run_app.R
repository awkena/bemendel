#' Launch the bemendel Shiny app.
#'
#' @description An interface function to launch a user-friendly Shiny app for
#' making genetic crosses in R.
#'
#' @concept "Mendelian genetics in R"
#'
#' @export
#'
#' @import shiny
#' @import reactable
#' @importFrom shinyjs useShinyjs
#' @import htmltools
#' @import bubbles
#' @import argonR
#' @import argonDash

run_app <- function() {
  # Find and launch the app
  appDir <- system.file("shinyapps", paste0("bemendel"), package = "bemendel")
  shiny::runApp(appDir, launch.browser = TRUE)
}

