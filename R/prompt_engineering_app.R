# Shinyapp:prompt engineering

#' @title prompt_engineering_app
#'
#' @description This Shiny app is designed for model testing and prompt engineering. It uses the training set—with manually extracted values from   `glance_manual_app()` —as the reference for training and evaluation. When run, the function automatically creates a log_files folder in your current working directory. After you exit the prompt_engineering_app, a CSV file is saved in this folder, containing details of each prompt tested, results, runtime, model type, and accuracy. This log supports model selection, documentation, and reproducibility. The chosen model and prompt can be used for the `process_with_ollama()` function.
#'
#' @examples
#' if (interactive()) {
#'  prompt_engineering_app()
#' }
#' @returns Runs a shiny app.
#' @export
prompt_engineering_app <- function() {
  app_dir <- system.file("shiny-app2/app.R", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}
