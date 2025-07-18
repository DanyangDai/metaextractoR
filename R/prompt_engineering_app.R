# Shinyapp:prompt engineering

#' @title prompt_engineering_app
#'
#' @description A shinyapp that designed for model testing and prompt engineering.
#'
#' @examples
#'
#' \dontrun{
#' prompt_engineering_app()
#' }
#'
#' @export
prompt_engineering_app <- function(){

  app_dir <- system.file("app2.R", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}
