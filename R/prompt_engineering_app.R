# Shinyapp:prompt engineering

#' @title prompt_engineering_app
#'
#' @description A shinyapp that designed for model testing and prompt engineering.
#'
#' @examples
#' \donotrun {
#' prompt_engineering()
#' }
#' @export
prompt_engineering_app <- function(){

  app_dir <- system.file("shiny-app2", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}
