# Shinyapp: manual validation

#' @title manual_validation_app
#'
#' @description A shinyapp that manually check the LLM outputs
#'
#' @examples
#' \donotrun {
#' prompt_engineering()
#' }
#' @export
#'
manual_validation_app <- function(){

  app_dir <- system.file("shiny-app3", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}

