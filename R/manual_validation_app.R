# Shinyapp: manual validation

#' @title manual_validation_app
#'
#' @description A shinyapp that manually check the LLM outputs
#'
#' @examples
#'
#' \dontrun{
#' manual_validation_app()
#' }
#'
#' @export
manual_validation_app <- function(){

  app_dir <- system.file("app3.R", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}

