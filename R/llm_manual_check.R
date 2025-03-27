#' Launch the first shinyapp for to look into what variables are avaiable in the abstract.
#'
#'
#' @description This function calls a shinyapp that display the abstract and allow for randomly reading in data to glace what are the avaiable information in abstract.
#'
#' @return Shiny app.
#'
#' @export

llm_manual_check <- function(){
  app_dir <- system.file("shiny-app3", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}
