#' Launch the first shinyapp for to look into what variables are available in the abstract.
#'
#'
#' @description This function calls a shinyapp that display the abstract and allow for randomly reading in data to glace what are the available information in abstract.
#'
#' @return Shiny app.
#'
#' @export
glance_manual_app <- function(){
  app_dir <- system.file("app1.R", package = "metaextractoR")

  shiny::runApp(app_dir, display.mode = "normal")
}
