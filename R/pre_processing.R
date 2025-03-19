# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples


add_predefined_vars <- function(abstract_data,list_vars){

  if (is.null(abstract_data)){
    warning("Please check that you have supplied a valid dataset")
  }
  else if (is.null(list_vars)) {
    warning("Make sure that you put in a list of variables")
  }
  else{
  new_col_names <- c(paste0(list_vars, "_manual"), paste0(list_vars, "_llm"))

  abstract_data[new_col_names] <- NA

  return(abstract_data)}

}

save_stage_1_data <- function(abstract_data) {

  dir.create(metaextroctor_process_data, recursive = TRUE)

  file_path <- file.path(process_data, stage_0_data)

  write.csv(abstract_data, file = file_path,row.names = F)

}





