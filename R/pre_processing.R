# Pre-processing before shinyapps -------------------------------------------------------------

#' @title Pre-processing functions before shinyapps
#'
#' @description Pre-processing functions and saving intermediate data
#'
#' @rdname add_predefined_vars
#'
#' @param abstract_data a csv file contains abstract information. This could be the csv file downloaded from covidence
#'
#' @param list_vars a vector of data elements you want to extract. i.e. c("no_participants,"no_female","..")
#'
#' @return new dataset with additional empty columns
#'

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


#' @title save_stage_1_data
#'
#' @description
#'This function will save the abstract data with empty columns to the processed data folder.
#'This csv file will be used in the shinyapp 1
#'The data will be stored in metaextroctor_process_data
#'
#' @param abstract_data a csv file contains abstract information as well as the variables you want to extract.
#'
#' @returns a csv file saved in the metaextroctor_process_data file named stage_0_data.csv
#' @export
save_stage_1_data <- function(abstract_data) {

  # check to see if the folder exist or not

  dir.create(metaextroctor_process_data, recursive = TRUE)

  file_path <- file.path(process_data, stage_0_data)

  write.csv(abstract_data, file = file_path,row.names = F)

}





