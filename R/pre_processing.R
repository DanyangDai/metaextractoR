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



#' @title set_training
#'
#' @description
#' This function will separate the abstracts into training and testing sets.
#'
#' @param abstracts The csv file contains abstracts with
#'
#' @param percentage percentage of separation training sets.
#'

separte_training <- function(abstracts, percentage = 0.1){

  # Set seed for reproducibility (if provided)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Read CSV file
  df <- read.csv(abstracts_path)

  # Create a random index for splitting
  sample_size <- floor(nrow(df) * percentage)
  test_indices <- sample(seq_len(nrow(df)), size = sample_size)

  # Split data into training and testing sets
  test_df <- df[test_indices, ]
  train_df <- df[-test_indices, ]

  return(list(train = train_df, test = test_df))

}

#' @title save_training_data
#'
#' @description
#'This function will save the training abstract data with empty columns to the processed data folder.
#'This csv file will be used in the shinyapp 1
#'The data will be stored in metaextroctor_process_data
#'
#' @param training_abs training abstracts including the variables you want to extract.
#'
#' @returns a csv file saved in the metaextroctor_process_data file named training_stage_0_data.csv
#' @export
#'
#'
save_training_data <- function(training_abs) {

  # check to see if the folder exist or not

  dir.create(metaextroctor_process_data, recursive = TRUE)

  file_path <- file.path(process_data, training_stage_0_data)

  write.csv(training_abs, file = file_path,row.names = F)

}

#' @title save_testing_data
#'
#' @description

#'This function will save the testing abstract data with empty columns to the processed data folder.
#'This csv file will be used in the shinyapp 2
#'The data will be stored in metaextroctor_process_data
#'
#' @param testing_abs training abstracts including the variables you want to extract.
#'
#' @returns a csv file saved in the metaextroctor_process_data file named training_stage_0_data.csv
#' @export
#'
save_training_data <- function(testing_abs) {

  # check to see if the folder exist or not

  dir.create(metaextroctor_process_data, recursive = TRUE)

  file_path <- file.path(process_data, testing_stage_0_data)

  write.csv(testing_abs, file = file_path,row.names = F)

}
