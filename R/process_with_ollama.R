# Batch text processing for extraction -------------------------------------------------------------

#' @title Process the abstract with a large language model
#'
#' @description A function that batch process data extraction from text.
#'
#' @param input A data frame contains abstracts and variables we want to extract. If you are not sure how the data should look like, please see the example dataset abstract. This data should be the testing data saved using the `save_training_data()` function.
#' @param model Large Language Model name you want to use i.e. "llama3.1:8b". If you are not sure what LLMs have been installed locally, please use the function: ellmer:::ollama_models() to find out what LLMs are available.
#' @param type_abstract is created using the type_object function from the ellmer package. Objects represent a collection of named values and are created with type_object(). Objects can contain any number of scalars, arrays, and other objects. They are similar to named lists in R. If still not sure, please look up the documentation of ellmer::type_object().
#' @param i number of abstracts you want to process at once.
#' @param abstract_col column name for the column contains abstract.
#'
#' @importFrom ellmer chat_ollama
#' @importFrom purrr imap_dfr
#' @importFrom dplyr bind_cols
#'
#' @examples
#' library(ellmer)
#' type_abstract <- type_object("Some key information from abstract.",
#'   no_patients_llm = type_integer("Find the total number of patients
#'                                   included in this study.",
#'                                   required = FALSE),
#'   no_AKI_llm = type_integer("Number of Acute Kidney Injury (AKI) patients
#'                              included in this study.",
#'                              required = FALSE),
#'   per_AKI_llm = type_number("Percentage of Acute Kidney Injury ",
#'                              required = FALSE),
#'   ICU_llm = type_boolean("Included only Intensive Care Unit (ICU) patients.",
#'                           required = FALSE),
#'   start_date = type_string("The starting date of the study written in
#'                             YYYY-MM-DD format",
#'                             required = FALSE),
#'   end_date = type_string("The end date of the study written in YYYY-MM-DD
#'                           format",
#'                           required = FALSE),
#'   age_mean_llm = type_number("Find the average age of the study cohort",
#'                               required = FALSE),
#'   age_median_llm = type_number("Find the median age of the study cohort",
#'                                 required = FALSE)
#' )
#' \dontrun{
#' process_with_ollama(abstracts,
#'                     type_abstract = type_abstract,
#'                     i = 1,
#'                     abstract_col = "Abstract")
#' }
#' @return Returns a data.frame with results and time.
#' @export
process_with_ollama <- function(
  input,
  model = "llama3.1:8b",
  type_abstract,
  i,
  abstract_col
) {
  input <- input |>
    select(-ends_with("llm"))

  # Validate input
  if (!is.data.frame(input)) {
    stop("Input must be a data frame")
  }

  if (!abstract_col %in% colnames(input)) {
    stop(sprintf(
      "Column '%s' not found in the data frame. Available columns: %s",
      abstract_col,
      paste(colnames(input), collapse = ", ")
    ))
  }

  result_df <- input

  # Convert abstracts to character and handle NA values
  result_df[[abstract_col]] <- as.character(result_df[[abstract_col]])
  result_df[[abstract_col]][is.na(result_df[[abstract_col]])] <- ""

  if (missing(i)) {
    i <- 1:nrow(input)
  }

  chat <- chat_ollama(model = model, seed = 1, api_args = list(temperature = 0))
  start <- Sys.time()

  results <- list() # Collect all results here
  for (num in i) {
    cli::cli_alert(sprintf("Processing abstract #%d", num))
    # Get the abstract text directly
    abstract_text <- result_df[[abstract_col]][num]
    # Skip if abstract is too short (optional)
    if (nchar(abstract_text) < 10) {
      cli::cli_alert_warning(sprintf(
        "Abstract #%d is too short, skipping",
        num
      ))
      next
    }

    tryCatch(
      {
        # Create a new chat instance for each abstract
        bot <- chat$clone()
        # Extract data and capture the result
        result <- bot$extract_data(abstract_text, type = type_abstract)

        # Store the result with the index as key
        results[[as.character(num)]] <- result
      },
      error = function(e) {
        # Add error handling
        cli::cli_alert_danger(sprintf(
          "Error processing abstract #%d: %s",
          num,
          e$message
        ))
        # For errors, store NULL (we'll handle it later)
        results[[as.character(num)]] <- NULL
      }
    )
  }

  create_empty_row <- function(template_df) {
    if (is.null(template_df) || nrow(template_df) == 0) {
      return(data.frame(placeholder = NA))
    }

    # Create a data frame with same columns but NA values
    empty_row <- as.data.frame(lapply(template_df, function(col) {
      if (is.factor(col)) {
        factor(NA, levels = levels(col))
      } else {
        as(NA, class(col)[1])
      }
    }))

    return(empty_row)
  }

  # Find template from non-NULL results
  template <- Find(function(x) !is.null(x), results)

  # Process results
  results_processed <- lapply(names(results), function(idx) {
    result <- results[[idx]]

    if (is.null(result)) {
      empty_row <- create_empty_row(template)
      empty_row$row_id <- idx
      empty_row
    } else {
      result$row_id <- idx
      result
    }
  })

  processed <- bind_rows(results_processed)

  # Log processing information
  message(sprintf("Started with %d abstracts", length(i)))

  end <- Sys.time()

  data <- bind_cols(result_df[i, ], processed)

  return(list(data, time = end - start))
}
