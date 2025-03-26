# Batch text processing for extraction -------------------------------------------------------------


#' @title process_with_ollama
#'
#' @description A function that batch process data extraction from text.
#'
#' @param input A dataframe contains abstracts and varaibles we want to extract.
#'
#' @param model Larage Language Model name you want to use i.e. "llama3.1:8b"
#'
#'
#'
#'


type_abstract <- type_object(
  "Some key information from abstract.",
  no_patients_llm = type_integer("Find the total number of patients included in this study.",required = FALSE),
  no_AKI_llm = type_integer("Number of Acute Kidney Injury (AKI) patients included in this study.", required = FALSE),
  per_AKI_llm = type_number("Percentage of Acute Kidney Injury ",required = FALSE),
  ICU_llm = type_boolean("Included only Intensive Care Unit (ICU) patients.",required = FALSE),
  start_date = type_string("The starting date of the study written in YYYY-MM-DD format",required = FALSE),
  end_date = type_string("The end date of the study written in YYYY-MM-DD format",required = FALSE),
  age_mean_llm = type_number("Find the average age of the study cohort",required = FALSE),
  age_median_llm = type_number("Find the median age of the study cohort",required = FALSE),
)


process_with_ollama <- function(input,model = "llama3.1:8b", i) {
  if(missing(i)) i <- 1:nrow(input)
  chat <- chat_ollama(model = model,
                      seed = 1,
                      api_args = list(temperature = 0))
  start <- Sys.time()
  processed <- imap_dfr(input$abstract[i], function(abstract, num) {
    cli::cli_alert("Processing abstract #{i[num]}")
    bot <- chat$clone()
    bot$extract_data(abstract, type = type_abstract)
  })
  end <- Sys.time()
  data <- bind_cols(input[i, ], processed)
  # |>
  #   mutate(valid_accuracy = no_of_patients_overall == no_patients)
  return(list(data, time = end - start))
}
