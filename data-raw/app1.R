## code to prepare `app1` dataset goes here
training_stage_0_data <- readr::read_csv("data-raw/sample_data/training_stage_0_data.csv")

usethis::use_data(training_stage_0_data, overwrite = TRUE)

testing_stage_0_data <- readr::read_csv("data-raw/sample_data/testing_stage_0_data.csv")

usethis::use_data(testing_stage_0_data, overwrite = TRUE)

app_2 <- readr::read_csv("data-raw/sample_data/app_2.csv")
usethis::use_data(app_2, overwrite = TRUE)

app_3 <- readr::read_csv("data-raw/sample_data/app_3.csv")
usethis::use_data(app_3, overwrite = TRUE)
