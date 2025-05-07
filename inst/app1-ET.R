library(shiny)
library(bslib)
library(DT)
library(shinyEventLogger) # remove this if possible
library(tidyr)
library(dplyr)
library(shinyFiles)


# UI ----------------------------------------------------------------------

sidebar <- sidebar(
  htmlOutput("data_status"),
  fileInput("file", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"),
            width = "120%"),
  actionButton("example", "Use example"),
  selectInput("selected_vars", "Select columns", choices = NULL, multiple = TRUE),
)

main <- mainPanel(

)

ui <- page_sidebar(
  title = "LLM Systematic Review",
  sidebar = sidebar,
  main
)

server <- function(input, output, session) {

  current_data <- reactiveVal(NULL)
  data_info <- reactiveVal("Data Status 🟡 No dataset available.")

  observeEvent(input$file, {
    this_data <- readr::read_csv(input$file$datapath)
    info_string <- glue::glue("Data Status 🟢 Using uploaded data.")
    data_info(info_string)
    current_data(this_data)
  })

  observeEvent(input$example, {
    this_data <- readRDS("abstracts.rds")
    data_info("Data Status 🟢 Using example.")
    current_data(this_data)
  })

  output$data_status <- renderUI({
    data_info()
  })

  observe({
    updateSelectInput(session, "abstract_col", choices = names(current_data()))
    updateSelectInput(session, "selected_vars", choices = names(current_data()))

  })

}


# Run the app
shinyApp(ui = ui, server = server)
