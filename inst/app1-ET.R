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
                       ".csv")),
  actionButton("example", "Use example"),
  selectInput("selected_vars", "Select columns", choices = NULL, multiple = TRUE),
)

main <- mainPanel(
  h3("Data Preview"),
  div(style = "height: 600px; overflow-y: auto;",
      DTOutput("selected_data")),
  textOutput("row_indicator"),
  width = 12
)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "LLM Systematic Review",
  sidebar = sidebar,
  main
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  current_row <- reactiveVal(1)
  current_data <- reactiveVal(NULL)
  data_info <- reactiveVal("Data Status 🟡 No dataset available.")


  # upload file -------------------------------------------------------------
  observeEvent(input$file, {
    this_data <- readr::read_csv(input$file$datapath)
    info_string <- glue::glue("Data Status 🟢 Using uploaded data.")
    data_info(info_string)
    current_data(this_data)
  })


  # use example data --------------------------------------------------------
  observeEvent(input$example, {
    this_data <- readRDS("abstracts.rds")
    data_info("Data Status 🟢 Using example.")
    current_data(this_data)
  })


  # show data status --------------------------------------------------------
  output$data_status <- renderUI({
    data_info()
  })


  # select data columns -----------------------------------------------------
  observe({
    updateSelectInput(session, "abstract_col", choices = names(current_data()))
    updateSelectInput(session, "selected_vars", choices = names(current_data()))
  })


  # display selected data ---------------------------------------------------
  output$selected_data <- renderDT({
    req(current_data(), input$selected_vars)
    df <- current_data()[current_row(), input$selected_vars, drop = FALSE]
    datatable(df,
              options = list(dom = 't', # Only show the table, no controls
                             ordering = FALSE,
                             scrolly = '800px',
                             searchHighlight = TRUE),
              rownames = FALSE)
  })


  # display row indicator ---------------------------------------------------
  output$row_indicator <- renderText({
    sample_result <- current_data()
    if (!is.null(sample_result) && nrow(sample_result) > 0) {
      paste("Row", current_row(), "of", nrow(sample_result))
    } else {
      "No data"
    }
  })

}



# Run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
