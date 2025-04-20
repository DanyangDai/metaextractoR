
library(shiny)
library(bslib)
library(DT)
library(data.table)
library(tidyverse)
library(shinyFiles)


# CSS ----------------------------------------------------------------------

checkbox_css <- HTML("
  .checkbox-group {
    width: 100%;
    padding: 10px;
    background: #f8f9fa;
    border-radius: 4px;
    max-height: 300px;
    overflow-y: auto;
  }
  .checkbox-group label {
    display: block;
    margin-bottom: 8px;
    width: 100%;
    word-wrap: break-word;
  }
  /* Modify the layout proportions */
  .sidebar {
    min-width: 500px !important;
    max-width: 500px !important;
  }
  .main-content {
    min-width: 0 !important;
    flex: 1;
  }
  /* Make the data table more compact */
  .dataTables_wrapper {
    font-size:1em;
  }
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper .dataTables_length select {
    height: 30px;
    padding: 2px 8px;
  }
")



# UI ----------------------------------------------------------------------

ui <- fluidPage(

  page_sidebar(

    title = "LLM Systematic Review",
    # Add custom CSS
    tags$head(
      tags$style(checkbox_css)
    ),

    sidebar = sidebar(
      width = 500,  # Increased sidebar width

      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                width = "120%"),

     # actionButton(inputId = "upload_data", label = "Upload Testing Abstracts"),
      # selectInput("sample_method", "Sample Method:",
      #             choices = c("Random 10 rows" = "rows10",
      #                         "Random 5% of data" = "percent5",
      #                         "Random 10% of data" = "percent10",
      #                         "All data" = "alldata")),
      # textOutput("sample_info"),

      ### Random button

     # actionButton("resample", "Resample Data", class = "btn-primary"),
      # Horizontal line
      tags$hr(),

      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE,width = "120%"),

      selectInput("selected_vars", "Select columns", choices = NULL, multiple = TRUE),
      # Add new variable section
      tags$hr()),



    # Main panel with data table output and edit interface
    mainPanel(
      width = 1500,
      h3("Data Preview"),
      div(style = "height: 600px; overflow-y: auto;",
          DTOutput("selected_data")),
      textOutput("row_indicator"),
      # textOutput("output_newvar_name"),
      # textOutput("output_newvar_value"),
      card(
        card_header("Manual Extraction"),
        DTOutput("add_manual_var")
      )),

    fluidRow(
      column(12,align = "left", actionButton("pre_btn","Previous")),
      column(12,align = "right", actionButton("next_btn","Next"))
    ),
    # Input: Variable selection
    # Download Button
    shinySaveButton("saveFile", "Save CSV", "Save as...")
  )
)

# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {

  #set_logging_session()

  current_row <- reactiveVal(1)
  current_data <- reactiveVal()
  #browser()
  # Reactive expression for the data
  # data <- reactive({
  #   req(input$file1)
  #
  #   # Read the CSV file
  #   tryCatch(
  #     {
  #       read_csv(input$file1$datapath)
  #     },
  #     error = function(e) {
  #       # Show an error message if file can't be read
  #       showNotification(paste("Error reading file:", e$message), type = "error")
  #       return(NULL)
  #     }
  #   )
  # })

  observe({
    req(input$file1)

    data <- read.csv(input$file1$datapath,
                     header = input$header)
    current_data(data)

    updateSelectInput(session, "abstract_col",choices = names(data))
    updateSelectInput(session, "selected_vars", choices = names(data))

    })
  # Reactive value for the sampled data
  # sampled_data <- reactiveVal()
  #
  #
  # # Function to sample the data
  # sample_data <- function() {
  #   df <- data()
  #   if (is.null(df)) return(NULL)
  #
  #   nrows <- nrow(df)
  #
  #   if(nrows <10){showNotification(paste("This dataset have less than 10 studies,read all studies in."), type = "message")}
  #
  #
  #   if (input$sample_method == "rows10") {
  #     sample_size <- min(10, nrows)
  #     sample_method_text <- "10 random rows"
  #   }  else if(input$sample_method == "percent5") {
  #     sample_size <- max(1, round(nrows * 0.05))
  #     sample_method_text <- "5% of data (random)"}
  #   else if (input$sample_method == "percent10") {
  #     sample_size <- max(1, round(nrows * 0.1))
  #     sample_method_text <- "10% of data (random)"
  #   } else{
  #     sample_size <- max(1, round(nrows * 1))
  #     sample_method_text <- "All data"
  #   }
  #
  #   if (nrows > 0) {
  #     sampled_indices <- sample(1:nrows, sample_size)
  #     sampled <- df[sampled_indices, ]
  #
  #     # Return both the sampled data and information about the sample
  #     list(
  #       data = sampled,
  #       info = paste("Showing", sample_size, "of", nrows, "rows -", sample_method_text)
  #     )
  #   } else {
  #     list(
  #       data = df,
  #       info = "No data to sample"
  #     )
  #   }
  # }
  #

  # Display selected data
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




  checkman_long <- reactive({

    #browser()

    req(current_data())

    result <- current_data()

    col_man <- grep("_manual$", names(result[1,]))

    result[current_row(),col_man,drop = FALSE] |>
      pivot_longer(cols = ends_with("_manual"),
                   names_to = "Manual_variable",
                   values_to = "Manual_value",
                   values_transform =list(Value = as.character)) |>
      arrange(Manual_variable)

  })

  output$add_manual_var <- renderDT({
    req(current_data())
    datatable(checkman_long(),
              editable = TRUE,
              options = list(dom = 't', # Only show the table, no controls
                             ordering = FALSE,
                             scrolly = '800px',
                             searchHighlight = TRUE),
              rownames = FALSE)
  })

  # Observe edits and update the data
  observeEvent(input$add_manual_var_cell_edit, {
    # browser()
    req(current_data,current_row)
    info <- input$add_manual_var_cell_edit
    # str(info)
    new_data <- current_data()
    new_data[current_row(), checkman_long()$Manual_variable[info[["row"]]]] <- info$value
    current_data(new_data)
  })


  # Navigate to next row
  observeEvent(input$next_btn, {
    req(current_data)
    sample_result <- current_data()
    if (!is.null(sample_result) && nrow(sample_result) > 0) {
      curr <- current_row()
      if (curr < nrow(sample_result)) {
        current_row(curr + 1)
      } else {
        # Wrap around to the first row
        current_row(1)
      }
    }
    updateTextInput(session, "new_var_value",value = "")
  })

  # Navigate to previouse row

  observeEvent(input$pre_btn, {
    req(current_data)
    sample_result <- current_data()
    if (!is.null(sample_result) && nrow(sample_result) > 0) {
      curr <- current_row()
      if (curr > 1) {
        current_row(curr - 1)
      } else {
        # Wrap around to the last row
        current_row(nrow(sample_result))
      }
    }
    updateTextInput(session, "new_var_value",value = "")

  })

  # Display current row indicator
  output$row_indicator <- renderText({
    sample_result <- current_data()
    if (!is.null(sample_result) && nrow(sample_result) > 0) {
      paste("Row", current_row(), "of", nrow(sample_result))
    } else {
      "No data"
    }
  })

  volumes <- c(Home = fs::path_home(), "Downloads" = fs::path_home("Downloads"))

  shinyFileSave(input, "saveFile", roots = volumes, session = session)

  observeEvent(input$saveFile, {


    fileinfo <- parseSavePath(volumes, input$saveFile)

    if (nrow(fileinfo) > 0) {
      filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_1",".csv")
      if (!grepl("\\.csv$", filepath)) {
        filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_1",".csv")  # Ensure .csv extension
      }
      write.csv(current_data(), filepath, row.names = FALSE)
      showNotification(paste("File saved to:", filepath), type = "message")
    }
  })

  output$sample_info <- renderText({
    sample_result <- current_data()
    if (!is.null(sample_result)) {
      sample_result$info
    }
  })

}

# Run the app
shinyApp(ui = ui, server = server)


