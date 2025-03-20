# Shinyapp:what is in the abstract -------------------------------------------------------------


#' @title glance_manual_extractor
#'
#' @description A shinyapp that display the abstract and allow for randomly reading in data to glace what are the avaiable information in abstract.
#'
#'

glance_manual_extractor <- function(){
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


  #set_logging(r_console = TRUE, file = TRUE, file_name = "shiny_app_log.txt")

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

        selectInput("sample_method", "Sample Method:",
                    choices = c("Random 10 rows" = "rows10",
                                "Random 5% of data" = "percent5",
                                "Random 10% of data" = "percent10",
                                "All data" = "alldata")),

        ### Random button

        actionButton("resample", "Resample Data", class = "btn-primary"),
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
      downloadButton("downloadData", "Download CSV")
    )
  )




  server <- function(input, output, session) {

    #set_logging_session()

    current_row <- reactiveVal(1)


    # Reactive expression for the data
    data <- reactive({
      req(input$file)

      # Read the CSV file
      tryCatch(
        {
          read_csv(input$file$datapath)
        },
        error = function(e) {
          # Show an error message if file can't be read
          showNotification(paste("Error reading file:", e$message), type = "error")
          return(NULL)
        }
      )
    })

    # Reactive value for the sampled data
    sampled_data <- reactiveVal(NULL)


    # Function to sample the data
    sample_data <- function() {
      df <- data()
      if (is.null(df)) return(NULL)

      nrows <- nrow(df)

      if (input$sample_method == "rows10") {
        sample_size <- min(10, nrows)
        sample_method_text <- "10 random rows"
      }  else if(input$sample_method == "percent5") {
          sample_size <- max(1, round(nrows * 0.05))
          sample_method_text <- "5% of data (random)"}
          else if (input$sample_method == "percent10") {
              sample_size <- max(1, round(nrows * 0.1))
              sample_method_text <- "10% of data (random)"
            } else{
              sample_size <- max(1, round(nrows * 1))
            sample_method_text <- "All data"
          }


    if (nrows > 0) {
      sampled_indices <- sample(1:nrows, sample_size)
      sampled <- df[sampled_indices, ]

      # Return both the sampled data and information about the sample
      list(
        data = sampled,
        info = paste("Showing", sample_size, "of", nrows, "rows -", sample_method_text)
      )
    } else {
      list(
        data = df,
        info = "No data to sample"
      )
    }


    # Update the sampled data when file is uploaded or resample button is clicked
    observeEvent(list(data(), input$resample), {
      result <- sample_data()
      sampled_data(result)

      updateSelectInput(session, "var_manual", choices = grep("_manual$", names(sampled_data),value = T))

      updateSelectInput(session, "selected_vars", choices = names(sampled_data))
    })

    # Display selected data
    output$selected_data <- renderDT({
      req(sampled_data(), input$selected_vars)
      df <- sampled_data()[current_row(), input$selected_vars, drop = FALSE]
      datatable(df,
                options = list(dom = 't', # Only show the table, no controls
                               ordering = FALSE,
                               scrolly = '600px',
                               searchHighlight = TRUE),
                rownames = FALSE)
    })

    checkman_long <- reactive({

      req(sampled_data())

      #  browser()

      col_man <- grep("_manual$", names(sampled_data()))

      sampled_data()[current_row(),col_man,drop = FALSE] |>
        pivot_longer(cols = ends_with("_manual"),
                     names_to = "Manual_variable",
                     values_to = "Manual_value",
                     values_transform =list(Value = as.character)) |>
        arrange(Manual_variable)

    })

    output$add_manual_var <- renderDT({
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
      #browser()
      info <- input$add_manual_var_cell_edit
      # str(info)
      new_data <- sampled_data()
      new_data[current_row(), checkman_long()$Manual_variable[info[["row"]]]] <- info$value
      sampled_data(new_data)
    })


    # Navigate to next row
    observeEvent(input$next_btn, {
      req(sampled_data())
      current <- current_row()
      if (current < nrow(sampled_data())) {
        current_row(current + 1)
      }
      updateTextInput(session, "new_var_value",value = "")
    })

    # Navigate to previouse row

    observeEvent(input$pre_btn, {
      req(sampled_data())
      current <- current_row()
      if (current > 1) {
        current_row(current - 1)
      }
      updateTextInput(session, "new_var_value",value = "")

    })

    # Display current row indicator
    output$row_indicator <- renderText({
      req(sampled_data())
      sprintf("Row %d of %d", current_row(), nrow(sampled_data()))
    })


    # Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-step1", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(sampled_data(), file, row.names = FALSE)
      }
    )


  }

  # Run the app
  shinyApp(ui = ui, server = server)

}
