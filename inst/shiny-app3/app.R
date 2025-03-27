library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)

# UI ----------------------------------------------------------------------

ui <- page_fluid(
  titlePanel("CSV Data Viewer and Editor"),

  card(
    card_header("Upload CSV File"),
    fileInput("file", "Choose CSV File", accept = ".csv")
  ),

  # New Card for Other Columns Selection
  card(
    card_header("Other Columns Display"),
    uiOutput("other_columns_selector"),
    DTOutput("other_columns_table")
  ),

  fluidRow(
    column(6,
           card(
             card_header("Navigation"),
             fluidRow(
               column(4, numericInput("current_row", "Current Row:", 1, min = 1, step = 1)),
               column(4, actionButton("prev_row", "Previous Row", class = "btn-secondary")),
               column(4, actionButton("next_row", "Next Row", class = "btn-secondary"))
             ),
             textOutput("row_info")
           )
    ),
    column(6,
           card(
             card_header("Actions"),
             actionButton("copy_values", "Copy _llm values to _manual", class = "btn-primary")
           )
    )
  ),

  fluidRow(
    column(6,
           card(
             card_header("Variables ending with _llm"),
             DTOutput("llm_table")
           )
    ),
    column(6,
           card(
             card_header("Variables ending with _manual (Editable)"),
             DTOutput("manual_table")
           )
    ),
    column(6,
           card(
             card_header("Warning"),
             verbatimTextOutput("warning_message")
           )
    )
  ),

  uiOutput("save_button_ui")
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # Initialize reactive values
  vals <- reactiveValues(
    data = NULL,
    llm_data = NULL,
    manual_data = NULL,
    other_data = NULL,
    total_rows = 0,
    other_columns = NULL
  )


  # Read CSV file when uploaded
  observeEvent(input$file, {
    req(input$file)

    # Read the CSV file
    data <- read_csv(input$file$datapath)
    vals$data <- data
    vals$total_rows <- nrow(data)

    # Update max row value
    updateNumericInput(session, "current_row", max = vals$total_rows)

    # Extract columns ending with _llm and _manual
    llm_cols <- grep("_llm$", names(data), value = TRUE)
    manual_cols <- grep("_manual$", names(data), value = TRUE)

    # Identify other columns (not ending with _llm or _manual)
    other_cols <- setdiff(names(data), c(llm_cols, manual_cols))
    vals$other_columns <- other_cols

    vals$llm_data <- data[, llm_cols, drop = FALSE]
    vals$manual_data <- data[, manual_cols, drop = FALSE]
    vals$other_data <- data[, other_cols, drop = FALSE]
  })

  # Render the column selector UI
  output$other_columns_selector <- renderUI({
    req(vals$other_columns)

    selectInput("selected_other_columns",
                "Select columns to display:",
                choices = vals$other_columns,
                multiple = TRUE,
                width = "100%")
  })

  # Display other columns table
  output$other_columns_table <- renderDT({
    req(vals$other_data, input$current_row, input$selected_other_columns)

    if (length(input$selected_other_columns) == 0) {
      return(NULL)
    }

    if (input$current_row <= vals$total_rows) {
      # Select only the columns chosen by the user
      selected_data <- vals$other_data[input$current_row, input$selected_other_columns, drop = FALSE]

      # Transpose the data for display
      selected_data_t <- as.data.frame(t(selected_data))
      selected_data_t <- cbind(Variable = rownames(selected_data_t), Value = selected_data_t[,1])
      rownames(selected_data_t) <- NULL
      colnames(selected_data_t) <- c("Variable", "Value")

      datatable(selected_data_t,
                options = list(dom = 't', ordering = FALSE, pageLength = 50),
                rownames = FALSE)
    }
  })

  # Display row info
  output$row_info <- renderText({
    req(vals$data)
    paste("Viewing row", input$current_row, "of", vals$total_rows)
  })

  # Navigate to previous row (with auto-save)
  observeEvent(input$prev_row, {
    if (input$current_row > 1) {
      # Auto-saving is handled by the cell edit observer
      # which directly updates vals$manual_data
      updateNumericInput(session, "current_row", value = input$current_row - 1)
    }
  })

  # Navigate to next row (with auto-save)
  observeEvent(input$next_row, {
    if (input$current_row < vals$total_rows) {
      # Auto-saving is handled by the cell edit observer
      # which directly updates vals$manual_data
      updateNumericInput(session, "current_row", value = input$current_row + 1)
    }
  })

  # Display _llm table
  output$llm_table <- renderDT({
    req(vals$llm_data)
    req(input$current_row)

    if (input$current_row <= vals$total_rows) {
      llm_row_data <- vals$llm_data[input$current_row, , drop = FALSE]
      llm_row_data_t <- as.data.frame(t(llm_row_data))
      llm_row_data_t <- cbind(Variable = rownames(llm_row_data_t), Value = llm_row_data_t[,1])
      rownames(llm_row_data_t) <- NULL
      colnames(llm_row_data_t) <- c("Variable", "Value")

      datatable(llm_row_data_t, options = list(dom = 't', ordering = FALSE, pageLength = 50),
                rownames = FALSE)
    }
  })


  # Display _manual table with editing enabled
  output$manual_table <- renderDT({
    req(vals$manual_data)
    req(input$current_row)

    if (input$current_row <= vals$total_rows) {
      manual_row_data <- vals$manual_data[input$current_row, , drop = FALSE]
      manual_row_data_t <- as.data.frame(t(manual_row_data))
      manual_row_data_t <- cbind(Variable = rownames(manual_row_data_t), Value = manual_row_data_t[,1])
      rownames(manual_row_data_t) <- NULL
      colnames(manual_row_data_t) <- c("Variable", "Value")

      datatable(manual_row_data_t,
                options = list(dom = 't', ordering = FALSE, pageLength = 50),
                rownames = FALSE,
                editable = list(target = "cell", disable = list(columns = c(0))))
    }
  })

  # Handle cell editing in the _manual table
  observeEvent(input$manual_table_cell_edit, {
    info <- input$manual_table_cell_edit

    # Get the variable names in the transposed table
    req(vals$manual_data, input$current_row <= vals$total_rows)

    # browser()

    # Create the transposed representation of the data to get the variable name
    manual_row_data <- vals$manual_data[input$current_row, , drop = FALSE]
    manual_col_names <- colnames(manual_row_data)

    # Since the table is transposed, we need to match the row in the
    # displayed table to the column in the original data
    if (info$row <= length(manual_col_names)) {
      variable_name <- manual_col_names[info$row]

      check_type <-  vals$manual_data[input$current_row, variable_name]

      if (is.numeric(check_type[[1]])){
        vals$manual_data[input$current_row, variable_name] <- as.numeric(info$value)
      } else if (is.character(check_type[[1]])){
        # Update the value in the original data frame
        validate(
          need(is.character(check_type[[1]]), "Please check the variable input type to be character")
        )

        vals$manual_data[input$current_row, variable_name] <- as.character(info$value)
      } else if (is.logical(check_type[[1]])){
        validate(
          need(is.logical(check_type[[1]]), "Please check the variable input type to be logical.")
        )

        vals$manual_data[input$current_row, variable_name] <- as.logical(info$value)
      } else {
        output$warning_message <- renderText({
          paste("Warning: Please check the input value is consistent.")
        })
      }
    }
  })

  # Copy values from _llm to _manual
  observeEvent(input$copy_values, {
    req(vals$llm_data, vals$manual_data)

    current_row <- input$current_row

    # Get _llm column names and corresponding _manual column names
    llm_cols <- names(vals$llm_data)

    for (llm_col in llm_cols) {
      # Generate corresponding _manual column name
      manual_col <- sub("_llm$", "_manual", llm_col)
      if (is.na( vals$manual_data[current_row, manual_col] )){
        # Check if the corresponding _manual column exists
        if (manual_col %in% names(vals$manual_data)) {
          # Copy the value
          vals$manual_data[current_row, manual_col] <- vals$llm_data[current_row, llm_col]
        }} else {
          vals$manual_data[current_row, manual_col] <- vals$manual_data[current_row, manual_col]
        }
    }
  })

  # Show save button only if data is loaded
  output$save_button_ui <- renderUI({
    req(vals$data)
    card(
      card_header("Save Changes"),
      downloadButton("save_data", "Download Modified CSV", class = "btn-success")
    )
  })

  # Download handler for the modified data
  output$save_data <- downloadHandler(
    filename = function() {
      paste0("modified_", input$file$name)
    },
    content = function(file) {
      # Reconstruct the full data frame with modifications
      data <- vals$data

      # Update with modified _manual values
      manual_cols <- grep("_manual$", names(data), value = TRUE)
      for (col in manual_cols) {
        data[, col] <- vals$manual_data[, col]
      }

      # Write to CSV
      write_csv(data, file)
    }
  )
}

shinyApp(ui = ui, server = server)


