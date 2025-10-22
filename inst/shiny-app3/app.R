library(shiny)
library(bslib)
library(DT)
library(shinyFiles)
library(shinyjs)

# UI ----------------------------------------------------------------------

ui <- page_fluid(
theme = bs_theme(
 bootswatch = "flatly",
 primary = "#2C7BE5",
 base_font = font_google("Inter"),
 heading_font = font_google("Inter")
 ),

  useShinyjs(),
  tags$head(
    tags$style(HTML("
 .card-header { font-weight: 600; }
 .mb-3 { margin-bottom: 1rem !important; }
 .btn + .btn { margin-left: .5rem; }
 .shiny-input-container { width: 100%; }
 .soft-text { color: #6c757d; font-size: 0.9rem; }
 .table-wrap { height: 50vh; overflow: auto; }
 .table-wrap-tall { height: 60vh; overflow: auto; }
 "))
  ),

  titlePanel(div(icon("table"), "CSV Data Viewer and Editor")),

  card(
    class = "mb-3",
    card_header(div(icon("upload"), "Upload CSV File")),
    downloadButton("download_sample", "Download Sample Data from GitHub"),
    card_body(
      fileInput("file", "Choose CSV File", accept = ".csv", width = "100%"),
      div(class = "soft-text", "Upload a CSV file to begin. Large files may take a moment to load.")
    )
  ),

  card(
    class = "mb-3",
    card_header(div(icon("columns"), "Other Columns Display")),
    card_body(
      uiOutput("other_columns_selector"),
      div(class = "soft-text", "Select which additional columns to show alongside your main tables.")
    ),

    card_body(
      div(class = "table-wrap", DTOutput("other_columns_table"))
    )
  ),
  fluidRow(
    column(
      6,
      card(
        class = "mb-3",
        card_header(div(icon("compass"), "Navigation")),
        card_body(
          fluidRow(
            column(4, numericInput("current_row", "Current Row:", 1, min = 1, step = 1, width = "100%")),
            column(4, actionButton("prev_row", "Previous Row", class = "btn btn-secondary w-100", icon = icon("arrow-left"))),
            column(4, actionButton("next_row", "Next Row", class = "btn btn-secondary w-100", icon = icon("arrow-right")))
          ),
          div(class = "soft-text mt-2", "Use the controls to move through rows."),
          div(style = "margin-top: .5rem;", textOutput("row_info"))
        )
      )
    ),
    column(
      6,
      card(
        class = "mb-3",
        card_header(div(icon("wand-magic-sparkles"), "Actions")),
        card_body(
          actionButton("copy_values", "Copy llm values to manual", class = "btn btn-primary", icon = icon("copy")),
          div(class = "soft-text mt-2", "Copies LLM-derived values into the corresponding editable manual fields.")
        )
      )
    )
  ),
  fluidRow(
    column(
      6,
      card(
        class = "mb-3",
        card_header(div(icon("robot"), "Variables ending with _llm")),
        card_body(
          div(class = "table-wrap-tall", DTOutput("llm_table"))
        )
      )
    ),
    column(
      6,
      card(
        class = "mb-3",
        card_header(div(icon("pen-to-square"), "Variables ending with _manual (Editable)")),
        card_body(
          div(class = "table-wrap-tall", DTOutput("manual_table"))
        )
      )
    ),
    column(
      6,
      card(
        class = "mb-3",
        card_header(div(icon("triangle-exclamation"), "Warning")),
        card_body(
          verbatimTextOutput("warning_message")
        )
      )
    )
  ),
  shinySaveButton("saveFile", "Save CSV", "Save as...", icon = icon("download"))
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


  # Downloadable csv of selected dataset
  volumes <- c(Home = fs::path_home(), "Downloads" = fs::path_home("Downloads"))

  shinyFileSave(input, "saveFile", roots = volumes, session = session)

  # Read CSV file when uploaded
  observeEvent(input$file, {
    req(input$file)

    # Read the CSV file
    data <- read.csv(input$file$datapath)
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

  observeEvent(input$saveFile, {

    #browser()

    fileinfo <- parseSavePath(volumes, input$saveFile)
    req(vals$data)

    # Reconstruct the full data frame with modifications
    data <- vals$data

    # Update with modified _manual values
    manual_cols <- grep("_manual$", names(data), value = TRUE)

    for (col in manual_cols) {
      data[, col] <- vals$manual_data[, col]
    }


    if (nrow(fileinfo) > 0) {
      filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_3",".csv")
      if (!grepl("\\.csv$", filepath)) {
        filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_3",".csv")  # Ensure .csv extension
      } else{
        filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_3",".csv")
      }
      write.csv(data, filepath, row.names = FALSE)
      showNotification(paste("File saved to:", filepath), type = "message")
    }
  })


  output$download_sample <- downloadHandler(
    filename = function() {
      "sample_data_app1.csv"
    },
    content = function(file) {
      # GitHub raw file URL
      url <- "https://github.com/DanyangDai/metaextractoR/blob/main/sample_data/app_3.csv"
      # Download file
      GET(url, write_disk(file, overwrite = TRUE))
    }
  )

  # Show save button only if data is loaded
  # output$save_button_ui <- renderUI({
  #   req(vals$data)
  #   card(
  #     card_header("Save Changes"),
  #     downloadButton("save_data", "Download Modified CSV", class = "btn-success")
  #   )
  # })

#
#   # Download handler for the modified data
#   output$save_data <- downloadHandler(
#     filename = function() {
#       paste0("modified_", input$file$name)
#     },
#     content = function(file) {
#       # Reconstruct the full data frame with modifications
#       data <- vals$data
#
#       # Update with modified _manual values
#       manual_cols <- grep("_manual$", names(data), value = TRUE)
#       for (col in manual_cols) {
#         data[, col] <- vals$manual_data[, col]
#       }
#
#       # Write to CSV
#       write_csv(data, file)
#     }
#   )
}

shinyApp(ui = ui, server = server)


