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
    heading_font = font_google("Inter"),
    secondary = "#6E84A3"
  ),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Overall styling */
      body { background-color: #f8f9fa; }
      .container-fluid { max-width: 1400px; margin: 0 auto; padding: 20px; }

      /* Card styling */
      .card { border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); border: none; margin-bottom: 20px; }
      .card-header {
        font-weight: 600;
        padding: 0.75rem 1.25rem;
        background-color: #fff;
        border-bottom: 1px solid rgba(0,0,0,0.08);
        border-top-left-radius: 8px !important;
        border-top-right-radius: 8px !important;
      }
      .card-body { padding: 1rem; background-color: #fff; }

      /* Spacing */
      .mb-3 { margin-bottom: 1rem !important; }
      .btn + .btn { margin-left: .5rem; }
      .shiny-input-container { width: 100%; margin-bottom: 0.75rem; }

      /* Text styling */
      .soft-text { color: #6c757d; font-size: 0.85rem; }
      .app-title {
        font-size: 1.75rem;
        margin: 0 0 1.5rem 0;
        color: #3a3f51;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .app-title i { color: #2C7BE5; }
      .section-title { font-weight: 600; margin-bottom: 10px; color: #3a3f51; }

      /* Tables */
      .table-wrap {
        height: 35vh; /* Increased height for Other Columns panel */
        overflow: auto;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        background: #fcfcfc;
      }
      .dataTables_wrapper { padding: 10px; }
      table.dataTable { border-collapse: collapse !important; }
      table.dataTable thead th {
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6 !important;
        font-weight: 600;
      }

      /* Inputs */
      .compact-input .form-control {
        height: 38px;
        padding: 0.375rem 0.75rem;
        border-radius: 6px;
        border: 1px solid #d1d9e6;
      }
      .compact-input .form-control:focus {
        box-shadow: 0 0 0 0.2rem rgba(44, 123, 229, 0.25);
        border-color: #2C7BE5;
      }

      /* Buttons */
      .btn {
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s;
      }
      .btn-primary {
        background-color: #2C7BE5;
        border-color: #2C7BE5;
      }
      .btn-primary:hover {
        background-color: #1a68d1;
        border-color: #1a68d1;
      }
      .btn-secondary {
        background-color: #95aac9;
        border-color: #95aac9;
      }
      .btn-secondary:hover {
        background-color: #6E84A3;
        border-color: #6E84A3;
      }
      .compact-btn {
        padding: 0.375rem 0.75rem;
        font-size: 0.9rem;
      }

      /* Navigation */
      .nav-row {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .nav-info {
        background-color: #f8f9fa;
        padding: 8px 12px;
        border-radius: 6px;
        font-size: 0.9rem;
        color: #495057;
      }

      /* Action panel */
      .action-panel {
        display: flex;
        gap: 0.75rem;
        align-items: center;
        width: 100%;
        justify-content: space-between;
      }
      .action-panel-left {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .action-panel-right {
        display: flex;
        align-items: center;
        gap: 10px;
      }

      /* File upload */
      .upload-section {
        display: flex;
        gap: 10px;
        align-items: center;
        margin-bottom: 15px;
      }
      .upload-section .shiny-input-container {
        margin-bottom: 0;
        flex-grow: 1;
      }

      /* Other columns panel */
      #other_columns_selector {
        margin-bottom: 15px;
      }

      /* Editor section */
      .editor-tables {
        display: flex;
        gap: 20px;
      }
      .editor-column {
        flex: 1;
      }

      /* Tooltips */
      .tooltip-icon {
        color: #95aac9;
        margin-left: 5px;
        cursor: help;
      }
    "))
  ),

  div(class = "app-title", icon("table"), "CSV Data Viewer and Editor"),

  fluidRow(
    column(
      3,
      card(
        class = "mb-3",
        card_header(div(icon("upload"), "Upload & Navigation")),
        card_body(
          div(class = "upload-section",
              div(style = "flex-grow: 1;",
                  fileInput("file", "Choose CSV File", accept = ".csv", width = "100%",
                            buttonLabel = "Browse...", placeholder = "No file selected")
              ),
              downloadButton("download_sample", "Sample", class = "btn-sm",
                             title = "Download a sample CSV file")
          ),

          hr(style = "margin: 1rem 0;"),

          div(class = "section-title", "Row Navigation"),
          div(class = "nav-row",
              div(style = "width: 30%;",
                  div(class = "compact-input",
                      numericInput("current_row", "Row:", 1, min = 1, step = 1, width = "100%")
                  )
              ),
              div(style = "width: 35%;",
                  actionButton("prev_row", "Previous", class = "btn btn-secondary w-100 compact-btn",
                               icon = icon("arrow-left"))
              ),
              div(style = "width: 35%;",
                  actionButton("next_row", "Next", class = "btn btn-secondary w-100 compact-btn",
                               icon = icon("arrow-right"))
              )
          ),
          div(class = "nav-info mt-2", textOutput("row_info"))
        )
      )
    ),

    column(
      9,
      card(
        class = "mb-3",
        card_header(div(icon("columns"), "Abstracts")),
        card_body(
          uiOutput("other_columns_selector"),
          div(class = "table-wrap", DTOutput("other_columns_table"))
        )
      )
    )
  ),

  card(
    class = "mb-3",
    card_header(
      div(class = "action-panel",
          div(class = "action-panel-left",
              div(icon("wand-magic-sparkles"), "Data Editor"),
              span(class = "soft-text", "Edit and manage your data values")
          ),
          div(class = "action-panel-right",
              actionButton("copy_values", "Copy LLM to Manual", class = "btn btn-primary btn-sm compact-btn",
                           icon = icon("copy"), title = "Copy all LLM values to manual columns"),
              shinySaveButton("saveFile", "Save CSV", "Save as...", icon = icon("download"),
                              class = "btn-primary btn-sm compact-btn")
          )
      )
    ),
    card_body(
      fluidRow(
        column(
          6,
          div(class = "editor-column",
              div(class = "section-title",
                  "Variables ending with _llm",
                  span(class = "tooltip-icon", icon("circle-info"),
                       title = "These values are generated by the LLM and cannot be edited directly")
              ),
              div(class = "table-wrap", DTOutput("llm_table"))
          )
        ),
        column(
          6,
          div(class = "editor-column",
              div(class = "section-title",
                  "Variables ending with _manual (Editable)",
                  span(class = "tooltip-icon", icon("circle-info"),
                       title = "You can edit these values directly by clicking on cells")
              ),
              div(class = "table-wrap", DTOutput("manual_table"))
          )
        )
      ),
      div(class = "mt-3", verbatimTextOutput("warning_message"))
    )
  )
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


