library(shiny)
library(bslib)
library(DT)
library(shinyEventLogger) # remove this if possible
library(tidyr)
library(dplyr)
library(shinyFiles)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = tagList(icon("flask"), "LLM Systematic Review"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    primary = "#2C7BE5"
  ),
  head_content = tags$head(
    includeCSS("inst/www/checkbox.css"),
    tags$link(rel = "icon", type = "image/png", href = "www/favicon.png"),
    tags$style(HTML("
 .sidebar {
 padding-top: 0.75rem;
 }
 .form-label { font-weight: 600; }
 .help-text { color: #6c757d; font-size: 0.9rem; }
 .card-header {
 font-weight: 700;
 background: #f8f9fa;
 }
 .muted {
 color: #6c757d;
 font-size: 0.9rem;
 }
 .sticky-actions {
 position: sticky;
 bottom: 0;
 z-index: 1000;
 background: rgba(255,255,255,0.95);
 backdrop-filter: saturate(180%) blur(6px);
 border-top: 1px solid #e9ecef;
 padding: 0.75rem 0;
 }
 .data-height {
 height: 60vh; /* responsive table height */
 overflow: auto;
 }
 .btn-outline-secondary {
 border-color: #ced4da;
 }
 "))
  ),
  sidebar = sidebar(
    width = 360,
    h5(class = "mt-2 mb-2", "Upload your CSV"), fileInput( "file1", label = NULL, multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), buttonLabel = "Browse...", placeholder = "No file selected" ), div(class = "help-text mb-3", "CSV should contain your abstracts and any other relevant fields." ),  tags$hr(),  h6(class = "mt-2 mb-1", "Options"), checkboxInput("header", "CSV has a header row", TRUE),  selectizeInput( "selected_vars", "Columns to display", choices = NULL, multiple = TRUE, options = list(placeholder = "Start typing to choose columns...") ),  tags$hr(class = "mb-3")
  ),
  div(
    class = "d-flex justify-content-end mb-3",
    shinySaveButton("saveFile", "Save CSV", "Save as...")
  ),
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center w-100",
          div("Data Preview"),
          div(textOutput("row_indicator", inline = TRUE), class = "muted")
      )
    ),
    div(class = "data-height",
        DTOutput("selected_data")
    )
  ),
  card(
    class = "mt-3",
    card_header("Manual Extraction"),
    DTOutput("add_manual_var")
  ),
  div(
    class = "sticky-actions mt-3",
    fluidRow(
      column(
        width = 6,
        div(
          class = "d-grid",
          actionButton("pre_btn", label = tagList(icon("arrow-left"), "Previous"),
                       class = "btn btn-outline-secondary")
        )
      ),
      column(
        width = 6,
        div(
          class = "d-grid",
          actionButton("next_btn", label = tagList("Next", icon("arrow-right")),
                       class = "btn btn-primary")
        )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {

  #set_logging_session()

  current_row <- reactiveVal(1)
  current_data <- reactiveVal()
  #browser()


  observe({
    req(input$file1)

    data <- read.csv(input$file1$datapath,
                     header = input$header)
    current_data(data)

    updateSelectInput(session, "abstract_col",choices = names(data))
    updateSelectInput(session, "selected_vars", choices = names(data))

  })


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


    req(current_data())

    result <- current_data()

    col_man <- grep("_manual$", names(result[1,]))

    # browser()

    df <- result[current_row(),col_man,drop = FALSE]

    reshape(df, varying = df[grepl("_manual$",names(result)),] |> names(),
            v.name = "Manual_variable",
            timevar = "Manual_value",
            times = df[grepl("_manual$",names(result)),] |> names(),
            direction = "long") |>
      arrange(Manual_variable) |>
      select(-id)

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
    info <- input$add_manual_var_cell_edit
    if (is.null(info) || is.null(info$row)) return()
    vals <- current_data()
    if (is.null(vals)) return()
    cur <- current_row() %||% 1L
    if (cur < 1 || cur > nrow(vals)) return()
    long_df <- isolate(checkman_long())
    if (nrow(long_df) == 0) return()
    col_name <- long_df$Manual_variable[info$row]
    if (is.na(col_name) || !(col_name %in% names(vals))) return()

    old_val <- vals[cur, col_name, drop = TRUE]
    new_val <- tryCatch(DT::coerceValue(info$value, old_val), error = function(e) info$value)
    vals[cur, col_name] <- new_val
    current_data(vals)
  })

  #
  # observeEvent(input$add_manual_var_cell_edit, {
  #   # browser()
  #   req(current_data(),current_row())
  #   info <- input$add_manual_var_cell_edit
  #   # str(info)
  #   new_data <- current_data()
  #   new_data[current_row(), checkman_long()$Manual_variable[info[["row"]]]] <- info$value
  #   current_data(new_data)
  # })


  # Navigate to next row
  observeEvent(input$next_btn, {
    req(current_data())
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


