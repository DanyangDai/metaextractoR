# Extract unique prefixes
get_prefix <- function(name) {
  sub("_.*", "", name)
}
prefixes <- unique(sapply(names(df), get_prefix))

get_download_locations <- function() {
  c(Home = fs::path_home(), "Downloads" = fs::path_home("Downloads"))
}

library(bslib)
library(shinyFiles)
# check functions

ollama_installed <- function() {
  nzchar(Sys.which("ollama"))
}

ollama_running <- function() {
  con <- try(
    socketConnection(
      host = "localhost",
      port = 11434,
      open = "r+",
      blocking = TRUE,
      timeout = 1
    ),
    silent = TRUE
  )
  if (inherits(con, "try-error")) {
    return(FALSE)
  } else {
    close(con)
    return(TRUE)
  }
}
library(shiny)

show_datatable <- function(.data, app_number, edit = FALSE) {
  if(app_number == 1) {
    DT::datatable(
      .data,
      editable = edit,
      options = list(
        dom = 't', # Only show the table, no controls
        ordering = FALSE,
        scrolly = '800px',
        searchHighlight = TRUE
      ),
      rownames = FALSE
    )
  } else if(app_number == 2) {
    # uses the same as 1 for now
  } else if(app_number == 3) {
    DT::datatable(
      .data,
      options = list(dom = 't', ordering = FALSE, pageLength = 50),
      rownames = FALSE,
      editable = edit
    )
  }
}


# UI ----------------------------------------------------------------------
ui <- page_sidebar(
  # CHANGED: use bslib page as the top-level (replaces fluidPage)
  title = "Prompt Engineering",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = TRUE, # NEW: allow content to fill the viewport (enables no page scroll)
  sidebar = sidebar(
    width = 360, # CHANGED: narrower sidebar to leave more space for main content
    downloadButton("download_sample", "Download Sample Data"),
    h5("Upload data"),
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
      width = "100%"
    ),
    checkboxInput("header", "Header", TRUE),
    actionButton(
      "upload_data",
      "Upload abstracts",
      class = "btn-primary",
      width = "100%"
    ),
    hr(),
    h5("Select columns to Display on the Data Preview"),
    selectInput(
      "selected_vars",
      "Select columns",
      choices = NULL,
      multiple = TRUE,
      width = "100%"
    ),
    card(
      header = card_header(
        icon("wand-magic-sparkles"),
        "Use LLM for data extraction"
      ),
      selectInput(
        "var_llm",
        "Select LLM variable column",
        choices = NULL,
        width = "100%"
      ),
      selectInput(
        "model_name",
        "Model Name",
        choices = ellmer::models_ollama(),
        width = "100%"
      ),
      selectInput(
        "extraction_type",
        "Type of Extraction Element",
        choices = c(
          "Integer" = "integer",
          "Double" = "number",
          "Binary" = "boolean",
          "Text" = "string"
        ),
        width = "100%"
      ),
      selectInput(
        "abstract_col",
        "Select Abstract column",
        choices = NULL,
        width = "100%"
      ),
      textAreaInput(
        "LLM_prompt",
        "Prompt for LLM extraction",
        height = "80px", # CHANGED: slightly shorter to save vertical space
        placeholder = "Describe exactly what to extract, expected format, and constraints."
      ),
      card_footer(
        actionButton(
          "useLLM",
          "Create LLM Variable",
          class = "btn-success",
          width = "100%",
          icon = icon("robot")
        )
      )
    ),
    card(
      class = "sidebar-card",
      card_header(icon("check-circle"), "Validation"),
      div(style = "min-height: 1.5em;", textOutput("variable_check")),
      div(style = "min-height: 1.5em;", textOutput("check_value")),
      div(style = "min-height: 1.5em;", textOutput("equalCheck"))
    )
  ),
  tags$head(tags$style(HTML(
    "
 html, body { height: 100%; overflow: hidden; } /* NEW: prevent page-level scroll /
 .bslib-sidebar { max-height: 100vh; overflow-y: auto; }/ NEW: sidebar gets its own scroll when needed */
 "
  ))),
  shinyjs::useShinyjs(), # MOVED: stays within page (works the same)
  uiOutput("ollama_warning"), # MOVED: inside page (top of main content)
  card(
    body_fill = TRUE, # NEW: let the card body fill the remaining height
    card_header(h3("Data Preview")),
    card_body(
      div(
        style = "height: 100%; overflow: auto;", # CHANGED: replaces fixed 800px height with flexible fill+scroll
        DT::DTOutput("selected_data")
      )
    ),
    card_footer(
      # Controls in a single line to save vertical space
      div(
        class = "d-flex gap-2 align-items-center",
        actionButton("pre_btn", "Previous"),
        actionButton("next_btn", "Next"),
        div(
          class = "ms-auto",
          shinySaveButton("saveFile", "Save CSV", "Save as...")
        )
      ),
      # Status texts placed in footer to avoid pushing content vertically
      div(
        class = "mt-2 d-flex gap-3",
        textOutput("row_indicator"),
        textOutput("output_newvar_name"),
        textOutput("output_newvar_value")
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  notify_warn <- function(msg, dur = 6) {
    showNotification(msg, type = "warning", duration = dur)
  }
  notify_err <- function(msg, dur = 8) {
    showNotification(msg, type = "error", duration = dur)
  }
  notify_info <- function(msg, dur = 4) {
    showNotification(msg, type = "message", duration = dur)
  }

  output$ollama_warning <- renderUI({
    if (!ollama_installed()) {
      div(
        style = "padding:10px; margin-bottom:15px; border:1px solid red;
                 background-color:#ffe6e6; color:#990000; border-radius:5px;",
        strong("⚠ Ollama not installed: "),
        "Please install Ollama from ",
        tags$a(href = "https://ollama.ai", "https://ollama.ai"),
        "."
      )
    } else if (!ollama_running()) {
      div(
        style = "padding:10px; margin-bottom:15px; border:1px solid orange;
                 background-color:#fff3cd; color:#664d03; border-radius:5px;",
        strong("⚠ Ollama not running: "),
        "Ollama is installed but not running.
         Start it by opening a terminal and running ",
        code("ollama serve"),
        "."
      )
    } else {
      NULL # everything is fine
    }
  })

  # create log file in the background
  # Don't know how
  temp <- NULL

  log_dir <- "log_files"
  temp_log_file <- tempfile(pattern = "user_log_", fileext = ".csv")

  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
    message("Folder created: ", log_dir)
  } else {
    message("Folder already exists: ", log_dir)
  }

  current_data <- reactiveVal()
  current_row <- reactiveVal(1)
  ollama_ok <- reactiveVal(FALSE)

  observe({
    temp <<- reactiveValues(
      model = input$model_name,
      prompt = input$LLM_prompt,
      row = current_row(),
    )
    # print("reached observe")
    shinyjs::runjs(
      'console.log("reached");console.log(document.getElementById("LLM_prompt").style.backgroundColor);document.getElementById("LLM_prompt").style.backgroundColor = "white";'
    )
  })

  observe({
    req(input$file1)

    data <- read.csv(input$file1$datapath, header = input$header)
    current_data(data)
    updateSelectInput(session, "abstract_col", choices = names(data))
    updateSelectInput(session, "selected_vars", choices = names(data))
    updateSelectInput(
      session,
      "var_llm",
      choices = grep("_llm$", names(data), value = T)
    )

    temp <- NULL

    log_dir <- "log_files"
    temp_log_file <- tempfile(pattern = "user_log_", fileext = ".csv")

    if (!dir.exists(log_dir)) {
      dir.create(log_dir)
      message("Folder created: ", log_dir)
    } else {
      message("Folder already exists: ", log_dir)
    }
  })

  # Display selected data
  output$selected_data <- DT::renderDT({
    req(current_data(), input$selected_vars)
    df <- current_data()[current_row(), input$selected_vars, drop = FALSE]
    show_datatable(df, 1)
  })

  output$variable_check <- renderText({
    req(current_data(), input$var_llm)

    paste("Manual Variable:", sub("_llm$", "_manual", input$var_llm))
  })

  output$check_value <- renderText({
    req(current_data(), input$var_llm)

    data <- current_data()

    current <- current_row()

    check_var <- sub("_llm$", "_manual", input$var_llm)

    if (!check_var %in% names(data)) {
      return("Manual Value: <missing column>")
    }
    paste("Manual Value:", data[current, check_var])
  })

  output$check_value <- renderText({
    req(current_data(), input$var_llm)

    data <- current_data()

    current <- current_row()

    check_var <- sub("_llm$", "_manual", input$var_llm)

    paste("Manual Value:", data[current, check_var])
  })

  # Navigate to previous row
  observeEvent(input$pre_btn, {
    req(current_data())
    current <- current_row()
    if (current > 1) {
      current_row(current - 1)
    }
  })

  # Navigate to next row
  observeEvent(input$next_btn, {
    req(current_data())
    current <- current_row()
    if (current < nrow(current_data())) {
      current_row(current + 1)
    }
  })

  # use LLM

  observeEvent(input$useLLM, {
    req(
      input$useLLM,
      current_data(),
      input$LLM_prompt,
      input$abstract_col,
      input$extraction_type
    )

    # browser()

    df <- current_data()[current_row(), input$abstract_col, drop = FALSE]

    current <- current_row()

    if (input$extraction_type == "integer") {
      type_abstract <- ellmer::type_object(
        "Extraction from abstract.",
        !!input$var_llm := ellmer::type_integer(input$LLM_prompt, required = FALSE)
      )
    } else if (input$extraction_type == "number") {
      type_abstract <- ellmer::type_object(
        "Extraction from abstract.",
        !!input$var_llm := ellmer::type_number(input$LLM_prompt, required = FALSE)
      )
    } else if (input$extraction_type == "boolean") {
      type_abstract <- ellmer::type_object(
        "Extraction from abstract.",
        !!input$var_llm := ellmer::type_boolean(input$LLM_prompt, required = FALSE)
      )
    } else if (input$extraction_type == "string") {
      type_abstract <- ellmer::type_object(
        "Extraction from abstract.",
        !!input$var_llm := ellmer::type_string(input$LLM_prompt, required = FALSE)
      )
    }

    data <- current_data()

    chat <- ellmer::chat_ollama(
      model = input$model_name,
      seed = 1,
      api_args = list(temperature = 0)
    )

    processed <- lapply(df[[input$abstract_col]], function(abstract) {
      bot <- chat$clone()
      bot$extract_data(abstract, type = type_abstract)
    })

    data[current, input$var_llm] <- ifelse(
      is.null(processed[[1]][[1]]),
      NA,
      processed[[1]][[1]]
    )

    current_data(data)

    abstract <- df[[input$abstract_col]]
    user_prompt <- input$LLM_prompt
    model <- input$model_name
    results <- ifelse(is.null(processed[[1]][[1]]), NA, processed[[1]][[1]])
    # also check value, make sure not NA
    # check see if the data checked aganist is the same with LLM value type

    # browser()

    check_var <- sub("_llm$", "_manual", input$var_llm)

    correct <- results == data[current, check_var]

    # Append the input and output to the log file with a timestamp
    log_entry <- data.frame(
      Timestamp = Sys.time(),
      abstract = abstract,
      model = model,
      prompt = user_prompt,
      results = results,
      correct = correct,
      stringsAsFactors = FALSE
    )

    # color prompt box
    # For this color, I want it to reset back to blank when it proceed to the next row.

    # additionally, I want a check box to show the percentage of the correctness for the text later.

    if (is.na(data[current, check_var])) {
      showNotification("Manual extraction value missing.")
    } else if(is.na(results == data[current, check_var])){
      showNotification(
        HTML("<span style='font-size: 20px; font-weight: bold; color: red;'>No value was extracted by LLM</span>"),
        duration = 5,
        type = "message"  # or "warning", "error"
      )
      shinyjs::runjs(
        'document.getElementById("LLM_prompt").style.backgroundColor = "lightgrey";'
      )
    } else if (
      ifelse(
        is.na(results == data[current, check_var]),
        FALSE,
        results == data[current, check_var]
      )
    ) {
      shinyjs::runjs(
        'document.getElementById("LLM_prompt").style.backgroundColor = "lightgreen";'
      )
    } else {
      # Change background color to red
      shinyjs::runjs(
        'document.getElementById("LLM_prompt").style.backgroundColor = "lightcoral";'
      )
    }

    write.table(
      log_entry,
      temp_log_file,
      sep = ",",
      append = TRUE,
      col.names = !file.exists(temp_log_file),
      row.names = FALSE
    )
  })

  # Display current row indicator
  output$row_indicator <- renderText({
    req(current_data())
    sprintf("Row %d of %d", current_row(), nrow(current_data()))
  })

  # Downloadable csv of selected dataset
  volumes <- get_download_locations()

  shinyFileSave(input, "saveFile", roots = volumes, session = session)

  observeEvent(input$saveFile, {
    fileinfo <- parseSavePath(volumes, input$saveFile)

    if (nrow(fileinfo) > 0) {
      filepath <- paste0(
        fileinfo$datapath,
        "_",
        Sys.Date(),
        "_data_step_2",
        ".csv"
      )
      if (!grepl("\\.csv$", filepath)) {
        filepath <- paste0(
          fileinfo$datapath,
          "_",
          Sys.Date(),
          "_data_step_2",
          ".csv"
        ) # Ensure .csv extension
      }
      write.csv(current_data(), filepath, row.names = FALSE)
      showNotification(paste("File saved to:", filepath), type = "message")
    }
  })
  # Register a session end callback to move the log file
  session$onSessionEnded(function() {
    # Define the final log file path
    final_log_file <- file.path(
      log_dir,
      paste0("user_log_", Sys.time(), ".csv")
    )

    # Move the temporary log file to the final destination
    if (file.exists(temp_log_file)) {
      file.rename(temp_log_file, final_log_file)
    }
  })

  output$download_sample <- downloadHandler(
    filename = function() {
      "sample_data_app2.csv"
    },
    content = function(file) {
      write.csv(metaextractoR::app_2, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
