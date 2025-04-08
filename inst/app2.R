library(shiny)
library(bslib)
library(DT)
library(ellmer)
library(purrr)
library(shinyjs)
library(shinyFiles)


# Extract unique prefixes
get_prefix <- function(name) {
  sub("_.*", "", name)
}
prefixes <- unique(sapply(names(df), get_prefix))


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  includeCSS("www/checkbox.css"),
  includeCSS("www/correct.css"),

  page_sidebar(

    title = "Prompt Engineering",

    sidebar = sidebar(
      width = 500,  # Increased sidebar width

      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                width = "120%"),

      actionButton(inputId = "upload_data", label = "Upload abstracts"),
      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE, width = "120%"),

      selectInput("selected_vars", "Select columns", choices = NULL, multiple = TRUE),

      card(
        card_header("Use LLM for data extraction"),
        selectInput("var_llm", "Select LLM variable column", choices = NULL),
        # [TODO] perhaps this should be free text to allow user to chose their own?
        # The shiny function in the R file can have a default choice.
        selectInput("model_name","Model Name",
                    choices = c("llama3.1:8b", "llama3", "medllama2" = "medllama2", "nuextract")),
        selectInput("extraction_type", "Type of Extraction Element",
                    choices = c(
                      "Integer" = "integer",
                      "Double" = "number",
                      "Binary" = "boolean",
                      "Text" = "string"
                    )

        ),
        textAreaInput("LLM_prompt","Prompt for LLM extraction",height = "100px"),

        selectInput("abstract_col", "Select Abstract column", choices = NULL),

        textOutput("variable_check"),

        textOutput("check_value"),

        textOutput("equalCheck"),

        actionButton("useLLM", "Create LLM Variable")
      )),


    # Main panel with data table output and edit interface
    mainPanel(
      width = 1500,
      h3("Data Preview"),
      div(style = "height: 800px; overflow-y: auto;",
          DTOutput("selected_data")),
      textOutput("row_indicator"),
      textOutput("output_newvar_name"),
      textOutput("output_newvar_value"),
      fluidRow(
        column(6, actionButton("pre_btn","Previous")),
        column(6,  actionButton("next_btn","Next"))
      ),
      shinySaveButton("saveFile", "Save CSV", "Save as...")

    ),

  ))

# SERVER ------------------------------------------------------------------


server <- function(input, output,session) {

  # create log file in the background
  # Don't know how
  temp <- NULL

  log_dir <- "log_files"

  temp_log_file <- tempfile(pattern = "user_log_", fileext = ".csv")

  current_data <- reactiveVal()
  current_row <- reactiveVal(1)

  observe({
    temp <<- reactiveValues(
      model = input$model_name,
      prompt = input$LLM_prompt,
      row = current_row(),
    )
    # print("reached observe")
    runjs('console.log("reached");console.log(document.getElementById("LLM_prompt").style.backgroundColor);document.getElementById("LLM_prompt").style.backgroundColor = "white";')

  })

  observe({
    req(input$file1)

    data <- read.csv(input$file1$datapath,
                     header = input$header)
    current_data(data)
    updateSelectInput(session, "abstract_col",choices = names(data))
    updateSelectInput(session, "selected_vars", choices = names(data))
    updateSelectInput(session,"var_llm", choices = grep("_llm$", names(data),value = T))
    #updateSelectInput(session, "value_check",choices = grep("_manual$",names(data) ,value = T))

    # pattern_check <- sub("_llm$", "_manual", input$var_llm)

    #  updateSelectInput(session, "value_check",choices =  grep(pattern_check, names(data),value = T))

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

  output$variable_check <- renderText({

    req(current_data(),input$var_llm)

    paste("Manual Variable:", sub("_llm$", "_manual", input$var_llm))
  })


  output$check_value <- renderText({

    req(current_data(),input$var_llm)

    data <- current_data()

    current <- current_row()

    check_var <-sub("_llm$", "_manual", input$var_llm)

    paste("Manual Value:", data[current,check_var])
  })


  output$check_value <- renderText({

    req(current_data(),input$var_llm)

    data <- current_data()

    current <- current_row()

    check_var <-sub("_llm$", "_manual", input$var_llm)

    paste("Manual Value:", data[current,check_var])
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
    req(input$useLLM, current_data(),input$LLM_prompt,input$abstract_col,input$extraction_type )

    # browser()

    df <- current_data()[current_row(), input$abstract_col, drop = FALSE]

    current <- current_row()

    if(input$extraction_type == "integer"){

      type_abstract <- type_object("Extraction from abstract.",
                                   !!input$var_llm := type_integer(input$LLM_prompt,required = FALSE))

    } else if(input$extraction_type == "number"){
      type_abstract <- type_object("Extraction from abstract.",
                                   !!input$var_llm := type_number(input$LLM_prompt,required = FALSE))
    } else if(input$extraction_type == "boolean"){
      type_abstract <- type_object("Extraction from abstract.",
                                   !!input$var_llm := type_boolean(input$LLM_prompt,required = FALSE))
    } else if (input$extraction_type == "string"){
      type_abstract <- type_object("Extraction from abstract.",
                                   !!input$var_llm := type_string(input$LLM_prompt,required = FALSE))
    }

    data <- current_data()

    chat <- chat_ollama(model = input$model_name,
                        seed = 1,
                        api_args = list(temperature = 0))


    processed <- map(df[[input$abstract_col]], function(abstract) {
      bot <- chat$clone()
      bot$extract_data(abstract, type = type_abstract)
    })

    data[current,input$var_llm]  <- processed[[1]][[1]]

    current_data(data)

    abstract <- df[[input$abstract_col]]
    user_prompt <- input$LLM_prompt
    model <- input$model_name
    results <- processed[[1]][[1]]
    # also check value, make sure not NA
    # check see if the data checked aganist is the same with LLM value type

    # browser()

    check_var <-sub("_llm$", "_manual", input$var_llm)

    correct <- processed[[1]][[1]] ==  data[current,check_var]


    # Append the input and output to the log file with a timestamp
    log_entry <- data.frame(Timestamp = Sys.time(),
                            abstract = abstract,
                            model = model,
                            prompt = user_prompt,
                            results = results,
                            correct = correct,
                            stringsAsFactors = FALSE)

    # color prompt box
    # For this color, I want it to reset back to blank when it proceed to the next row.

    # additionally, I want a check box to show the percentage of the correctness for the text later.

    if(is.na(data[current,check_var])){

      showNotification("Manual extraction value missing.")
    } else if( processed[[1]][[1]] ==  data[current,check_var]) {
      runjs('document.getElementById("LLM_prompt").style.backgroundColor = "lightgreen";')
    } else {
      # Change background color to red
      runjs('document.getElementById("LLM_prompt").style.backgroundColor = "lightcoral";')
    }


    write.table(log_entry, temp_log_file, sep = ",", append = TRUE,
                col.names = !file.exists(temp_log_file), row.names = FALSE)

  })


  # Display current row indicator
  output$row_indicator <- renderText({
    req(current_data())
    sprintf("Row %d of %d", current_row(), nrow(current_data()))
  })


  #
  #  # Observe changes in the text input and update the background color
  #  observe({
  #    # Compare the user input with the values in Column1 and Column2
  #
  # # # #   browser()
  # # #
  #    if ( processed[[1]][[1]] ==  data[current_data,input$value_check]) {
  #      updateTextInput(session, "LLM_prompt", class = "correct")
  #    } else {
  #      updateTextInput(session, "LLM_prompt", class = "incorrect")
  #    }
  #  })
  # #
  # Downloadable csv of selected dataset
  volumes <- c(Home = fs::path_home(), "Downloads" = fs::path_home("Downloads"))

  shinyFileSave(input, "saveFile", roots = volumes, session = session)

  observeEvent(input$saveFile, {
    fileinfo <- parseSavePath(volumes, input$saveFile)

    if (nrow(fileinfo) > 0) {
      filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_2",".csv")
      if (!grepl("\\.csv$", filepath)) {
        filepath <- paste0(fileinfo$datapath, "_",Sys.Date(),"_data_step_2",".csv")  # Ensure .csv extension
      }
      write.csv(current_data(), filepath, row.names = FALSE)
      showNotification(paste("File saved to:", filepath), type = "message")
    }
  })
  # Register a session end callback to move the log file
  session$onSessionEnded(function() {
    # Define the final log file path
    final_log_file <- file.path(log_dir, paste0("user_log_", Sys.time(), ".csv"))

    # Move the temporary log file to the final destination
    if (file.exists(temp_log_file)) {
      file.rename(temp_log_file, final_log_file)
    }
  })


}

# Run the application
shinyApp(ui = ui, server = server)




