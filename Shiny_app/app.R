library(shiny)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
source("helpers.R")

# ==============================================================================
# GLOBAL SETUP
# ==============================================================================
# Load the static background data required for X13 seasonal adjustments once
td <- read_csv("data/raw/td_var.csv", show_col_types = FALSE)
td_ts <- ts(td[, -1], start = c(year(min(td$date)), month(min(td$date))), frequency = 12)

preadj <- read_excel("data/raw/hol_preadj.xlsx") %>% mutate(date = as.Date(date))
hag_ts <- ts(preadj[, -1], start = c(year(min(preadj$date)), month(min(preadj$date))), frequency = 12)

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("GDP Nowcasting & Bridge Model"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("raw_data", "1. Upload Raw Data (Excel)",
                accept = c(".xlsx")),
      
      actionButton("run_btn", "2. Run Pipeline", class = "btn-primary", style = "width: 100%; margin-bottom: 20px;"),
      
      uiOutput("download_ui")
    ),
    
    mainPanel(
      h4("Execution Logs"),
      verbatimTextOutput("logs_output", placeholder = TRUE)
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive values to hold logs and workbook
  rv <- reactiveValues(
    logs = character(0),
    report_wb = NULL
  )
  
  # Create a temporary file to hold logs for real-time streaming
  log_file <- tempfile("shiny_logs_", fileext = ".txt")
  file.create(log_file)
  
  # Helper to append logs to both reactive value and the physical file
  append_log <- function(msg) {
    formatted_msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
    rv$logs <- c(rv$logs, formatted_msg)
    cat(paste0(formatted_msg, "\n"), file = log_file, append = TRUE)
  }
  
  # Read the log file every 500ms to update the UI while the main thread is blocked
  log_data <- reactiveFileReader(500, session, log_file, readLines)
  
  output$logs_output <- renderText({
    paste(log_data(), collapse = "\n")
  })
  
  # Observe Run Button
  observeEvent(input$run_btn, {
    req(input$raw_data)
    
    # Reset states and log file
    rv$logs <- character(0)
    rv$report_wb <- NULL
    file.create(log_file) # Clear the file
    
    # Wrap entire execution in a tryCatch to log errors gracefully
    tryCatch({
      
      withProgress(message = 'Processing...', value = 0, {
        
        # 1. Transformations
        append_log(">>> STEP 1: Running Transformations")
        trans_res <- run_transformations(
          raw_data_path = input$raw_data$datapath,
          td_ts = td_ts,
          hag_ts = hag_ts,
          update_log = append_log,
          update_progress = function(val, msg) { incProgress(val * 0.33, detail = msg) }
        )
        
        # 2. DFM & XGBoost
        append_log(">>> STEP 2: Running DFM & XGBoost")
        dfm_xgb_res <- run_dfm_xgboost(
          combined_panel = trans_res$combined_panel,
          target_raw = trans_res$target_raw,
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.33 + (val * 0.33), detail = msg) }
        )
        
        # 3. Report Generation
        append_log(">>> STEP 3: Generating Final Report")
        wb <- generate_report(
          models_res = dfm_xgb_res,
          blocks_shifted = trans_res$blocks_shifted,
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.66 + (val * 0.33), detail = msg) }
        )
        
        rv$report_wb <- wb
        append_log(">>> PIPELINE COMPLETED SUCCESSFULLY")
        setProgress(1, detail = "Done!")
      })
      
    }, error = function(e) {
      append_log(paste("ERROR:", e$message))
    })
  })
  
  # Conditionally show the download button only when report_wb is available
  output$download_ui <- renderUI({
    if (!is.null(rv$report_wb)) {
      downloadButton("download_report", "3. Download Excel Report", class = "btn-success", style = "width: 100%;")
    }
  })
  
  # Download Handler for the generated Excel workbook
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Nowcast_Executive_Report_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$report_wb)
      openxlsx::saveWorkbook(rv$report_wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
