# ==============================================================================
# GDP Nowcasting Application (DFM + XGBoost Ensemble)
# ==============================================================================

options(shiny.autoload.r=FALSE)
library(shiny)
library(bslib)
library(shinycssloaders)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(readxl)
library(purrr)
library(tseries)
library(forecast)
library(seasonal)
library(lubridate)
library(openxlsx)
library(readr)
library(dfms)
library(xts)
library(xgboost)

# --- UI Definition ---
ui <- page_navbar(
  title = "Macroeconomic Nowcasting Engine",
  theme = bs_theme(version = 5, preset = "lux"),
  
  sidebar = sidebar(
    width = 350,
    h4("Model Configuration"),
    fileInput("upload_data", "Upload Raw Excel Data (e.g., nowcasting_data_raw_new.xlsx)", 
              accept = c(".xlsx")),
    
    numericInput("w_xgb", "XGBoost Ensemble Weight", value = 0.60, min = 0, max = 1, step = 0.05),
    numericInput("w_dfm", "DFM Ensemble Weight", value = 0.40, min = 0, max = 1, step = 0.05),
    
    actionButton("run_model", "Execute Nowcast Pipeline", class = "btn-primary btn-lg mt-3", width = "100%"),
    
    hr(),
    h5("Output"),
    downloadButton("download_report", "Download Excel Report", class = "btn-success mt-2", width = "100%")
  ),
  
  nav_panel("Executive Summary",
            fluidRow(
              value_box(
                title = "Current GDP Nowcast (Growth)",
                value = textOutput("vb_nowcast_growth"),
                showcase = bsicons::bs_icon("graph-up-arrow"),
                theme = "primary"
              ),
              value_box(
                title = "Ensemble RMSE",
                value = textOutput("vb_rmse"),
                showcase = bsicons::bs_icon("bullseye"),
                theme = "info"
              ),
              value_box(
                title = "Ensemble MAE",
                value = textOutput("vb_mae"),
                showcase = bsicons::bs_icon("rulers"),
                theme = "secondary"
              )
            ),
            card(
              card_header("Nowcast vs Actual GDP Growth"),
              withSpinner(plotOutput("plot_historical", height = "400px"))
            )
  ),
  
  nav_panel("Drivers & News Report",
            fluidRow(
              column(6, card(
                card_header("Sector Contributions"),
                withSpinner(plotOutput("plot_sectors", height = "350px"))
              )),
              column(6, card(
                card_header("Top Variable Drivers"),
                withSpinner(plotOutput("plot_vars", height = "350px"))
              ))
            ),
            card(
              card_header("Detailed Impact Table"),
              withSpinner(DTOutput("table_impacts"))
            )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive values to store results
  rv <- reactiveValues(
    nowcast_growth = NA,
    rmse = NA,
    mae = NA,
    news_report = NULL,
    sector_report = NULL,
    plot_hist = NULL,
    plot_sec = NULL,
    plot_var = NULL,
    wb = NULL
  )
  
  observeEvent(input$run_model, {
    req(input$upload_data)
    
    # Check weights
    if (abs(input$w_xgb + input$w_dfm - 1.0) > 0.001) {
      showNotification("Warning: Ensemble weights do not sum to 1.0", type = "warning")
    }
    
    showModal(modalDialog("Running Pipeline... This may take a few minutes.", footer=NULL))
    
    tryCatch({
      
      # 1. Copy uploaded file to the raw data path so transformations.r finds it
      file.copy(input$upload_data$datapath, "data/raw/nowcasting_data_raw_new.xlsx", overwrite = TRUE)
      
      # 2. Setup isolated environment to hold everything
      my_env <- new.env(parent = globalenv())
      my_env$w_xgb <- input$w_xgb
      my_env$w_dfm <- input$w_dfm
      
      # 3. Source the pipeline sequentially
      source("R/transformations.r", local = my_env)
      source("DFM_V2.R", local = my_env)
      source("R/report_V4.R", local = my_env)
      
      # 4. Calculate Ensemble RMSE and MAE across backtest
      dfm_historical <- rowSums(sweep(my_env$X_test_mat, MARGIN = 2, my_env$gdp_loadings, `*`))
      dfm_fcst <- (dfm_historical * my_env$gdp_sd) + my_env$gdp_mean
      
      ensemble_fcst <- (input$w_xgb * my_env$pred_test) + (input$w_dfm * dfm_fcst)
      
      my_env$results$Ensemble_FCST <- NA
      my_env$results$Ensemble_FCST[my_env$idx_forecast] <- ensemble_fcst
      
      oos_results <- my_env$results %>% 
        filter(Date >= my_env$test_start_date, !is.na(GDP), !is.na(Ensemble_FCST))
      
      ens_rmse <- sqrt(mean((oos_results$GDP - oos_results$Ensemble_FCST)^2, na.rm=TRUE))
      ens_mae  <- mean(abs(oos_results$GDP - oos_results$Ensemble_FCST), na.rm=TRUE)
      
      # 5. Extract values to reactive values
      rv$nowcast_growth <- my_env$ensemble_gdp_nowcast
      rv$rmse <- ens_rmse
      rv$mae <- ens_mae
      rv$news_report <- my_env$news_export
      rv$plot_sec <- my_env$p_Sector
      rv$plot_var <- my_env$p_vars
      
      # 6. Extract the actual and ensemble forecast for plotting
      rv$plot_hist <- ggplot() +
        geom_line(data = oos_results, aes(x = Date, y = GDP, color = "Actual GDP"), linewidth = 1) +
        geom_point(data = oos_results, aes(x = Date, y = GDP, color = "Actual GDP"), size = 2) +
        geom_line(data = my_env$results %>% filter(!is.na(Ensemble_FCST)), 
                  aes(x = Date, y = Ensemble_FCST, color = "Ensemble Forecast"), linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = c("Actual GDP" = "black", "Ensemble Forecast" = "red")) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "bottom") +
        labs(x = "", y = "GDP Growth")
      
      # 7. Save workbook for download
      rv$wb <- my_env$wb
      
      removeModal()
      showNotification("Nowcast Pipeline Completed!", type = "message")
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error:", e$message), type = "error", duration = NULL)
    })
  })
  
  # Render Value Boxes
  output$vb_nowcast_growth <- renderText({
    req(!is.na(rv$nowcast_growth))
    sprintf("%.2f%%", rv$nowcast_growth * 100)
  })
  
  output$vb_rmse <- renderText({
    req(!is.na(rv$rmse))
    sprintf("%.4f", rv$rmse)
  })
  
  output$vb_mae <- renderText({
    req(!is.na(rv$mae))
    sprintf("%.4f", rv$mae)
  })
  
  output$plot_historical <- renderPlot({
    req(rv$plot_hist)
    rv$plot_hist
  })
  
  output$plot_sectors <- renderPlot({
    req(rv$plot_sec)
    rv$plot_sec
  })
  
  output$plot_vars <- renderPlot({
    req(rv$plot_var)
    rv$plot_var
  })
  
  output$table_impacts <- renderDT({
    req(rv$news_report)
    datatable(rv$news_report, 
              options = list(pageLength = 15, scrollX = TRUE), 
              rownames = FALSE) %>%
      formatPercentage(c("Impact (%)", "Relative Share (%)"), 2) %>%
      formatRound("Total Impact", 5)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("GDP_Nowcast_Report_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(rv$wb)
      saveWorkbook(rv$wb, file, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui, server)
