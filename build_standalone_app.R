# ==============================================================================
# BUILD SCRIPT: Standalone Windows Executable (.exe) for Shiny App
# ==============================================================================
# This script uses RInno to bundle the Shiny app, all necessary R packages, 
# and a portable version of R into a single Windows installer (.exe).
# 
# Users who install the .exe will NOT need R or RStudio installed on their PCs!
# ==============================================================================

message("1. Checking for remotes package...")
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

message("2. Installing RInno from GitHub...")
if (!requireNamespace("RInno", quietly = TRUE)) {
  remotes::install_github("ficonsulting/RInno")
}
library(RInno)

message("3. Checking for Inno Setup compiler...")
# This will download and install Inno Setup if it's missing.
# NOTE: You may see a Windows Admin Prompt (UAC). Please click "Yes".
tryCatch({
  RInno::install_inno()
}, error = function(e) {
  message("Inno Setup already installed or error: ", e$message)
})

message("4. Configuring standalone app package...")
create_app(
  app_name = "GDP_Nowcasting_Engine",
  app_dir = getwd(),
  pkgs = c(
    "shiny", "bslib", "shinycssloaders", "DT", "dplyr", "tidyr", 
    "ggplot2", "zoo", "readxl", "purrr", "tseries", "forecast", 
    "seasonal", "lubridate", "openxlsx", "readr", "dfms", "xts", "xgboost"
  ),
  include_R = TRUE,      # This is the magic that bundles R-Portable!
  R_version = paste(R.version$major, R.version$minor, sep = "."),
  privilege = "lowest",  # Allows users without admin rights to install the app
  default_dir = "userdocs"
)

message("5. Compiling .exe installer... This will take a few minutes...")
compile_iss()

message("==================================================================")
message("SUCCESS! The installer is ready.")
message("Look for the 'RInno_installer' folder in your project directory.")
message("You can send the .exe inside that folder to anyone!")
message("==================================================================")
