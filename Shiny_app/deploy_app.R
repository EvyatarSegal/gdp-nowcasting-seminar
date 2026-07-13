if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# Authenticate with your shinyapps.io account
rsconnect::setAccountInfo(name=,
			  token=,
			  secret=)

# Deploy the application
message("Deploying the application to shinyapps.io...")
files_to_deploy <- c("app.R", "helpers.R", "transformations.r", "dfm_v2.r", "report_v4.r", "data")
rsconnect::deployApp(appDir = "c:/Users/Evyatar/Documents/nowcasting_shiny_app", appFiles = files_to_deploy, forceUpdate = TRUE)
