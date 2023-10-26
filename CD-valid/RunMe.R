## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Country data validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##                    Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 25th, 2023
##
## This version:      October 25th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("Code/settings.R")
source("Code/sociodem.R")

# Please fill the country code to be validated

country_name <- "example"
# List of chosen analyses (add/remove as needed)
analysis <- "pretest"
chosen_analyses <- c("time_changes", "tps_comparisson")

master_data.df <- haven::read_dta(paste0("Input", 
                                         "/",
                                         country_name, "_clean.dta"))
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Defining analysis Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####################################################
# Correspondence patterns
# 
# 1. time_changes = t-test function
# 2. tps_comparisson = TPS function
# 3. tps_trend_comparisson = TPS trends function
#
####################################################


# Define analysis functions

time_changes <- function(data) {
  # Your t-test analysis code here
  # This function should return the analysis result
}

tps_comparisson <- function(data) {
  # Your TPS analysis code here
  # This function should return the analysis result
}

tps_trend_comparisson <- function(data) {
  # Your TPS trend analysis code here
  # This function should return the analysis result
}

# List of analysis functions
if(analysis == "pretest") {
  
  analysis_functions <- list(
    time_changes = time_changes,
    tps_comparisson = tps_comparisson,
    tps_trend_comparisson = tps_trend_comparisson
    )
  
} else {
  
  analysis_functions <- list(
    time_changes = time_changes,
    tps_comparisson = tps_comparisson,
    tps_trend_comparisson = tps_trend_comparisson
  )
  
}


# Use purrr::map to apply the analysis functions and create the analysis.list
analysis.list <- purrr::map(chosen_analyses, ~ {
  if (.x %in% names(analysis_functions)) {
    analysis_functions[[.x]](master_data.df)
  }
})

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Outcomes function                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

openxlsx::write.xlsx(analysis.list, "Outcomes/Pretest/Example/example.xlsx")
