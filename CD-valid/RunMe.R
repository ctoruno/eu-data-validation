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
## This version:      October 26th, 2023
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
source("Code/time_changes.R")
source("Code/TPS.R")

# Please fill the country code to be validated

country_name <- "example"
country_ind <- "CZ"
country <- "Czechia"
# List of chosen analyses (add/remove as needed)
type_data <- "pretest"

master_data.df <- haven::read_dta(paste0("Input", 
                                         "/",
                                         country_name, "_clean.dta"))

GPP_previous.df <- haven::read_dta(paste0("Input/eu_merge.dta")) %>%
  mutate(country_code = 
           case_when(
             country_name_ltn %in% "Czechia" ~ "CZ",
             country_name_ltn %in% "Estonia" ~ "EE",
             country_name_ltn %in% "Finland" ~ "FI",
             country_name_ltn %in% "France" ~ "FR",
             country_name_ltn %in% "Slovenia" ~ "SI",
             country_name_ltn %in% "Spain" ~ "ES",
             country_name_ltn %in% "Sweden" ~ "SE"
           ))

TPS.df <- read_csv("Input/TPS_data.csv")

codebook.df <- read_excel("Input/EU2 GPP 2023 Codebook.xlsx")
variable_list.df <- match_indicators()

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

time_changes.df <- time_changes(data = master_data.df,
                                country_code = country_ind)

tps_comparisson <- TPS_function(country = country, 
                                gpp = master_data.df, 
                                tps = TPS.df)


# List of analysis functions
if(type_data == "pretest") {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson
    )
  
} else {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson,
    tps_trend_comparisson = tps_trend_comparisson
  )
  
}

analysis.list <- analysis_functions

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Outcomes function                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (i in c("Czechia", "Estonia", "Finland", "France","Spain", "Sweden", "Slovenia")) {
  rmarkdown::render("./Code/Country Report Test.Rmd", 
                    params = list(country = i),
                    output_file=paste0(i, " Validation Report", ".html"),
                    output_dir = paste0("./Outcomes/Pretest/", i)
                    )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

openxlsx::write.xlsx(analysis.list,
                     paste0("Outcomes/Pretest/",
                     country,
                     "/",
                     country,
                     ".xlsx"))