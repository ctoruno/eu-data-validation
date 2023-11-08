## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Country data validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##                    Carlos A. ToruC1o Paniagua   (ctoruno@worldjusticeproject.org)
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
source("Code/missing_values.R")

# Please fill the country code to be validated

country_name <- "Greece"
country_ind <- "EL"
country <- "Greece"
type<- "real"

# List of chosen analyses (add/remove as needed)
type_data <- "pretest"

master_data.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/1. PTR/", 
                                         country_name,
                                         "/1. Clean Data", 
                                         "/",
                                         country_name, "_clean.dta"))

GPP_previous.df <- haven::read_dta(paste0("Input/eu_merge.dta")) 

TPS.df <- read_csv("Input/TPS_data.csv")

codebook.df <- read_excel("Input/EU2 GPP 2023 Codebook.xlsx")
matched_tps<- suppressMessages(import_list("Input/Selected GPP&TPS for QCC.xlsx"))
matched_tps<- matched_tps$`Selection and matching`
variable_list.df <- match_indicators()
sampling_plans.df <- read_excel("Input/Sampling_plan_integrated.xlsx") %>%
  filter(country_code %in% country_ind)

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
                                type= "real")

tps_comparisson.df <- TPS_function(country = country,
                                   gpp = master_data.df,
                                   tps = TPS.df,
                                   mat = matched_tps,
                                   type = "real")

#sociodem_comparisson.df <- sociodem_comparisson()

#missing_values.df<- missing_values(data= master_data.df, 
#                                   country= country)

# List of analysis functions

if(type_data == "pretest") {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df
    #sociodem_comparisson = sociodem_comparisson.df
    )
  
} else {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df,
    tps_trend_comparisson = tps_trend_comparisson.df,
    sociodem_comparisson = sociodem_comparisson.df
  )
  
}

analysis.list <- analysis_functions


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

openxlsx::write.xlsx(analysis.list,
                     paste0("Outcomes/Pretest/",
                            country,
                            "/",
                            country,
                            ".xlsx"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Outcomes function                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

countries<- c("Greece")
authors<- c("Santiago Pardo")

report<- function(ctry, auth){
  
  rmarkdown::render("./Code/Country Report Test.Rmd", 
                    params = list(country = ctry, author = auth, date= Sys.Date()),
                    output_file=paste0(ctry, " Validation Report", ".html"),
                    output_dir = paste0("./Outcomes/Pretest/", ctry))
}

for (i in 1:length(countries)){
  
  report(countries[[i]], authors[[i]])
  
}

