## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            TPS Data Merge - master
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 4th, 2023
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

# Please fill the sources to be updated
data2update <- c("FIW", "VDM")

# Loading settings
source("Code/settings.R")

# Loading specific cleaning routines
lapply(data2update, 
       function(source_acronym){
         
         routine_path <- file.path("Cleaning",
                                   source_acronym,
                                   paste0(source_acronym, "_function.R"),
                                   fsep = "/")
         source(routine_path)
         
       })

# Creating an empty list to store raw and clean data
raw_data   <- list()
clean_data <- list()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Defining Cleaning Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1 Loading Data ============================================================================================
readMe <- function(acronym){
  
  # European Social Survey
  if (acronym == "ESS") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/ESS/ESS_raw.dta",
                               fsep = "/")) 
  }
  
  # Flash Eurobarometer 507
  if (acronym == "FLE_507") {
    rdata <- import_list(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/FLE_507_raw.xlsx",
                               fsep = "/")) 
  }

  # Flash Eurobarometer 519
  if (acronym == "FLE_519") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/FLE_519_raw.dta",
                               fsep = "/")) 
  }
  
  # Flash Eurobarometer 520
  if (acronym == "FLE_520") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/FLE_520_raw.dta",
                               fsep = "/")) 
  }
  
  # Flash Eurobarometer 524
  if (acronym == "FLE_524") {
    rdata <- import_list(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/FLE_524_raw.xlsx",
                               fsep = "/")) 
  }
  
  # Freedom in the World
  if (acronym == "FIW") {
    rdata <- read_xlsx(file.path(path2SP, 
                               "8. Data/TPS/Freedom House/FIW_raw.xlsx",
                               fsep = "/")) 
  }
  
  # Global Corruption Barometer
  if (acronym == "GCB") {
    rdata <- read_dta(file.path(path2SP, 
                                "8. Data/TPS/Global Corruption Barometer/GCB_raw.dta",
                                fsep = "/")) 
  }
  
  # Government Transparency Index
  if (acronym == "GTI") {
    rdata <- import_list(file.path(path2SP, 
                                 "8. Data/TPS/Government Transparency/GTI_raw.xlsx",
                                 fsep = "/")) 
  }
  
  # Special Eurobarometer 489
  if (acronym == "SPE_489") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/SPE_489_raw.dta",
                               fsep = "/")) 
  }
  
  # Special Eurobarometer 502
  if (acronym == "SPE_502") {
    rdata <- read_dta(file.path(path2SP, 
                                "8. Data/TPS/Eurobarometer/SPE_502_raw.dta",
                                fsep = "/")) 
  }
  
  # Special Eurobarometer 507
  if (acronym == "SPE_507") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/SPE_507_raw.dta",
                               fsep = "/")) 
  }
  
  # Special Eurobarometer 523
  if (acronym == "SPE_523") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Eurobarometer/SPE_523_raw.dta",
                               fsep = "/")) 
  }
  
  # Special Eurobarometer 534
  if (acronym == "SPE_534") {
    rdata <- import_list(file.path(path2SP, 
                                   "8. Data/TPS/Eurobarometer/SPE_534_raw.xlsx",
                                   fsep = "/")) 
  
  # World Values Survey (+European Values Survey)
  if (acronym == "WVS") {
    rdata <- read_dta(file.path(path2SP, 
                               "8. Data/TPS/Freedom House/WVS_raw.dta",
                               fsep = "/")) 
  }
  
  # V-Dem
  if (acronym == "VDM") {
    rdata <- readRDS(file.path(path2SP, 
                               "8. Data/TPS/V-Dem/VDM_raw.rds",
                               fsep = "/")) 
  }
  
  return(rdata)
}
  
## 1.2 Cleaning Data ===========================================================================================

applyMe <- function(acronym, df) {
  
  # Function to locate
  func_name <- paste0(acronym, "_clean")
  
  # Check if the function exists
  if (exists(func_name, mode = "function")) {
    
    print(paste("Applying function:",
                func_name))
    result <- do.call(func_name, 
                      args = list("df" = df))
    return(result)
    
  } else {
    stop(paste("Function", func_name, "not found"))
    
  }
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Data Cleaning Routines                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Applying loading and cleaning functions to data sources
lapply(data2update, 
       function(src){
         
         # Step 1: Read
         raw_data[[src]]   <<- readMe(src)
         
         # Step 2: Clean
         clean_data[[src]] <<- applyMe(src, df = raw_data[[src]])
         
         # Step 3: Save
         write.csv(clean_data[[src]],
                   file.path(
                     "Cleaning",
                     src,
                     paste0(src, "_clean.csv"),
                     fsep = "/"
                   ))
         
       })


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Merging Data Sources                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# general_info <- read_csv("../../GPP/0. Metadata/EU_general_information.csv") %>%
#   left_join(
#     purrr::reduce(
#       clean_data,
#       left_join,
#       by = "Country"
#     ),
#     by = c("country_code_nuts" = "country")
#   )
  
