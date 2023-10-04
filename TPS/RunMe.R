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
  
  # Freedom in the World
  if (acronym == "FIW") {
    rdata <- read_xlsx(file.path(path2SP, 
                                 "8. Data/TPS/Freedom House/FIW_raw.xlsx",
                                 fsep = "/")) 
  }
  
  # V-Dem
  if (acronym == "VDM") {
    rdata <- readRDS(file.path(path2SP, 
                               "8. Data/TPS/V-Dem/VDM_raw.rds",
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
  
