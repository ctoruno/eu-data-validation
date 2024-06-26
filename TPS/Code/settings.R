## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Pre-settings
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 3rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

p_load(char = c(
  
  # Data Loading
  "haven", "readxl", "writexl", "rio",
  
  # Utilities
  "caret",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "ctoruno") {
  path2SP <- paste0("/Users/ctoruno/OneDrive - World Justice Project/Data Analytics")
  path2eu <- paste0("/Users/ctoruno/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics")
  path2eu <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/EU Subnational")

} else if (Sys.info()["user"]=="Dhabiby"){
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics")
  path2eu<- paste0("/Users/Dhabiby/World Justice Project/Research - EU Subnational")
  
} else if (Sys.info()["user"] == "nclapacs") {
  path2SP <- paste0("/Users/nclapacs/OneDrive - World Justice Project/Data Analytics")
  path2eu <- paste0("/Users/nclapacs/OneDrive - World Justice Project/EU Subnational")
  
} else {
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
  path2eu <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - EU SUBNATIONAL DIRECTORY"
  
}

nuts <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
          "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
          "RO", "SE", "SI", "SK")
