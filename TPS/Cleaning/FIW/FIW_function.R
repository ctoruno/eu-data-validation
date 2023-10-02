## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FIW Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 29th, 2023
##
## This version:      September 29th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Call Libraries and Data                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
library(readxl)
library(caret)


#SharePoint path

if (Sys.info()["user"]=="Dhabiby"){
  
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics/")
} 

fiw<- read_xlsx(paste0(path2SP, "8. Data/TPS/Freedom House/FIW_raw.xlsx"))



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Define Function                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



FIW_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                3.  Identify Indicators of Interest                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  targetvars<- c("Country/Territory", "A", "A1", "A2", "A3", "B", "B1", "B2", "D", "D1", "D2", "E",
                "E1", "E2", "E3", "F", "PR", "CL")
  
  
  cntry<- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", 
            "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", 
            "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                4.  Select Indicators of Interest                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  dfv<- df%>%
    filter(Edition == 2023)%>%
    filter(`Country/Territory` %in% cntry)%>%
    select(all_of(targetvars))
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                5.  Reorient Indicator Coding                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                6.  Normalize Values from 0-1                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dfv[nrow(dfv) + 1,] <- c("mins", rep(list(0), ncol(dfv)-1))
  
  process<- preProcess(dfv, method = c("range"))
  normalized <- predict(process, dfv)
  
  dfv2<- slice(normalized, 1:(n() - 1))
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                7.  Aggregate to One Score per Country                                                    ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #not needed
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                8.  Write Clean Dataset                                                                   ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  

  dfv2$Country<- rep(NA, nrow(dfv2))
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  
  
  clean<- dfv2%>%
    mutate(Country = case_when(is.na(Country) ~ 
                               deframe(tibble(cntry, nuts))[`Country/Territory`], 
                             TRUE ~ Country))%>%
    select(Country, )
  
  write.csv(clean, "FIW_clean.csv")
}

FIW_clean(fiw)

