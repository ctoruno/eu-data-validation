## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- ESS Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 28th, 2023
##
## This version:      October 4th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
library(caret)

#SharePoint path

if (Sys.info()["user"]=="Dhabiby"){
  
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics/")
} 

ess<- read_dta(paste0(path2SP, "8. Data/TPS/ESS/ESS_raw.dta"))
ess<- read_dta(paste0(path2SP, "8. Data/TPS/ESS/ESS-Data-Wizard-subset-2023-08-09.dta"))

unique(ess$cntry)
ESS_clean<- function(df){
  
  unique(ess$name)
  
  df<- ess%>%
    filter(name %in% c("ESS10e03_1", "ESS10SCe03"))
  
  df%>%
    count(cntry)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars<- c("cntry", "contplt", "donprty", "pbldmna", "pstplonl", "volunfp", "medcrgv", "medcrgvc", 
                 "fairelc", "dfprtal", "votedir", "gptpelc", "fairelcc", "dfprtalc", "votedirc", "gptpelcc")
  
  #, "dweight", "pspwght", "pweight", "anweight", "prob"
  
  
  cy<-  c("AT", "BE", "BG", "CY", "CZ", "DE", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
          "LT", "LV", "NL", "PL", "PT", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data=======================================================================================
  
  
  df2<- df%>%
    filter(cntry %in% cy)%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro<- c()
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                         ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  
  no<- setdiff(targetvars[-1], ro)
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(cntry)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IT", "LT", "LV", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean1<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cy, nuts))[cntry], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-cntry)
  
  clean<- clean1[-2]
  
  return(clean)
  
}

#ESS_clean(ess)

