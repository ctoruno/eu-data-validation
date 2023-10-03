## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- WVS Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 2nd, 2023
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
##                1.  Call Libraries and Data                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
library(caret)
library(haven)

#SharePoint path

if (Sys.info()["user"]=="Dhabiby"){
  
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics/")
} 

wvs<- read_dta(paste0(path2SP, "8. Data/TPS/WVS/WVS_raw.dta"))
# try EVS/WVS data instead


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Define Function                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#use reg_nuts_1 ! -1 and -4, group by country find largest value of year
#leave a year column in clean data


WVS_clean<- function(df){
  
  df<- wvs
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                3.  Identify Indicators of Interest                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  targetvars<- c("COUNTRY_ALPHA", "E069_64", "E265_01", "E265_02", "E265_03", "E265_04", 
                 "E265_05", "E265_06", "E265_07", "E265_08", "E265_09", "E236", "E276")
  
  cntry<- c("AUT", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "GRC", "ESP", "FIN", "FRA", "HRV", "HUN", "ITA", "LTU", 
            "LVA", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK")
  
  
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                4.  Select Indicators of Interest                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  dfyr<- df%>%
    filter(COUNTRY_ALPHA %in% cntry)%>%
    group_by(COUNTRY_ALPHA)%>%
    summarise(year= max(S020))
  
  dfyr
    
    
    # filter(S020 == 2019)
    # select(all_of(targetvars))
  
 # unique(dfv$COUNTRY_ALPHA)
  
  
  dfv%>%
    group_by(COUNTRY_ALPHA)%>%
    summarise(yr = S020)%>%
    distinct
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                5.  Reorient Indicator Coding                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  oriented<- dfv
  
  #Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient.
  for(i in c("E069_64", "E265_05", "E265_06", "E265_09")){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                              ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
    
  }
  
  
  #put any variable that does not need to be reoriented here
  
  for(i in c("E265_01", "E265_02", "E265_03", "E265_04", "E265_07", "E265_08", "E236", "E276")){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                6.  Normalize Values from 0-1                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
    
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                7.  Aggregate to One Score per Country                                                    ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  print(dfv)
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                8.  Write Clean Dataset                                                                   ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  
}

#WVS_clean(ess)

