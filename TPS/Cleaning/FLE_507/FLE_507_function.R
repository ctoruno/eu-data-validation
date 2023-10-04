## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FLE_503 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
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

suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
library(caret)
library(rio)

#SharePoint path

if (Sys.info()["user"]=="Dhabiby"){
  
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics/")
} 

f507<- import_list(paste0(path2SP, "8. Data/TPS/Eurobarometer/FLE_507_raw.xlsx"))


FLE_507_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  
  targetvars<- c("Q7_6", "Q7_7", "Q7_8", "Q8_1", "Q8_2", "Q8_3")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================

  aggregate<- data.frame(ctrycode = cntry)
  
  for (n in targetvars){
    
    f<- df[[n]]
    
    dfq<- f[c(11,19,22,25,28,31),]
    dfq[1,1]<- "q"
    
    colnames(dfq) <- dfq[1,]
    dfq <- dfq[-1, ] 
    
  ## 1.3 Re-orient indicators ==================================================================================
    
    vals<-dfq[[1]]
    new_vals<- c(1,2/3,1/3,0, NA_real_)
    
  ## 1.4 Normalize indicators ==================================================================================
    
    dfq[[n]] <- as.numeric(rep(NA, nrow(dfq)))
    
    df2<- dfq%>%
      mutate(!!paste0(n) := case_when(is.na(dfq[[n]]) ~ 
                                        deframe(tibble(vals, new_vals))[q], 
                                      TRUE ~dfq[[n]]))%>%
      select(-c(EU27, q))%>%
      pivot_longer(cols = BE:SE, names_to = "ctrycode", values_to = "count")
    
    df2$count<- as.numeric(df2$count)
    
    df3<- df2%>%
      uncount(count)%>%
      select(ctrycode, paste0(n))
    
  ## 1.5 Aggregate indicators at the country level =============================================================
    
    agg<- df3%>%
      group_by(ctrycode)%>%
      summarise_at(n, mean, na.rm= TRUE)
    
    aggregate<- left_join(aggregate, agg, by = join_by(ctrycode))
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[ctrycode], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`ctrycode`)
  
  return(clean)
  
}


FLE_507_clean(f507)

