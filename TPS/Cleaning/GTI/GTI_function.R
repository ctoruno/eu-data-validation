## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - GTI Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 10th, 2023
##
## This version:      October 10th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


GTI_clean <- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("T-Index scores")
  
  
  cntry <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", 
             "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
             "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
             "Slovenia", "Slovak Republic")
  
  ## 1.2 Sub-setting data========================================================================================
  
  df2 <- df$`T-Index scores`
  
  df3<- df2%>%
    filter(Countryname %in% cntry)%>%
    filter(Year == 2023)%>%
    select(Countryname, tindex_total)
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  # Not needed
  
  ## 1.4 Normalize indicators ===================================================================================
  
  df3[nrow(df3) + 1,] <- c("min", rep(list(0), ncol(df3)-1))
  df3[nrow(df3) + 1,] <- c("max", rep(list(20), ncol(df3)-1))
  
  process <- preProcess(df3, method = c("range"))
  normalized <- predict(process, df3)
  
  df4 <- slice(normalized, 1:(n() - 2))
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  # Not needed
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  df4$Country<- rep(NA, nrow(df4))
  
  clean <- df4%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[Countryname], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    arrange(Country)%>%
    select(-Countryname)
  
  return(clean)
}
