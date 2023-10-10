## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - PII Function
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


PII_clean <- function(df){

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("countryname","Administrative Transparency", "Budget Transparency", "Freedom of the Press")
  
  
  cntry <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", 
             "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
             "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
             "Slovenia", "Slovak Republic")
  
  ## 1.2 Sub-setting data========================================================================================
  
  df1 <- df$`2021`
  
  df2<- df1[-c(1:8),]
  
  colnames(df2)<- df2[1,]
  
  df2<- df2[-1,]
  
  df3<- as.tibble(df2%>%
    filter(countryname %in% cntry)%>%
    select(all_of(targetvars)))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  # Not needed
  
  ## 1.4 Normalize indicators ===================================================================================
  
  df3$`Administrative Transparency`<- as.numeric(df3$`Administrative Transparency`)
  df3$`Budget Transparency`<- as.numeric(df3$`Budget Transparency`)
  df3$`Freedom of the Press`<- as.numeric(df3$`Freedom of the Press`)
  
  df3[nrow(df3) + 1,] <- c("min", rep(list(1), ncol(df3)-1))
  df3[nrow(df3) + 1,] <- c("max", rep(list(10), ncol(df3)-1))
  
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
  
  nuts <- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
            "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
            "RO", "SE", "SI", "SK")
  
  clean <- df4%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[countryname], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    arrange(Country)%>%
    select(-countryname)
  
  return(clean)
}
