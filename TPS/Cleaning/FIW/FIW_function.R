## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - FIW Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##                    Carlos Toruno   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 29th, 2023
##
## This version:      October 23rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


FIW_clean <- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("Country/Territory", "A", "A1", "A2", "A3", "B", "B1", "B2", "B4", "C2", "C3", "D", "D1", "D2", "D4", "E",
                  "E1", "E2", "E3", "F", "F1", "F2", "F4", "PR", "CL", "G1", "G2")
  
  
  cntry <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", 
             "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
             "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
             "Slovenia", "Slovakia")
  
  ## 1.2 Sub-setting data========================================================================================
  
  dfv <- df %>%
    filter(Edition == 2023) %>%
    filter(`Country/Territory` %in% cntry )%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  # Not needed
  
  ## 1.4 Normalize indicators ===================================================================================
  
  dfv[nrow(dfv) + 1,] <- c("mins", rep(list(0), ncol(dfv)-1))
  
  process <- preProcess(dfv, method = c("range"))
  normalized <- predict(process, dfv)
  
  dfv2 <- slice(normalized, 1:(n() - 1))
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  # Not needed
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  dfv2$Country<- rep(NA, nrow(dfv2))
  
  nuts <- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
            "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
            "RO", "SE", "SI", "SK")
  
  clean <- dfv2%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[`Country/Territory`], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`Country/Territory`)
  
  return(clean)
}
