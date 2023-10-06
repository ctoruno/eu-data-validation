## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - QOG Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 5th, 2023
##
## This version:      October 5th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df <- read_dta(file.path(path2SP, 
                         "8. Data/TPS/Quality of Government/QOG_raw.dta",
                         fsep = "/")) 

QOG_clean <- function(df){
 
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("country", "q14", "q15", "q16", 
                  "q17_1", "q17_2", "q18_1", "q18_2", "q18_3", "q18_4")
  #"corruptionp", "corruption_subPer", "corruption_subExp" , "q19_1", "q19_2", "q19_3", "q19_4"
  
  cntry <- c("1","2","3","5","6","11","7", "8", "12", "26", "9", "10", "4", "13", "14", "15", "17", "18", "16", "19", 
             "20", "21", "22", "23", "27", "25", "24")
  
  ## 1.2 Sub-setting data========================================================================================
  
  df2 <- df%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro<- c("q14", "q15", "q16", "q17_1", "q17_2")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 10, ifelse(oriented[[i]] == 2, 9, 
                                  ifelse(oriented[[i]] == 3, 8, ifelse(oriented[[i]] == 4, 7, 
                                  ifelse(oriented[[i]] == 5, 6, ifelse(oriented[[i]] == 6, 5, 
                                  ifelse(oriented[[i]] == 7, 4, ifelse(oriented[[i]] == 8, 3, 
                                  ifelse(oriented[[i]] == 9, 2, ifelse(oriented[[i]] == 10, 1, NA_real_))))))))))
  }
  
  no<- setdiff(targetvars[-1], ro)
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2), oriented[[i]], NA_real_)
    
  }
  
  ## 1.4 Normalize indicators ===================================================================================
  
  oriented$country<- as.character(oriented$country)
  
  process <- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  aggregate<- normalized%>%
    group_by(country)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  aggregate$Country<- rep(NA, nrow(aggregate))
  
  nuts <- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
            "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
            "RO", "SE", "SI", "SK")
  
  clean <- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[country], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-country)
  
  return(clean)
}
