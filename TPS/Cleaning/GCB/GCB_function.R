## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - GCB Function
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

GCB_clean <- function(df){
 
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("COUNTRY", "Q2", "Q3", "Q5_1", "Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6", "Q5_7", "Q8_1", "Q8_2", 
                  "Q8_3", "Q8_4", "Q8_5", "Q8_6", "Q9_1", "Q9_2", "Q9_3", "Q9_4", "Q9_5", "Q9_6", "Q19_5", "Q20_1",
                  "Q20_2")
  
  
  cntry <- c("1","2","3","5","6","11","7", "8", "12", "26", "9", "10", "4", "13", "14", "15", "17", "18", "16", "19", 
             "20", "21", "22", "23", "27", "25", "24")
  
  
  ## 1.2 Sub-setting data========================================================================================
  
  df2 <- df%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro1<- c("Q2", "Q5_1", "Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6", "Q5_7")
  
  for(i in ro1){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                   ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("Q19_5")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 3, ifelse(oriented[[i]] == 3, 1, 
                                   ifelse(oriented[[i]] == 2, 2, NA_real_)))
  }
  
  ro3<- c("Q20_1", "Q3")
  
  for(i in ro3){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 5, ifelse(oriented[[i]] == 2, 4, 
                                   ifelse(oriented[[i]] == 3, 3, ifelse(oriented[[i]] == 4, 2, ifelse(oriented[[i]] == 5, 1, NA_real_)))))
  }
  
  ro4<- c("Q20_2")
  
  for(i in ro4){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4, 5), oriented[[i]], NA_real_)
    
  }
  
  no<- setdiff(targetvars[-1], c(ro1, ro2, ro3, ro4))
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  ## 1.4 Normalize indicators ===================================================================================
  
  oriented$COUNTRY<- as.character(oriented$COUNTRY)
  
  process <- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  aggregate<- normalized%>%
    group_by(COUNTRY)%>%
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
                                 deframe(tibble(cntry, nuts))[COUNTRY], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-COUNTRY)
  
  return(clean)
}
