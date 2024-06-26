## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- SPE_502 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 5th, 2023
##
## This version:      October 10th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Source: Eurobarometer 92.4   December 2019 ZA No. 7602

SPE_502_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  targetvars<- c("isocntry", "qb15_5", "qb15_6", "qb15_7", "qb15_13",
                 "qb5", "qb6", "qb7_2", "qb7_4", "qb7_8", "qb7_9", "qb7_12", "qb10", "qb15_1", "qb15_2", "qb15_3", 
                      "qb15_4", "qb15_8", "qb15_9", "qb15_11", "qb15_12")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  df$isocntry<- recode(df$isocntry, "DE-W"="DE", "DE-E"="DE")
  
  df2<- df%>%
    filter(isocntry %in% cntry)%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro1<- c("qb15_5", "qb15_7", "qb15_13")
  
  for(i in ro1){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                          ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("qb7_2", "qb7_4", "qb7_8", "qb7_9", "qb7_12")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 0, ifelse(oriented[[i]] == 0, 1, NA_real_))
  }
  
  ro3<- c("qb6")
  
  for(i in ro3){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4, 5), oriented[[i]], NA_real_)
  }
  
  ro4<- c("qb10")
  
  for(i in ro4){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 1, ifelse(oriented[[i]] == 2, 0, NA_real_))
  }
  

  no<- setdiff(targetvars[-1], c(ro1, ro2,ro3,ro4))
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(isocntry)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[isocntry], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`isocntry`)
  
  return(clean)
  
}
