## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- SPE_523 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 10th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Source Eurobarometer 97.2  March-April 2022    ZA No. 7887

SPE_523_clean<- function(df){

  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators    =============================================================================
  
  p1<- c("isocntry", "qa15_5", "qa15_6", "qa15_7", "qa15_13")
  p2<- c("qa15_5", "qa15_6", "qa15_7", "qa15_13",
         "qa5", "qa6", "qa7_2", "qa7_4", "qa7_8", "qa7_9", "qa7_12", "qa10", "qa15_1", "qa15_2", "qa15_3", 
         "qa15_4", "qa15_8", "qa15_9", "qa15_11", "qa15_12")
  
  targetvars<- c(p1,p2)
  
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
  
  ro<- c("qa15_5", "qa15_7", "qa15_13")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                       ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("qa7_2", "qa7_4", "qa7_8", "qa7_9", "qa7_12")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 0, ifelse(oriented[[i]] == 0, 1, NA_real_))
  }
  
  ro3<- c("qa6")
  
  for(i in ro3){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4, 5), oriented[[i]], NA_real_)
  }
  
  ro4<- c("qa10")
  
  for(i in ro4){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 1, ifelse(oriented[[i]] == 2, 0, NA_real_))
  }
  
  no<- setdiff(targetvars[-1], c(ro, ro2, ro3, ro4))
  
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

