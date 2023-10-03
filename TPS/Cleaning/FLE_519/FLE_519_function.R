## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FLE_519 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 28th, 2023
##
## This version:      October 3rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


FLE_519_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Identify Indicators of Interest                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  targetvars<- c("isocntry", "q1", "q2a_1", "q2a_2", "q2a_3")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Select Indicators of Interest                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  df2<- df%>%
    filter(isocntry %in% cntry)%>%
    select(all_of(targetvars))


  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                3.  Reorient Indicator Coding                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  oriented<- df2
  
  ##Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient.##
  
  ro<- c("q1")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                         ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
    
  }
  
  
  no<- setdiff(targetvars[-1], ro)
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                4.  Normalize Values from 0-1                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                5.  Aggregate to One Score per Country                                                    ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  aggregate<- normalized%>%
    group_by(isocntry)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                6.  Write Clean Dataset                                                                   ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[isocntry], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`isocntry`)
  
  write.csv(clean, "FLE_519_clean.csv")
  
  
}

#FLE_519_clean(f519)

