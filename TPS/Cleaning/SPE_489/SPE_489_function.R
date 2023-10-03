## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- SPE_489 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 3rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Source: Eurobarometer 91.3   April 2019     ZA No. 7572


SPE_489_clean<- function(df){
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Identify Indicators of Interest                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  targetvars<- c("isocntry", "qa9_1", "qa9_2", "qa9_3", "qa1_1", "qa1_2", "qa1_3", "qa1_5", "qa2_1", "qa2_2", "qa2_3", 
                 "qa2_5", "qa3_2", "qa4_2", "qa5_3", "qa5_4", "qa5_5", "qa5_6", "qa6_3", "qa6_4", "qa6_5", "qa6_6", 
                 "qa7_1", "qa7_2", "qa7_3", "qa3_5", "qa4_5")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Select Indicators of Interest                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  df$isocntry<- recode(df$isocntry, "DE-W"="DE", "DE-E"="DE")
  
  dfv<- df%>%
    filter(isocntry %in% cntry)%>%
    select(all_of(targetvars))
  


  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                3.  Reorient Indicator Coding                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  oriented<- dfv

  #Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient.
  
  ro<- c("qa9_1", "qa9_2", "qa9_3", "qa1_1", "qa1_2", "qa1_3", "qa1_5", "qa3_2","qa5_3", "qa5_4", "qa5_5", "qa5_6",
         "qa7_1", "qa7_2", "qa7_3", "qa3_5")
  
  for(i in ro){
  
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                        ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  
  }


  no<- setdiff(targetvars[-1], ro)

  for(i in no){
  
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3), oriented[[i]], NA_real_)
  
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
  
  write.csv(clean, "SPE_489_clean.csv")
  
  
}

#SPE_489_clean(s489)

