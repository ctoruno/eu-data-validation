## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- ESS Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 28th, 2023
##
## This version:      October 5th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ESS_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars<- c("cntry", "contplt", "donprty", "pbldmna", "pstplonl", "volunfp", "medcrgvc", 
                  "fairelcc", "dfprtalc", "votedirc", "gptpelcc")
  
  #"medcrgv", "fairelc", "dfprtal", "votedir", "gptpelc", "dweight", "pspwght", "pweight", "anweight", "prob"
  
  
  cy<-  c("AT", "BE", "BG", "CY", "CZ", "DE", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
          "LT", "LV", "NL", "PL", "PT", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data=======================================================================================
  
  df2<- df%>%
    filter(cntry %in% cy)%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the appropriate vector to reorient 
  
  #Yes/No questions
  ro<- c("contplt", "donprty", "pbldmna", "pstplonl", "volunfp")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 1, ifelse(oriented[[i]] == 2, 0, NA_real_))
  }
  
  
  no<- setdiff(targetvars[-1], ro)
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1:10), oriented[[i]], NA_real_)
    
  }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  
  oriented[nrow(oriented) + 1,] <- c("mins", rep(list(0), ncol(oriented)-1))
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  normalized2 <- slice(normalized, 1:(n() - 1))
  
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized2%>%
    group_by(cntry)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IT", "LT", "LV", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cy, nuts))[cntry], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-cntry)
  
  
  return(clean)
  
}


