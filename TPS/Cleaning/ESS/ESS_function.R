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
## This version:      October 11th, 2023
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
  
  p1<- c("cntry", "contplt", "donprty", "pbldmna", "pstplonl", "volunfp", "medcrgvc", 
         "fairelcc", "dfprtalc", "votedirc", "gptpelcc")
  p3<- c("psppsgva", "psppipla", "trstprl", "trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun")
  p4<- c("cttresac", "rghmgprc")
  
  targetvars<- c(p1, p3, p4)
  
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
  
  ro2<- c("psppsgva", "psppipla")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1:5), oriented[[i]], NA_real_)
  }
  
  no<- setdiff(targetvars[-1], c(ro, ro2))
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(0:10), oriented[[i]], NA_real_)
    
  }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  df3a<- oriented%>%
    select(-psppsgva, -psppipla)
  
  df3a[nrow(df3a) + 1,] <- c("mins", rep(list(0), ncol(df3a)-1))
  
  processa<- preProcess(df3a, method = c("range"))
  normalizeda <- predict(processa, df3a)
  
  normalizeda2 <- slice(normalizeda, 1:(n() - 1))
  
  
  df3b<- oriented%>%
    select(psppsgva, psppipla)
  
  processb<- preProcess(df3b, method = c("range"))
  normalizedb <- predict(processb, df3b)
  
  norm<- cbind(normalizeda2, normalizedb)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- norm%>%
    group_by(cntry)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BE", "BG", "CY", "CZ", "DE", "EE", "EL", "ES", "FI", 
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


