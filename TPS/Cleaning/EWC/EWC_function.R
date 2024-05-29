## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- EWC Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 26th, 2023
##
## This version:      October 26th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

EWC_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================

  targetvars<- c("Country", "osh_risk", "work_life_balance")
  
  cy<- as.character(c(1:27))
  
  ## 1.2 Sub-setting data=======================================================================================
  
  df$Country<- as.character(df$Country)
  
  df2<- df%>%
    filter(Country %in% cy)%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the appropriate vector to reorient 
  
  #Yes/No questions
  ro<- c("osh_risk")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 0, ifelse(oriented[[i]] == 2, 1, NA_real_))
  }
  
  ro2<- c("work_life_balance")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                    ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  # no<- setdiff(targetvars[-1], c(ro, ro2))
  # 
  # for(i in no){
  #   
  #   oriented[[i]]<- ifelse(oriented[[i]] %in% c(0:10), oriented[[i]], NA_real_)
  #   
  # }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  df3<- oriented
    
  process<- preProcess(df3, method = c("range"))
  normalized <- predict(process, df3)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(Country)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  aggregate$c<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(c = case_when(is.na(c) ~ 
                                 deframe(tibble(cy, nuts))[Country], 
                               TRUE ~ c))%>%
    select(c, everything())%>%
    select(-Country)%>%
    rename("Country" = "c")
  
  
  return(clean)
  
}


