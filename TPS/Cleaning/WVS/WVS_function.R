## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- WVS Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 2nd, 2023
##
## This version:      October 4th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


WVS_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars<- c("cntry_AN", "year", "E265_01", "E265_02", "E265_03", "E265_04", 
                 "E265_05", "E265_06", "E265_07", "E265_08", "E236")
  
  cy<- c("AT", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", "FI", "FR", "HR", "HU", "IT", 
            "LT", "LV", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  
  ## 1.2 Sub-setting data=======================================================================================
  
  df2<- df%>%
    select(all_of(targetvars))
  
  dfy<- df%>%
    filter(cntry_AN %in% cy)%>%
    group_by(cntry_AN)%>%
    summarise(yr= max(year))
  
  df3<- data.frame(matrix(ncol= length(targetvars), nrow = 0))
  colnames(df3)<- targetvars
  
  for (i in 1:length(cy)){
    
    latest<- df2%>%
      filter(cntry_AN == dfy$cntry_AN[i] & year == dfy$yr[i])
    
    df3<- rbind(df3, latest)
  }
  
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df3
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro<- c("E265_01", "E265_05", "E265_06")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                              ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  
  no<- setdiff(targetvars[-c(1,2)], ro)
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2, 3, 4), oriented[[i]], NA_real_)
    
  }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
    
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(cntry_AN, year)%>%
    summarise_at(targetvars[-c(1,2)], mean, na.rm= TRUE)
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  nuts<- c("AT", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IT", "LT", "LV", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean1<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cy, nuts))[cntry_AN], 
                               TRUE ~ Country))%>%
    select(Country, everything())
  
  clean<- clean1[-2]
  
  return(clean)
  
}

