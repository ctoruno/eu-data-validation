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
## This version:      October 13th, 2023
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
  
  p1<- c("cntry_AN", "year", "E265_01", "E265_02", "E265_03", "E265_04", 
         "E265_05", "E265_06", "E265_07", "E265_08", "E236")
  p3<- c("E069_04", "E069_06", "E069_17", "E069_11", "E069_12", "E069_07", "E069_08", "E069_18",
         "A068", "A080_01", "E025", "E026", "E027", "E028")
  #p5<- c("H008_01", "H001", "H004", "H005", "H002_01", "H002_02", "H002_03", "H002_04", "H002_05", "H008_05", 
  #       "H008_06")
  #"E069_10", "E069_64", "E286", "E287", "E288", "E289", "E291"
  
  targetvars<- c(p1, p3)
  
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
  
  ro<- c("E265_01", "E265_05", "E265_06", "E069_04", "E069_06", "E069_17", "E069_11", "E069_12", "E069_07", "E069_08", "E069_18")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                              ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  
  ro2<- c("E025", "E026", "E027", "E028")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 3, ifelse(oriented[[i]] == 2, 2, 
                                                         ifelse(oriented[[i]] == 3, 1, NA_real_)))
  }
  
  no<- setdiff(targetvars[-c(1,2)], c(ro, ro2))
  
  for(i in no){
    
    oriented[[i]]<- ifelse(oriented[[i]] <0, NA_real_, oriented[[i]])
    
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
  
  
  nuts<- c("AT", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
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

