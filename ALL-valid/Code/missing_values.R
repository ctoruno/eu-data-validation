## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation-  Missing Values Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 1st, 2023
##
## This version:      February 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

missing_values<- function(data= fullmerge){
  
  dataNA<-data %>% 
    mutate(across(everything(), ~ replace(., . %in% c(-9999, -8888), NA)))
  dataNA<-dataNA %>% 
    mutate(across(!c(AJR_solvingtime,AJE_offwork_time, AJE_income_loss, AJE_healthcare_visits, AJE_hospital_time, income2), ~ replace(., . %in% c(98, 99), NA)))
  
  cntry<- data.frame(matrix(nrow=0, ncol=3))
  colnames(cntry)<- c("Country", "Prop10", "Prop15") 
  
  for (i in unique(dataNA$country_name_ltn)){
    
    df<-dataNA%>%
      filter(country_name_ltn == i)
    df<- df[-c(1:9)]
    
    navec<- colSums(is.na(df))/nrow(df)
    perc10<- sum(navec>.10)/length(navec)
    perc15<- sum(navec>.15)/length(navec)
    
    cntry[nrow(cntry)+1,]<- c(i, perc10, perc15)
  }
  
  nuts<- data.frame(matrix(nrow=0, ncol=3))
  colnames(nuts)<- c("NUTS ID", "Prop10", "Prop15") 
  
  for (i in unique(dataNA$nuts_id)){
    
    df<-dataNA%>%
      filter(nuts_id == i)
    df<- df[-c(1:9)]
    
    navec<- colSums(is.na(df))/nrow(df)
    perc10<- sum(navec>.10)/length(navec)
    perc15<- sum(navec>.15)/length(navec)
    
    nuts[nrow(nuts)+1,]<- c(i, perc10, perc15)
  }
  return(list("country" = cntry, "nuts" = nuts))
  
}


  