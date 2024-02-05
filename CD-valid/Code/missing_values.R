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
    mutate(across(everything(), ~ replace(., . %in% c(98, 99, -9999, -8888), NA)))
  
  cntry<- data.frame(matrix(nrow=0, ncol=3))
  colnames(cntry)<- c("Country", "Prop10", "Prop15") 
  
  for (i in unique(dataNA$country_name_ltn)){
    
    df<-dataNA%>%
      filter(country_name_ltn == i)
    
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
    
    navec<- colSums(is.na(df))/nrow(df)
    perc10<- sum(navec>.10)/length(navec)
    perc15<- sum(navec>.15)/length(navec)
    
    nuts[nrow(nuts)+1,]<- c(i, perc10, perc15)
  }
  return(list("country" = cntry, "nuts" = nuts))
  
}


  