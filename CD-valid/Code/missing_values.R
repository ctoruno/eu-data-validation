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
## This version:      November 1st, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

missing_values<- function(data, country){
  
  countries<- c("Czechia", "Estonia", "Finland", "France", "Slovenia", "Spain", "Sweden")
  ns<- c(1:7)
  
  ind<- which(countries == country)
  cy<- ns[[ind]]
  
  df<- data%>%
    filter(country_name_ltn == cy)%>%
    select(-age)
  
  missing<- 0
  
  for (i in 1:nrow(df)){
    
    perc<- sum(df[i,] %in% c(98, 99))/(length(data[i,]) +1)
    
    if (perc >= 0.1){
      
      missing<- missing +1
    }
    
  }
  
}


  