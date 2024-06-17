## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  QRQ Outlier Analysis
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 17th, 2024
##
## This version:      June 17th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

qrq_outlier_analysis<- function(data = eu_qrq_final, type){
  
  if (type == "position"){
    rankings<- data%>%
      group_by(indicator, scenario) %>%
      mutate(Rank = rank(-QRQ_value))
    
    results<- data.frame(matrix(nrow = 0, ncol = 13))
    colnames(results)<- c("Country", "NUTS Region", "Indicator", "QRQ_value", "Rank", "Lower Bound", "Q1", "Median", "Q3", "Upper Bound", "IQR", "Flag", "Scenario")
    
    for (i in unique(data$nuts)){
      
      for (j in unique(data$scenario)){
        
        nrank<- rankings%>%
          filter(nuts == i)%>%
          filter(scenario == j)
        
        iqr<- IQR(nrank[["Rank"]], na.rm = TRUE)
        q1 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
        q2 <- median(nrank[["Rank"]], na.rm = TRUE)
        q3 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
        lower_bound<- q1 - (1.5*iqr)
        upper_bound<- q3 + (1.5*iqr)
        
        df<- nrank%>%
          mutate("Lower Bound" = lower_bound,
                 "Q1" = q1,
                 "Median" = q2,
                 "Q3" = q3,
                 "Upper Bound" = upper_bound,
                 "IQR"= iqr,
                 Flag = if_else(Rank>upper_bound | Rank < lower_bound, 1, 0)
          )%>%
          rename(Country = country,
                 "NUTS Region" = nuts,
                 Indicator = indicator,
                 Scenario = scenario)
        
        results<- rbind(results, df)
        
        
      }
    }
  } else if (type == "score"){
    
    results<- data.frame(matrix(nrow = 0, ncol = 12))
    colnames(results)<- c("Country", "NUTS Region", "Indicator", "QRQ_value", "Lower Bound", "Q1", "Median", "Q3", "Upper Bound", "IQR", "Flag", "Scenario")
    
    for (i in unique(data$indicator)){
      
      for (k in unique(data$scenario)){
        
        datas<- data%>%
          filter(scenario == k)%>%
          filter(indicator == i)
        
        iqr<- IQR(datas$QRQ_value, na.rm = TRUE)
        q1 <- quantile(datas$QRQ_value, prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
        q2 <- median(datas$QRQ_value, na.rm = TRUE)
        q3 <- quantile(datas$QRQ_value, prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
        lower_bound<- q1 - (1.5*iqr)
        upper_bound<- q3 + (1.5*iqr)
        
        
        df<- datas%>%
          mutate("Lower Bound" = lower_bound,
                 "Q1" = q1,
                 "Median" = q2,
                 "Q3" = q3,
                 "Upper Bound" = upper_bound,
                 "IQR"= iqr,
                 Flag = if_else(QRQ_value>upper_bound | QRQ_value < lower_bound, 1, 0)
          )%>%
          rename(Country = country,
                 "NUTS Region" = nuts,
                 Indicator = indicator,
                 Scenario = scenario)
        
        results<- rbind(results, df)
        
      }
      
    }
    
  }
  
  return(results)
  
}

