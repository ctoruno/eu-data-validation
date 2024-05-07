## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  Outlier Analysis
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 4th, 2024
##
## This version:      March 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outlier_analysis<- function(gpp_data.df = fullmerge){
  
  ## 1.1 Identifying indicators    =============================================================================
  countries<- unique(gpp_data.df$country_name_ltn)
  # vars<- colnames(select_if(gpp_data.df, is.numeric))[-c(1:5)]
  # vars<- vars[!startsWith(vars, "AJ")]
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  oriented<- gpp_data.df
  
  results<- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(results)<- c("Country", "Question", "Flag")
  
  for (i in reportvarslist){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99, 8888, 9999), NA_real_, oriented[[i]])
    
    mean_agg<- mean(oriented[[i]], na.rm = TRUE)
    sd_agg<- sd(oriented[[i]], na.rm = TRUE)
    bound<- 2.5*sd_agg
    
    if (!is.na(sd_agg) & !is.na(mean_agg)){
      for (j in countries){
        
        country_df<- oriented%>%
          filter(country_name_ltn == j)
        
        if (sum(country_df[[i]], na.rm = TRUE)!= 0){
          
          mean_cy<- mean(country_df[[i]], na.rm = TRUE)
          
          if (abs(mean_agg-mean_cy)> bound){
            
            results[nrow(results)+1]<- c(j, i, "Red Flag")
          } else {
            results[nrow(results)+1,]<- c(j, i, "Green Flag")
          }
          
        }else{
          results[nrow(results)+1,]<- c(j, i, "Not Enough Info")
        }
        
      }
    } else{
      results[nrow(results)+1,]<- c(j, i, "Not Enough Info")
    }
  }
  
  return(results)
  
}

