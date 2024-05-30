## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  Outlier Analysis
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 21st, 2024
##
## This version:      May 29th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outlier_analysis<- function(gpp_data.df = fullmerge){
  
  ## 1.1 Identifying indicators    =============================================================================
  nuts<- unique(gpp_data.df$nuts_id)
  
  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2 <- gpp_data.df %>% 
    select(nuts_id, all_of(reportvarslist)) 
  
  normalized <- normalizingvars(gpp2, reportvarslist)
  
  gppaggregate.df <- normalized %>%
    group_by(nuts_id) %>%
    summarise_at(reportvarslist, mean, na.rm= TRUE)
  
  oriented<- gpp_data.df
  
  results<- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(results)<- c("NUTS Region", "Question", "Score", "Lower Bound", "Median", "Upper Bound", "IQR", "Flag")
  
  for (i in reportvarslist){
    
    #oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99, 8888, 9999), NA_real_, oriented[[i]])
    
    iqr<- IQR(gppaggregate.df[[i]], na.rm = TRUE)
    q1 <- quantile(gppaggregate.df[[i]], prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
    q2 <- median(gppaggregate.df[[i]], na.rm = TRUE)
    q3 <- quantile(gppaggregate.df[[i]], prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
    lower_bound<- q1 - (1.5*iqr)
    upper_bound<- q3 + (1.5*iqr)
    
    mean_agg<- mean(oriented[[i]], na.rm = TRUE)
    sd_agg<- sd(oriented[[i]], na.rm = TRUE)
    bound<- 2.5*sd_agg
    
    #if (!is.na(sd_agg) & !is.na(mean_agg)){
    for (j in nuts){
      
      # country_df<- oriented%>%
      #   filter(country_name_ltn == j)
      
      country_df<- gppaggregate.df%>%
        filter(nuts_id == j)
      
      if (!is.na(country_df[[i]])){
        
        if (country_df[[i]]> upper_bound | country_df[[i]]< lower_bound){
          
          results<- rbind(results, tibble("NUTS Region" =j, "Question" = i, "Score"= country_df[[i]], "Lower Bound"= lower_bound, "Median" = q2, "Upper Bound"= upper_bound, "IQR"=  iqr, "Flag" = "Red"))
        } else {
          results<- rbind(results, tibble("NUTS Region" =j, "Question" = i, "Score"= country_df[[i]], "Lower Bound"= lower_bound, "Median" = q2, "Upper Bound"= upper_bound, "IQR"=  iqr, "Flag" = "Green"))
        }
        
      }else{
        results<- rbind(results, tibble("NUTS Region" =j, "Question" =i, "Score"=NA_real_,"Lower Bound"= NA_real_ ,"Median" = NA_real_, "Upper Bound"= NA_real_, "IQR"= NA_real_, "Flag" ="Not Enough Info"))
      }
      
    }
    #} else{
    #results[nrow(results)+1,]<- c(j, i, NA_real_, NA_real_ , NA_real_, NA_real_, "Not Enough Info")
    #}
  }
  
  return(results)
  
}

