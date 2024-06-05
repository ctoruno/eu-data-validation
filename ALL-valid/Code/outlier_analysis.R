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

outlier_analysis<- function(gpp_data.df = master_data.df, type){
  
  ## 1.1 Identifying indicators    =============================================================================
  nuts<- unique(gpp_data.df$nuts_id)
  
  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2 <- gpp_data.df %>% 
    select(nuts_id, all_of(reportvarslist)) 
  
  normalized <- normalizingvars(gpp2, reportvarslist)
  
  gppaggregate.df <- normalized %>%
    group_by(nuts_id) %>%
    summarise_at(reportvarslist, mean, na.rm= TRUE)
    
  if (type == "NUTS"){
    
    results<- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(results)<- c("NUTS Region", "Question", "Score", "Lower Bound", "Median", "Upper Bound", "IQR", "Flag")
    
    for (i in reportvarslist){
      
      
      iqr<- IQR(gppaggregate.df[[i]], na.rm = TRUE)
      q1 <- quantile(gppaggregate.df[[i]], prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
      q2 <- median(gppaggregate.df[[i]], na.rm = TRUE)
      q3 <- quantile(gppaggregate.df[[i]], prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
      lower_bound<- q1 - (1.5*iqr)
      upper_bound<- q3 + (1.5*iqr)

      for (j in nuts){
        
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

    }
  } else if (type == "question"){
    
    rankings<- gppaggregate.df%>%
      pivot_longer(cols = !nuts_id, names_to = "Question", values_to = "Score")%>%
      group_by(Question) %>%
      mutate(Rank = rank(-Score))
    
    results<- data.frame(matrix(nrow = 0, ncol = 9))
    colnames(results)<- c("NUTS Region", "Question", "Score", "Rank", "Lower Bound", "Median", "Upper Bound", "IQR", "Flag")
    
    for (i in unique(gppaggregate.df$nuts_id)){
      
      nrank<- rankings%>%
        filter(nuts_id == i)
      
      iqr<- IQR(nrank[["Rank"]], na.rm = TRUE)
      q1 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
      q2 <- median(nrank[["Rank"]], na.rm = TRUE)
      q3 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
      lower_bound<- q1 - (1.5*iqr)
      upper_bound<- q3 + (1.5*iqr)
      
      df<- nrank%>%
        mutate("Lower Bound" = lower_bound,
               "Median" = q2,
               "Upper Bound" = upper_bound,
               "IQR"= iqr,
               Flag = if_else(Rank>upper_bound | Rank < lower_bound, "Red", "Green")
        )
      results<- rbind(results, df)
      
    }
    
  }
  
  return(results)
  
}

