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
  repvars<- reportvarslist[!grepl("^DIS_", reportvarslist)]
  repvars<- setdiff(repvars, c("BRB_permit_A", "BRB_benefits_A", "BRB_id_A", "BRB_school_A", "BRB_health_A"))
  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2 <- gpp_data.df %>% 
    select(nuts_id, all_of(repvars)) 
  
  normalized <- normalizingvars(gpp2, repvars)
  
  gppaggregate.df <- normalized %>%
    group_by(nuts_id) %>%
    summarise_at(repvars, mean, na.rm= TRUE)
    
  if (type == "NUTS"){
    
    results<- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(results)<- c("NUTS Region", "Question", "Score", "Lower Bound", "Median", "Upper Bound", "IQR", "Flag")
    
    for (i in repvars){
      
      
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
    
    results<- data.frame(matrix(nrow = 0, ncol = 13))
    colnames(results)<- c("NUTS Region", "Question", "Score", "Rank", "Lower Bound", "Q1", "Median", "Q3", "Upper Bound", "IQR", "Flag",
                          "SD", "SD_flag")
    
    for (i in unique(gppaggregate.df$nuts_id)){
      
      nrank<- rankings%>%
        filter(nuts_id == i)
      
      iqr<- IQR(nrank[["Rank"]], na.rm = TRUE)
      q1 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
      q2 <- median(nrank[["Rank"]], na.rm = TRUE)
      q3 <- quantile(nrank[["Rank"]], prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
      lower_bound<- q1 - (1.5*iqr)
      upper_bound<- q3 + (1.5*iqr)
      
      mean_agg<- mean(nrank[["Rank"]], na.rm = TRUE)
      sd_agg<- sd(nrank[["Rank"]], na.rm = TRUE)
      bound<- 2.5*sd_agg
      
      df<- nrank%>%
        mutate("Lower Bound" = lower_bound,
               "Q1" = q1,
               "Median" = q2,
               "Q3" = q3,
               "Upper Bound" = upper_bound,
               "IQR"= iqr,
               Flag = if_else(Rank>upper_bound | Rank < lower_bound, "Red", "Green"),
               SD= bound,
               SD_flag = if_else(Rank>mean_agg+bound | Rank < mean_agg - bound, "Red", "Green")
        )
      results<- rbind(results, df)
      
    }
    
  }
  
  return(results)
  
}

