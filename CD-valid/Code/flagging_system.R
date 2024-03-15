## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  Flagging System
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 5th, 2024
##
## This version:      March 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

flagging_system<- function(gpp_data.df = fullmerge){
  
  reportvars<- codebook.df %>%
    filter(Report == 1)
  
  reportvarslist<- reportvars$Variable
  ## 1.1 Internal Flags  =====================================================================================

  over_time<- data.frame()
  for (i in unique(fullmerge$country_name_ltn)){
    
    df<- fullmerge%>%
      filter(country_name_ltn == i)
    
    time_changes.df <- time_changes(data.df = df,
                                    gpp_vars = reportvarslist,
                                    country = i, 
                                    type = "full")
    
    over_time<- rbind(over_time, time_changes.df)
  }
  
  internal<- over_time%>%
    select(country, variable, warning)%>%
    rename("Over_time"= warning)
  
  internal<- left_join(outlier_analysis.df%>%
    rename("Outliers_analysis" = Flag), internal, by = join_by("Country" == "country", "Question" == "variable"))
  


  ## 1.2 External Flags  =====================================================================================
  
  TPS_ranking_analysis.df%>%
    filter(Type_Survey== "population")
  
  
  final_flags<- data.frame(matrix(nrow= 0, ncol = 3))
  
  
  
}