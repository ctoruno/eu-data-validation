flags_overview<- function(){
  
  html_flags<- html_flags.df%>%
    select(Country, GPP_Variable_Name)%>%
    mutate("HTML_flag" = "Red")
  
  df<- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(df)<- c("Country", "GPP_Variable_Name")
  
  for (i in unique(html_flags.df$Country)){
    
    df<- rbind(df, tibble("Country" = rep(i, length(reportvarslist)),
                          "GPP_Variable_Name" = reportvarslist))
    
  }
  
  df2<-left_join(df, html_flags)
  
  df2[is.na(df2)]<- "Green"
  df3<- left_join(df2, outlier_analysis.df%>%
                    rename("GPP_Variable_Name" = "Question",
                           "Outliers_flag" = "Flag"))
  df3$Outliers_flag<- gsub(" .*", "", df3$Outliers_flag)
  
  
  ranking<- TPS_ranking_analysis.df%>%
    ungroup()%>%
    select(country_name_ltn, question, flagged_questions, Type_Survey)%>%
    distinct()%>%
    pivot_wider(names_from = Type_Survey, values_from = flagged_questions, values_fn = list)%>% 
    unnest(cols = everything())%>%
    arrange(country_name_ltn, question)
  
  df4<- left_join(df3, ranking%>%
                    rename("Country" = "country_name_ltn",
                           "GPP_Variable_Name" = "question",
                           "Population_ranking_flag" = "population",
                           "Expert_ranking_flag" = "expert"))
  
  df4$Population_ranking_flag<- gsub(" .*", "", df4$Population_ranking_flag)
  
  df4$Expert_ranking_flag<- gsub(" .*", "", df4$Expert_ranking_flag)
  
  gpp<- fullmerge %>% 
    select(country_name_ltn, all_of(reportvarslist)) 
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  normalized<- normalizingvars(gpp, reportvarslist)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  gppaggregate <- normalized%>%
    group_by(country_name_ltn)%>%
    summarise_at(reportvarslist, mean, na.rm= TRUE)%>%
    pivot_longer(cols = all_of(reportvarslist), names_to = "GPP_Variable_Name", values_to = "Score")%>%
    rename("Country"= "country_name_ltn")
  
  subp<- metadata%>%
    select(GPP_Variable_Name, Sub_Pillar, Pillar)%>%
    rbind(variable_list.df%>%
            select(variable, subpillar, pillar)%>%
            rename("GPP_Variable_Name" = "variable", 
                   "Sub_Pillar" = "subpillar",
                   "Pillar" = "pillar"))%>%
    distinct()
  
  
  df5<- left_join(df4, gppaggregate)
  
  df6<- left_join(df5, subp)
  
  df6$Final_flag<- rep(NA_character_, nrow(df6))
  
  for (i in 1:nrow(df6)){
    
    if (df6$HTML_flag[[i]] == "Red"){
      
      if (df6$Outliers_flag[[i]] == "Green"){
        
        if (is.na(df6$Population_ranking_flag[[i]])| df6$Population_ranking_flag[[i]] == "Green"){
          
          if (is.na(df6$Expert_ranking_flag[[i]]) | df6$Expert_ranking_flag[[i]] == "Green"){
            
            df6$Final_flag[[i]]<- "Green"
            
          } else {df6$Final_flag[[i]]<- "Red"}
          
        } else {df6$Final_flag[[i]]<- "Red"}
        
      } else {df6$Final_flag[[i]]<- "Red"}
      
    } else if (df6$Outliers_flag[[i]] == "Red"){
      
      if (is.na(df6$Population_ranking_flag[[i]])| df6$Population_ranking_flag[[i]] == "Green"){
        
        if (is.na(df6$Expert_ranking_flag[[i]]) | df6$Expert_ranking_flag[[i]] == "Green"){
          
          df6$Final_flag[[i]]<- "Green"
          
        }
      }
    } else if (is.na(df6$Population_ranking_flag[[i]]) & is.na(df6$Expert_ranking_flag[[i]])){
      
      df6$Final_flag[[i]]<- "Green"
      
    } else if (is.na(df6$Population_ranking_flag[[i]]) & df6$Expert_ranking_flag[[i]] == "Red"){
      
      df6$Final_flag[[i]]<- "Red"
      
    } else if (is.na(df6$Expert_ranking_flag[[i]]) & df6$Population_ranking_flag[[i]] == "Red") {
      
      df6$Final_flag[[i]]<- "Red"
      
    } else {
      
      df6$Final_flag[[i]]<- "Green"
      
    }
  }
  
  
  return(df6)
  
}
