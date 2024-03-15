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
  
  ## 1.1 Internal Flags  =====================================================================================

  over_time<- data.frame()
  for (i in unique(fullmerge$country_name_ltn)){
    
    df<- fullmerge%>%
      filter(country_name_ltn == i)
    
    time_changes.df <- time_changes(data.df = df,
                                    gppvars = reportvarslist,
                                    country = i, 
                                    type = "full")
    
    over_time<- rbind(over_time, time_changes.df)
  }
  
  internal<- over_time%>%
    select(country, variable, warning)%>%
    rename("time"= warning)
  
  internal$time<- ifelse(internal$time == "Not enough info", NA_character_, internal$time)
  
  internal<- left_join(outlier_analysis.df%>%
    rename("outliers" = Flag), internal, by = join_by("Country" == "country", "Question" == "variable"))
  
  internal$outliers<- ifelse(internal$outliers == "Not Enough Info", NA_character_, internal$outliers)


  ## 1.2 External Flags  =====================================================================================
  
  ranking<- TPS_ranking_analysis.df%>%
    select(Pillar, country_name_ltn, question, flagged_questions, flagged_score, Type_Survey)%>%
    distinct()%>%
    pivot_wider(names_from = Type_Survey, values_from = c(flagged_questions, flagged_score), values_fn = list)%>% 
    unnest(cols = everything())%>%
    select(-flagged_score_expert)
  
  hm<- left_join(internal, ranking, by = join_by("Country" == "country_name_ltn", "Question" == "question"))%>%
    distinct()%>%
    rename("p_ranking" = flagged_questions_population,
           "threshold" = flagged_score_population,
           "e_ranking" = flagged_questions_expert)
  
  hm[,c("outliers", "time", "p_ranking", "e_ranking", "threshold")] <- lapply(hm[,c("outliers", "time", "p_ranking", "e_ranking", "threshold")], gsub, pattern = "\\s.*", replacement = "")
  
  final_flags<- hm%>%
    mutate("External_validation" = ifelse(is.na(p_ranking) & is.na(e_ranking) & is.na(threshold), NA_character_,
                                    ifelse(is.na(p_ranking)& is.na(threshold) & e_ranking %in% c("Green", "Red"), paste0(e_ranking, "*"),
                                    ifelse(is.na(p_ranking)& is.na(threshold) & e_ranking == "Yellow", "Green*",
                                    ifelse(p_ranking == "Red", "Red", 
                                    ifelse(e_ranking == "Red" & threshold == "Red", "Red", 
                                    ifelse(p_ranking == "Yellow" & e_ranking == "Yellow" & threshold == "Yellow", "Red", "Green")))))),
           
           "Internal_validation" = ifelse(is.na(outliers) & is.na(time), NA_character_,
                                    ifelse(is.na(outliers), paste0(time, "*"),
                                    ifelse(is.na(time), paste0(outliers, "*"),
                                    ifelse(outliers == "Red" & time == "Red", "Red", 
                                    ifelse(outliers == "Green" & time == "Green", "Green",
                                    ifelse(outliers == "Green" & time == "Red", "Red",
                                    ifelse(outliers == "Red" & time == "Green", "Red", NA_character_))))))),
           
           "Final_flag" = ifelse(is.na(External_validation), paste0(Internal_validation, "*"),
                           ifelse(is.na(Internal_validation), paste0(External_validation, "*"),
                           ifelse(External_validation == "Red" & Internal_validation == "Red", "Red", 
                           ifelse(External_validation == "Red" & Internal_validation == "Green", "Yellow",
                           ifelse(External_validation == "Green" & Internal_validation == "Red", "Yellow",
                           ifelse(External_validation == "Green" & Internal_validation == "Green", "Green", 
                           ifelse(grepl("Red", External_validation) & Internal_validation == "Red", "Red**",
                           ifelse(grepl("Red", External_validation) & Internal_validation == "Green", "Yellow**",
                           ifelse(grepl("Green", External_validation) & Internal_validation == "Red", "Yellow**",
                           ifelse(grepl("Green", External_validation) & Internal_validation == "Green", "Green**", 
                           ifelse(grepl("Red", Internal_validation) & External_validation == "Red", "Red**",
                           ifelse(grepl("Red", Internal_validation) & External_validation == "Green", "Yellow**",
                           ifelse(grepl("Green", Internal_validation) & External_validation == "Red", "Yellow**",
                           ifelse(grepl("Green", Internal_validation) & External_validation == "Green", "Green**",
                           ifelse(grepl("Red", Internal_validation) & grepl("Red", External_validation), "Red***",
                           ifelse(grepl("Red", Internal_validation) & grepl("Green", External_validation), "Yellow***",
                           ifelse(grepl("Green", Internal_validation) & grepl("Red", External_validation), "Yellow***",
                           ifelse(grepl("Green", Internal_validation) & grepl("Green", External_validation), "Green***", NA_character_))))))))))))))))))
           )
  
  return(final_flags)

  
}