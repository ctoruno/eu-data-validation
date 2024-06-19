## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  NUTS Ranking Analysis
##
## Author:            Dalia Habiby   (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 18th, 2024
##
## This version:      June 18th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nuts_ranking_analysis.fn <- function(gpp_data.df = master_data.df,
                                tps_data.df = TPS.df,
                                metadata.df = metadataTPS,
                                analysis) {
  

    
    mat <- metadata.df
    
    ## 1.1 Identifying indicators    =============================================================================
    
    tpsvars <- mat$TPS_Variable_Name
    gppvars <- mat$GPP_Variable_Name
    gppvars <- gppvars[!grepl("^DIS_", gppvars)]
    gppvars <- gppvars[!grepl("^BRB_", gppvars)]
    
    
    ## 1.2 Sub-setting data  =====================================================================================
    gpp2 <- gpp_data.df %>% 
      select(country_name_ltn, nuts_id, all_of(gppvars)) 
    
    ## 1.3 Re-orient indicators ==================================================================================
    
    normalized <- normalizingvars(gpp2, gppvars)
    
    ## 1.5 Aggregate indicators at the country level =============================================================
    
    matchTPS <- mat %>%
      select(question = GPP_Variable_Name, tps_question = TPS_Variable_Name, Sub_Pillar, Pillar, Type_Survey, Match, EU_Questionnaire)
    
    tps.df <- tps_data.df %>%
      select(country_name_ltn, all_of(tpsvars), ) %>%
      pivot_longer(cols = !country_name_ltn, names_to = "tps_question", values_to = "tps_value")
    
    ## 1.6 Merge all data    =============================================================================
    
    gppaggregate.df <- normalized %>%
      group_by(country_name_ltn, nuts_id) %>%
      summarise_at(gppvars, mean, na.rm= TRUE) %>%
      pivot_longer(cols = all_of(gppvars), names_to = "question", values_to = "value") %>%
      left_join(weight.df%>% 
                  select(nuts_id, regionpoppct))%>%
      group_by(country_name_ltn, question)%>%
      mutate(value_weighted = sum(value*regionpoppct))%>%
      select(country_name_ltn, question, value_weighted)%>%
      distinct()%>%
      rename(value = value_weighted)%>%
      left_join(y = matchTPS, by = "question", relationship = "many-to-many") %>%
      left_join(y = tps.df, by = c("tps_question", "country_name_ltn")) %>%
      select(country_name_ltn, question, value, tps_question, tps_value, Match, Type_Survey, EU_Questionnaire) %>%
      distinct() %>% 
      drop_na() 
    
    ## 1.7 Ranking analysis  =============================================================================
    
    rankings.df <- gppaggregate.df %>%
      group_by(question, tps_question) %>%
      mutate(Rank_GPP = rank(-value),
             Rank_TPS = rank(-tps_value)) %>%
      arrange(question, tps_question, Rank_GPP)%>% 
      ungroup()%>%
      group_by(country_name_ltn, question, Type_Survey)%>%
      mutate(Rank_TPS_avg = mean(Rank_TPS))
    
    flagged_data.df <- rankings.df %>%
      group_by(question, tps_question, country_name_ltn) %>%
      mutate(
        Trend             = Rank_GPP - Rank_TPS_avg,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_GPP - Rank_TPS_avg)),
        traditional_flag = if_else(Diff_Rank > 10, "Red Flag",
                                   "Green Flag", NA_character_)
      ) %>%
      distinct()
  
  flagged_data.df<- diff_rank(flagged_data.df, analysis)
  
  
  return(flagged_data.df)
  
}
