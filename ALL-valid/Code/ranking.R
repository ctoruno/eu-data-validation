## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  Ranking Analysis
##
## Author:            Santiago Pardo   (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 2nd, 2024
##
## This version:      February 2nd, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

TPS_ranking_analysis.fn <- function(gpp_data.df = fullmerge,
                                    tps_data.df = TPS.df,
                                    metadata.df = metadataTPS) {

  mat <- metadata.df
  
  ## 1.1 Identifying indicators    =============================================================================
  
  tpsvars <- mat$TPS_Variable_Name
  gppvars <- mat$GPP_Variable_Name
  
  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2 <- gpp_data.df %>% 
    select(country_name_ltn, all_of(gppvars)) 
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  normalized <- normalizingvars(gpp2, gppvars)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  matchTPS <- mat %>%
    select(question = GPP_Variable_Name, tps_question = TPS_Variable_Name, Pillar, Type_Survey, Match)
  
  tps.df <- tps_data.df %>%
    select(country_name_ltn, all_of(tpsvars), ) %>%
    pivot_longer(cols = !country_name_ltn, names_to = "tps_question", values_to = "tps_value")
  
  ## 1.6 Merge all data    =============================================================================
  
  gppaggregate.df <- normalized %>%
    group_by(country_name_ltn) %>%
    summarise_at(gppvars, mean, na.rm= TRUE) %>%
    pivot_longer(cols = !country_name_ltn, names_to = "question", values_to = "value") %>%
    left_join(y = matchTPS, by = "question", relationship = "many-to-many") %>%
    left_join(y = tps.df, by = c("tps_question", "country_name_ltn")) %>%
    distinct() %>% 
    drop_na()
  
  ## 1.7 Ranking analysis    =============================================================================
  
  rankings.df <- gppaggregate.df %>%
    group_by(question, tps_question) %>%
    mutate(Rank_GPP = rank(-value),
           Rank_TPS = rank(-tps_value)) %>%
    arrange(question, tps_question, Rank_GPP)
  
  flagged_data.df <- rankings.df %>%
    group_by(question, tps_question, country_name_ltn) %>%
    mutate(
      Diff_Rank         = max(abs(Rank_GPP - Rank_TPS)),
      flagged_questions = if_else(Diff_Rank >= 7, "Red Flag",
                                  "Green Flag", NA_character_)
      )
  
  return(flagged_data.df)
  
}
