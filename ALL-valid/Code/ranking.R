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

ranking_analysis.fn <- function(gpp_data.df = master_data.df,
                                tps_data.df = TPS.df,
                                metadata.df = metadataTPS,
                                analysis) {
  
  
  if(analysis == "TPS") {
    
    mat <- metadata.df
    
    ## 1.1 Identifying indicators    =============================================================================
    
    tpsvars <- mat$TPS_Variable_Name
    gppvars <- mat$GPP_Variable_Name
    gppvars <- gppvars[!grepl("^DIS_", gppvars)]
    
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
  } else {
    
    
    ## 2.1 Identifying Indicators ================================================================================
    gppvars <- c(variable_list.df$variable, "BRB_permit_B")
    gppvars <- setdiff(gppvars, c("BRB_permit_A", "BRB_benefits_A", "BRB_id_A", "BRB_school_A", "BRB_health_A"))
    
    ## 2.2 Sub-setting data  =====================================================================================
    
    GPP.df <- GPP_previous.df %>%
      filter(!is.na(country_name_ltn)) %>%
      group_by(country_name_ltn) %>%
      mutate(max_year = max(year, na.rm = T)) %>%
      filter(max_year == year) %>%
      select(country_name_ltn, year, all_of(gppvars)) %>%
      ungroup()
    
    #GPP.df$BRB_permit_A<- ifelse(GPP.df$BRB_permit_A== 0, 2, GPP.df$BRB_permit_A)
    GPP.df$BRB_permit_B<- ifelse(GPP.df$BRB_permit_B== 0, 2, GPP.df$BRB_permit_B)
    #GPP.df$BRB_benefits_A<- ifelse(GPP.df$BRB_benefits_A== 0, 2, GPP.df$BRB_benefits_A)
    GPP.df$BRB_benefits_B<- ifelse(GPP.df$BRB_benefits_B== 0, 2, GPP.df$BRB_benefits_B)
    #GPP.df$BRB_id_A<- ifelse(GPP.df$BRB_id_A== 0, 2, GPP.df$BRB_id_A)
    GPP.df$BRB_id_B<- ifelse(GPP.df$BRB_id_B== 0, 2, GPP.df$BRB_id_B)
    #GPP.df$BRB_school_A<- ifelse(GPP.df$BRB_school_A== 0, 2, GPP.df$BRB_school_A)
    GPP.df$BRB_school_B<- ifelse(GPP.df$BRB_school_B== 0, 2, GPP.df$BRB_school_B)
    #GPP.df$BRB_health_A<- ifelse(GPP.df$BRB_health_A== 0, 2, GPP.df$BRB_health_A)
    GPP.df$BRB_health_B<- ifelse(GPP.df$BRB_health_B== 0, 2, GPP.df$BRB_health_B)
    
    GPP.df$year <- as.character(GPP.df$year)
    
    
    data_subset.df <- gpp_data.df %>%
      select(country_name_ltn, nuts_id, year, all_of(gppvars))
    
    data_subset.df$year <- as.character(data_subset.df$year)
    
    
    ## 2.3 Normalizing data  =====================================================================================
    
    normalizedprev <- normalizingvars(GPP.df, gppvars)
    
    normalizedcurr <- normalizingvars(data_subset.df, gppvars)
    
    ## 2.4 Aggregate data  =======================================================================================
    

    prevaggregate.df<- normalizedprev %>%
      group_by(country_name_ltn) %>%
      summarise_at(gppvars, mean, na.rm= TRUE) %>%
      pivot_longer(cols = all_of(gppvars), names_to = "question", values_to = "prev_value")
    
    gppaggregate.df <- normalizedcurr %>%
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
      left_join(y = variable_list.df%>% 
                  select(variable, `2023  EU Questionnaire`), by = join_by("question" == "variable"), relationship = "many-to-many") %>%
      left_join(y = prevaggregate.df, by = c("question", "country_name_ltn")) %>%
      distinct() %>% 
      drop_na()
    
    
    rankings.df <- gppaggregate.df %>%
      group_by(question) %>%
      mutate(Rank_curr = rank(-value),
             Rank_prev = rank(-prev_value)) %>%
      arrange(question, Rank_curr)
    
    flagged_data.df <- rankings.df %>%
      group_by(question, country_name_ltn) %>%
      mutate(
        Trend             = Rank_curr - Rank_prev,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_curr - Rank_prev)),
        traditional_flag = if_else(Diff_Rank > 7, "Red Flag",
                                    "Green Flag", NA_character_)
      )
    
  }
  
  flagged_data.df<- diff_rank(flagged_data.df, analysis)
  
  
  return(flagged_data.df)
  
}
