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

QRQ_ranking.fn <- function(data, 
                           analysis) {
  
  if(analysis == "TPS") {
    
    # QRQ TPS scores ======================================================================================================
    
    QRQ_TPS_final <- QRQ_final_TPS %>%
      left_join(data, by = c("indicator","country_name_ltn"), relationship = "many-to-many") %>%
      select(country_name_ltn, country_code_nuts, indicator, QRQ_value, TPS_variable, TPS_value, scenario)
    
    rankings.df <- QRQ_TPS_final %>%
      group_by(indicator, TPS_variable, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value, na.last = "keep"),
             Rank_TPS = rank(-TPS_value, na.last = "keep"))
    
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, TPS_variable, country_name_ltn, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_TPS,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_QRQ - Rank_TPS)),
        TPS_flag_tr       = if_else(Diff_Rank >= 10, "Red", 
                                    "Green", NA_character_)
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "qrq") %>%
      select(country_name_ltn, country_code_nuts, indicator,
             QRQ_value, TPS_variable, TPS_value, Rank_QRQ, Rank_TPS, Diff_Rank, 
             Trend, TPS_flag_tr, TPS_flag_iqr = flagged_questions, scenario)
    
    
    return(flagged_data.df)
    
  } else if (analysis == "NUTS") {
    
    # QRQ TPS NUTS scores ======================================================================================================
    
    TPS_nuts_final <- TPS_NUTS_QRQ %>%
      left_join(data, 
                by = c("nuts", "indicator"), 
                relationship = "many-to-many") %>%
      select(!country.x) %>%
      rename(country = country.y,
             TPS_NUTS_value = TPS_value) 
    
    FLE_524_Q5 <- TPS_nuts_final %>%
      filter(indicator == "p_2_2") %>%
      group_by(nuts, country, scenario, indicator, QRQ_value) %>%
      mutate(FLE_524_Q5 = mean(TPS_NUTS_value, na.rm = T)) %>%
      select(nuts, scenario, FLE_524_Q5, indicator, QRQ_value, country) %>%
      pivot_longer(cols = !c(nuts, scenario, indicator, QRQ_value, country), 
                   names_to = "TPS_variable", values_to = "TPS_NUTS_value") %>%
      distinct()
    
    rankings.df <- TPS_nuts_final %>% 
      filter(TPS_variable != "FLE_524_Q5_2") %>%
      filter(TPS_variable != "FLE_524_Q5_1") %>%
      select(nuts, country, scenario, indicator, TPS_variable, TPS_NUTS_value, QRQ_value) %>%
      rbind(FLE_524_Q5) %>%
      group_by(indicator, scenario, TPS_variable) %>%
      mutate(Rank_QRQ_NUTS = rank(-QRQ_value, na.last = "keep"),
             Rank_TPS_NUTS = rank(-TPS_NUTS_value, na.last = "keep")) 
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country, nuts, scenario) %>%
      mutate(
        Trend             = Rank_QRQ_NUTS - Rank_TPS_NUTS,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_QRQ_NUTS - Rank_TPS_NUTS))
      ) %>%
      rename(
        country_name_ltn = country,
        country_code_nuts = nuts,
        QRQ_NUTS_value = QRQ_value
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "LONG") %>%
      select(country_name_ltn, country_code_nuts, indicator, 
             QRQ_NUTS_value, TPS_variable, TPS_NUTS_value, Rank_QRQ_NUTS, Rank_TPS_NUTS, Diff_Rank, Trend,
             TPS_NUTS_flag_iqr = flagged_questions, scenario) %>%
      distinct() 
    
    return(flagged_data.df)
    
  } else if (analysis == "GPP") {
    
    # # List of subpillars ======================================================================================================
    # subpillar <- c(
    #   "p_7_1",
    #   "p_7_2",
    #   "p_7_3",
    #   "p_7_4",
    #   "p_7_5",
    #   "p_7_6",
    #   "p_8_1",
    #   "p_8_2", 
    #   "p_8_3",
    #   "p_8_5",
    #   "p_8_6",
    #   "p_8_7",
    #   "p_5_1",
    #   "p_5_2",
    #   "p_1_02",
    #   "p_1_04",
    #   "p_1_05",
    #   "p_1_06",
    #   "p_1_07",
    #   "p_1_08",
    #   "p_1_09",
    #   "p_1_11",
    #   "p_1_12",
    #   "p_3_2",
    #   "p_4_04",
    #   "p_4_05",
    #   "p_4_08",
    #   "p_4_09",
    #   "p_4_10",
    #   "p_4_11",
    #   "p_4_12",
    #   "p_4_14",
    #   "p_2_1",
    #   "p_2_3",
    #   "p_2_4",
    #   "p_3_1")
    # 
    # # Initialize an empty list to store the results
    # 
    # all_scores <- list()
    # 
    # # Function which calculate the scores ======================================================================================================
    # 
    # # Loop through each subpillar
    # for (subp in subpillar) {
    #   
    #   gpp_vars_sub <- codebook.df  %>%
    #     filter(str_detect(subpillar, subp)) %>%
    #     pull(Variable)
    #   
    #   data_subset.df <- master_data.df %>%
    #     select(country_name_ltn, nuts_id, all_of(gpp_vars_sub))
    #   
    #   normalized <- normalizingvars(data_subset.df, gpp_vars_sub) %>%
    #     rowwise() %>%
    #     mutate(GPP_score = mean(c_across(all_of(gpp_vars_sub)), na.rm = TRUE))
    #   
    #   gpp_scores <- normalized %>%
    #     group_by(nuts_id) %>%
    #     summarise(GPP_score = mean(GPP_score, na.rm = TRUE)) %>%
    #     mutate(indicator = subp)
    #   
    #   all_scores[[subp]] <- gpp_scores
    # }
    # 
    # # Combine all the scores into a single data frame
    # 
    # final_scores <- bind_rows(all_scores) %>%
    #   rename(nuts = nuts_id)
    # 
    # saveRDS(final_scores, 
    #         file = paste0(path2eu,
    #                       "/EU-S Data/eu-data-validation/ALL-valid/Outputs/",
    #                       "GPP_scores.RDS")
    #         )
    # 
    final_scores <- readRDS(file = paste0(path2eu,
                                          "/EU-S Data/eu-data-validation/ALL-valid/Outputs/",
                                          "GPP_scores.RDS")
                            )
    
    # Match QRQ and GPP ======================================================================================================
    
    GPP_QRQ <- final_scores %>%
      left_join(data, by = c("nuts", "indicator")) %>%
      select(country, nuts, scenario, indicator, GPP_score, QRQ_value) %>%
      filter(indicator != "p_5_1") %>%
      filter(indicator != "p_5_2")
    
    # Ranking Analysis ======================================================================================================
    
    rankings.df <- GPP_QRQ %>% 
      group_by(indicator, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value, na.last = "keep"),
             Rank_GPP = rank(-GPP_score, na.last = "keep")) %>%
      rename(
        country_name_ltn = country,
        country_code_nuts = nuts
      )
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn, country_code_nuts, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_GPP,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_QRQ - Rank_GPP)),
        GPP_flag_tr       = if_else(Diff_Rank >= 15, "Red",
                                    "Green", NA_character_)
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "LONG") %>%
      select(country_name_ltn, country_code_nuts, indicator, QRQ_value, GPP_score, 
             Rank_QRQ, Rank_GPP, Diff_Rank, Trend, GPP_flag_tr, 
             GPP_flag_iqr = flagged_questions, scenario)
    
    
    return(flagged_data.df)
    
    
  } else {
    
    # QRQ ROLI scores ======================================================================================================
    QRQ_ROLI_final <- eu_qrq_roli %>%
      left_join(data, by = c("indicator","country_name_ltn"), relationship = "many-to-many") 
    
    rankings.df <- QRQ_ROLI_final %>% 
      group_by(indicator, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value, na.last = "keep"),
             Rank_ROLI = rank(-ROLI_QRQ_value, na.last = "keep"))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_ROLI,
        Trend             = if_else(Trend < 0, "Positive", "Negative"),
        Diff_Rank         = max(abs(Rank_QRQ - Rank_ROLI)),
        ROLI_flag_tr      = if_else(Diff_Rank >= 7, "Red", 
                                         "Green", NA_character_)
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "qrq") %>%
      select(country_name_ltn, indicator, QRQ_value, ROLI_QRQ_value, Rank_QRQ, 
             Rank_ROLI, Diff_Rank, Trend, ROLI_flag_tr, ROLI_flag_iqr = flagged_questions,
             scenario)
    
    return(flagged_data.df)
    
    
  }
  
}

