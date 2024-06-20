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
    
    rankings.df <- data %>%
      group_by(indicator, TPS_variable, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_TPS = rank(-TPS_value))
    
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, TPS_variable, country_name_ltn, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_TPS,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
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
    
    # QRQ LONGITUDINAL scores ======================================================================================================
    
    rankings.df <- data %>% 
      group_by(indicator, scenario, TPS_variable) %>%
      mutate(Rank_QRQ_NUTS = rank(-QRQ_value),
             Rank_TPS_NUTS = rank(-TPS_NUTS_value))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country, nuts, scenario) %>%
      mutate(
        Trend             = Rank_QRQ_NUTS - Rank_TPS_NUTS,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
        Diff_Rank         = max(abs(Rank_QRQ_NUTS - Rank_TPS_NUTS))
      ) %>%
      rename(
        country_name_ltn = country,
        country_code_nuts = nuts,
        QRQ_NUTS_value = QRQ_value
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "LONG") %>%
      select(country_name_ltn, country_code_nuts, indicator, 
             QRQ_NUTS_value, TPS_NUTS_value, Rank_QRQ_NUTS, Rank_TPS_NUTS, Diff_Rank, Trend,
             TPS_NUTS_flag_iqr = flagged_questions, scenario)
      
    
    return(flagged_data.df)
    
  } else if (analysis == "GPP") {
    
    # List of subpillars ======================================================================================================
    subpillar <- c(
      "p_8_1",
      "p_8_2", 
      "p_8_3",
      "p_8_5",
      "p_8_6",
      "p_8_7",
      "p_5_1",
      "p_5_2",
      "p_1_02",
      "p_1_04",
      "p_1_05",
      "p_1_06",
      "p_1_07",
      "p_1_08",
      "p_1_09",
      "p_1_11",
      "p_1_12",
      "p_3_2",
      "p_4_04",
      "p_4_05",
      "p_4_08",
      "p_4_09",
      "p_4_10",
      "p_4_11",
      "p_4_12",
      "p_4_14",
      "p_2_1",
      "p_2_3",
      "p_2_4",
      "p_3_1")
    
    # Initialize an empty list to store the results
    all_scores <- list()
    
    # Function which calculate the scores ======================================================================================================
    
    # Loop through each subpillar
    for (subp in subpillar) {
      
      gpp_vars_sub <- codebook.df  %>%
        filter(str_detect(subpillar, subp)) %>%
        pull(Variable)
      
      data_subset.df <- data %>%
        select(country_name_ltn, nuts_id, all_of(gpp_vars_sub))
      
      normalized <- normalizingvars(data_subset.df, gpp_vars_sub) %>%
        rowwise() %>%
        mutate(GPP_score = mean(c_across(all_of(gpp_vars_sub)), na.rm = TRUE))
      
      gpp_scores <- normalized %>%
        group_by(nuts_id) %>%
        summarise(GPP_score = mean(GPP_score, na.rm = TRUE)) %>%
        mutate(indicator = subp)
      
      all_scores[[subp]] <- gpp_scores
    }
    
    # Combine all the scores into a single data frame
    final_scores <- bind_rows(all_scores) %>%
      rename(nuts = nuts_id)
    
    # Match QRQ and GPP ======================================================================================================
    
    GPP_QRQ <- final_scores %>%
      left_join(eu_qrq_final, by = c("nuts", "indicator")) %>%
      select(country, nuts, scenario, indicator, GPP_score, QRQ_value) %>%
      filter(indicator != "p_5_1") %>%
      filter(indicator != "p_5_2")
    
    # Ranking Analysis ======================================================================================================
    
    rankings.df <- GPP_QRQ %>% 
      group_by(indicator, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_GPP = rank(-GPP_score)) %>%
      rename(
        country_name_ltn = country,
        country_code_nuts = nuts
      )
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn, country_code_nuts, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_GPP,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
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
    
    rankings.df <- data %>% 
      group_by(indicator, scenario) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_ROLI = rank(-ROLI_QRQ_value))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn, scenario) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_ROLI,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
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

