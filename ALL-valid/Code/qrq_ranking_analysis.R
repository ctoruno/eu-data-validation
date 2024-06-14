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
      group_by(indicator, TPS_variable) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_TPS = rank(-TPS_value))
    
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, TPS_variable, country_name_ltn) %>%
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
             Trend, TPS_flag_tr, TPS_flag_iqr = flagged_questions)
    
    
    return(flagged_data.df)
    
  } else if (analysis == "LONG") {
    
    # QRQ LONGITUDINAL scores ======================================================================================================
    
    rankings.df <- data %>% 
      group_by(indicator) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_LONG = rank(-long_QRQ_value))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn, country_code_nuts) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_LONG,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
        Diff_Rank         = max(abs(Rank_QRQ - Rank_LONG)),
        LONG_flag_tr = if_else(Diff_Rank >= 15, "Red", 
                                         "Green", NA_character_)
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "LONG") %>%
      select(country_name_ltn, country_code_nuts, indicator, 
             QRQ_value, long_QRQ_value, Rank_QRQ, Rank_LONG, Diff_Rank, Trend,
             LONG_flag_tr, LONG_flag_iqr = flagged_questions)
      
    
    return(flagged_data.df)
    
  } else {
    
    # QRQ ROLI scores ======================================================================================================
    
    rankings.df <- data %>% 
      group_by(indicator) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_ROLI = rank(-ROLI_QRQ_value))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn) %>%
      mutate(
        Trend             = Rank_QRQ - Rank_ROLI,
        Trend             = if_else(Trend < 0, "Negative", "Positive"),
        Diff_Rank         = max(abs(Rank_QRQ - Rank_ROLI)),
        ROLI_flag_tr      = if_else(Diff_Rank >= 7, "Red", 
                                         "Green", NA_character_)
      )
    
    flagged_data.df <- diff_rank(flagged_data.df, "qrq") %>%
      select(country_name_ltn, indicator, QRQ_value, ROLI_QRQ_value, Rank_QRQ, 
             Rank_ROLI, Diff_Rank, Trend, ROLI_flag_tr, ROLI_flag_iqr = flagged_questions)
    
    return(flagged_data.df)
    
    
  }
  

}

