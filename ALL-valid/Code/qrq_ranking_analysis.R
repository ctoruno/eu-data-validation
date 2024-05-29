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
        Diff_Rank         = max(abs(Rank_QRQ - Rank_TPS)),
        TPS_flagged_questions = if_else(Diff_Rank >= 7, "Red", 
                                        "Green", NA_character_)
      )
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
        Diff_Rank         = max(abs(Rank_QRQ - Rank_LONG)),
        LONG_flagged_questions = if_else(Diff_Rank >= 15, "Red", 
                                         "Green", NA_character_)
      )
    
    return(flagged_data.df)
    
  } else {
    
    # QRQ ROLI scores ======================================================================================================
    
    rankings.df <- QRQ_ROLI_final %>% 
      group_by(indicator) %>%
      mutate(Rank_QRQ = rank(-QRQ_value),
             Rank_ROLI = rank(-ROLI_QRQ_value))
    
    flagged_data.df <- rankings.df %>%
      group_by(indicator, country_name_ltn) %>%
      mutate(
        Diff_Rank         = max(abs(Rank_QRQ - Rank_ROLI)),
        ROLI_flagged_questions = if_else(Diff_Rank >= 3, "Red", 
                                         "Green", NA_character_)
      )
    
    return(flagged_data.df)
    
    
  }
  

}

