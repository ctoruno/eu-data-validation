rankings.df <- QRQ_TPS_final %>%
  group_by(indicator, TPS_variable) %>%
  mutate(Rank_QRQ = rank(-QRQ_value),
         Rank_TPS = rank(-TPS_value))


flagged_data.df <- rankings.df %>%
  group_by(indicator, TPS_variable, country_name_ltn) %>%
  mutate(
    Diff_Rank         = max(abs(Rank_QRQ - Rank_TPS)),
    flagged_questions = if_else(Diff_Rank >= 7, "Red Flag", 
                                        "Green Flag", NA_character_)
  )
