final_outcomes <- QRQ_flagging_system.df_short %>%
  group_by(scenario) %>%
  summarise(
    total_flags = sum(total_flags_iqr, na.rm = T),
    total_outliers_pos = sum(c_flags_POS_iqr, na.rm = T),
    total_outliers_scores = sum(c_flags_SCORE_iqr, na.rm = T),
    total_outliers_capital = sum(c_flags_CAPITALS_iqr, na.rm = T)
  )


final_outcomes_nuts <- QRQ_flagging_system.df %>%
  group_by(scenario, country_code_nuts) %>%
  summarise(
    total_flags = sum(total_flags_iqr, na.rm = T),
    total_outliers_pos = sum(c_flags_POS_iqr, na.rm = T),
    total_outliers_scores = sum(c_flags_SCORE_iqr, na.rm = T),
    total_outliers_capital = sum(c_flags_CAPITALS_iqr, na.rm = T)
  ) %>%
  filter(scenario == "best scenario alt 2")
