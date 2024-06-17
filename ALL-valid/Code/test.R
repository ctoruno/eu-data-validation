QRQ_flags <- QRQ_flagging_system.df %>%
  group_by(country_name_ltn, country_code_nuts, scenario) %>%
  summarise(
    total_flags_tr = sum(total_flags_tr, 
                         na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, 
                          na.rm = T)
  ) %>%
  ungroup()

write_xlsx(QRQ_flags,
           path = paste0(path2eu,
                         "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_flags.xlsx")
)

best_scenario <- QRQ_flags %>%
  group_by(country_code_nuts, scenario) %>%
  mutate(
    final_score = mean(c(total_flags_tr,total_flags_iqr),na.rm = T)
    ) %>%
  group_by(country_code_nuts) %>%
  mutate(
    best_score = min(final_score),
    ) %>%
  filter(
    final_score == best_score
  ) %>%
  ungroup() %>%
  group_by(country_code_nuts) %>%
  mutate(
    best_iqr   = min(total_flags_iqr)
  ) %>%
  filter(
    total_flags_iqr == best_iqr
  ) %>%
  ungroup() %>%
  mutate(counter = 1) %>%
  group_by(country_code_nuts) %>%
  mutate(
    repeated = sum(counter),
    number = as.numeric(str_extract_all(scenario, "\\d+")),
    number = if_else(is.na(number), 0, number),
    min_number = min(number),
  ) %>%
  ungroup() %>%
  mutate(
    filtro = 
      case_when(
        repeated == 1 ~ 1,
        min_number == number ~ 1,
        T ~ 0
      )
  ) %>%
  filter(filtro == 1) %>%
  select(country = country_name_ltn, nuts = country_code_nuts, best_scenario = scenario, best_score)

final_scores_nuts <- eu_qrq_final %>%
  left_join(best_scenario, by = c("country", "nuts")) %>%
  filter(scenario == best_scenario)

write_xlsx(final_scores_nuts,
           path = paste0(path2eu,
                         "/EU-S Data/eu-data-validation/ALL-valid/Outputs/best_scenario_nuts.xlsx")
)

avg_scores <- final_scores_nuts %>%
  select(country, nuts, best_score) %>%
  distinct() %>%
  summarise(
    best_score = mean(best_score)
  )

final_scores_country <- final_scores_nuts %>%
  left_join(weight.df %>%
              select(country, nuts = nuts_id, regionpop, regionpoppct), 
            by = c("country", "nuts")) %>%
  drop_na() %>%
  group_by(country, indicator) %>%
  mutate(
    region_adjusted = regionpop/sum(regionpop),
    repeated = if_else(regionpoppct == region_adjusted, 1, 0),
    QRQ_country_value = region_adjusted*QRQ_value
  ) %>%
  ungroup() %>%
  group_by(country, indicator) %>%
  summarise(QRQ_value = sum(QRQ_country_value))

write_xlsx(final_scores_country,
           path = paste0(path2eu,
                         "/EU-S Data/eu-data-validation/ALL-valid/Outputs/best_scenario_country.xlsx")
)