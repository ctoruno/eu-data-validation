## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Altogether data validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 7th, 2024
##
## This version:      May 7th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Best scenario function

best_scenario.fn <- function(
    
  data.df = QRQ_flagging_system.df
  
){
  
  best_scenario <- data.df  %>%
    group_by(country_code_nuts, scenario) %>%
    mutate(
      final_score = sum(total_flags_iqr,na.rm = T)
    ) %>%
    group_by(country_code_nuts) %>%
    mutate(
      best_score = min(final_score, na.rm = T),
    ) %>%
    filter(
      final_score == best_score
    ) %>%
    group_by(country_code_nuts) %>%
    mutate(
      number = as.numeric(str_extract_all(best_scenario_final, "\\d+")),
      number = if_else(is.na(number), 0, number),
      number = if_else(str_detect(scenario, "best scenario"), number + 10, number),
      max_number = max(number),
      num_scenario = max_number %% 10,
      best_scenario_final = paste("scenario", num_scenario)
    )  %>%
    ungroup() %>%
    filter(
      number == max_number
    ) %>%
    select(country = country_name_ltn, nuts = country_code_nuts, scenario, best_scenario_final, best_score) 
  
  nuts_best <- best_scenario %>%
    select(nuts, best_scenario_final, best_score) %>%
    distinct()
  
  # Final scores
  final_scores_nuts <- eu_qrq_final %>%
    select(!best_scenario_final) %>%
    left_join(nuts_best, by = c("nuts")) %>%
    filter(scenario == best_scenario_final) %>%
    mutate(
      scenario = "best scenario"
    ) %>%
    select(country, nuts, indicator, scenario, best_scenario_final, best_score, QRQ_value, capital)
  
  
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
    group_by(country, indicator, scenario) %>%
    summarise(QRQ_value = sum(QRQ_country_value)) %>%
    distinct()
  
  best_scores.ls <- list(final_scores_nuts, final_scores_country, nuts_best)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 1                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================


final_scores.ls <- best_scenario.fn()
names(final_scores.ls) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)

eu_qrq_final_it <- eu_qrq_final %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    best_scenario_final = "best scenario"
  )

EU_QRQ_country_it <- EU_QRQ_country %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df1 <- flags_overview(type = "QRQ",
                                         QRQ_country_data = EU_QRQ_country_it,
                                         QRQ_nuts_data = eu_qrq_final_it
                                         )

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df1 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 1                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls2 <- best_scenario.fn(QRQ_flagging_system.df1)
names(final_scores.ls2) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls2[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 2"
  )
  

eu_qrq_final_it_2 <- eu_qrq_final_it %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls2[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 2",
    best_scenario_final = "best scenario 2"
  )

EU_QRQ_country_it_2 <- EU_QRQ_country_it %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_2, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_2, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_2,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_2, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_2, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_2)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df2 <- flags_overview(type = "QRQ",
                                         QRQ_country_data = EU_QRQ_country_it_2,
                                         QRQ_nuts_data = eu_qrq_final_it_2
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df2 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 3                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls3 <- best_scenario.fn(QRQ_flagging_system.df2)
names(final_scores.ls3) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls3[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 3"
  )


eu_qrq_final_it_3 <- eu_qrq_final_it_2 %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls3[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 3",
    best_scenario_final = "best scenario 3"
  )

EU_QRQ_country_it_3 <- EU_QRQ_country_it_2 %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_3, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_3, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_3, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_3,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_3, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_3, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_3)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df3 <- flags_overview(type = "QRQ",
                                          QRQ_country_data = EU_QRQ_country_it_3,
                                          QRQ_nuts_data = eu_qrq_final_it_3
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df3 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 4                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls4 <- best_scenario.fn(QRQ_flagging_system.df3)
names(final_scores.ls4) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls4[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 4"
  )


eu_qrq_final_it_4 <- eu_qrq_final_it_3 %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls3[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 4",
    best_scenario_final = "best scenario 4"
  )

EU_QRQ_country_it_4 <- EU_QRQ_country_it_3 %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_4, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_4, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_4, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_4,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_4, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_4, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_4)

### QRQ flagging system ======================================================================================================


QRQ_flagging_system.df4 <- flags_overview(type = "QRQ",
                                          QRQ_country_data = EU_QRQ_country_it_4,
                                          QRQ_nuts_data = eu_qrq_final_it_4
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df4 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 5                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls5 <- best_scenario.fn(QRQ_flagging_system.df4)
names(final_scores.ls5) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls5[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 5"
  )


eu_qrq_final_it_5 <- eu_qrq_final_it_4 %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls5[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 5",
    best_scenario_final = "best scenario 5"
  )

EU_QRQ_country_it_5 <- EU_QRQ_country_it_4 %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_5, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_5, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_5, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_5,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_5, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_5, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_5)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df5 <- flags_overview(type = "QRQ",
                                          QRQ_country_data = EU_QRQ_country_it_5,
                                          QRQ_nuts_data = eu_qrq_final_it_5
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df5 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 6                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls6 <- best_scenario.fn(QRQ_flagging_system.df5)
names(final_scores.ls6) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls6[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 6"
  )


eu_qrq_final_it_6 <- eu_qrq_final_it_5 %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls6[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 6",
    best_scenario_final = "best scenario 6"
  )

EU_QRQ_country_it_6 <- EU_QRQ_country_it_5 %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_6, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_6, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_6, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_6,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_6, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_6, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_6)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df6 <- flags_overview(type = "QRQ",
                                          QRQ_country_data = EU_QRQ_country_it_6,
                                          QRQ_nuts_data = eu_qrq_final_it_6
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df6 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T)
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Iteration 7                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Final scores ======================================================================================================

final_scores.ls7 <- best_scenario.fn(QRQ_flagging_system.df6)
names(final_scores.ls7) <- c("final_scores_nuts", "final_scores_country", "nuts_best")

### Data composition ======================================================================================================

# NUTS LEVEL

final_scores_nuts_final <- final_scores.ls6[["final_scores_nuts"]] %>%
  select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final)%>%
  mutate(
    scenario = "best scenario 7"
  )

eu_qrq_final_it_7 <- eu_qrq_final_it_6 %>%
  rbind(final_scores_nuts_final)

# COUNTRY LEVEL

final_scores_country_final <- final_scores.ls6[["final_scores_country"]] %>%
  select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
  mutate(
    scenario = "best scenario 7",
    best_scenario_final = "best scenario 7"
  )

EU_QRQ_country_it_7 <- EU_QRQ_country_it_6 %>%
  rbind(final_scores_country_final)

### QRQ validation ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = EU_QRQ_country_it_7, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = EU_QRQ_country_it_7, 
                                   analysis = "ROLI")

GPP_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_7, 
                                 analysis = "GPP")

TPS_nuts_validation <- QRQ_ranking.fn(data = eu_qrq_final_it_7,
                                      analysis = "NUTS")

Positions_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_7, 
                                             type = "position")

Scores_validation <- qrq_outlier_analysis(data = eu_qrq_final_it_7, 
                                          type = "score")

Capitals_analysis <- capitals.fn(data = eu_qrq_final_it_7)

### QRQ flagging system ======================================================================================================

QRQ_flagging_system.df7 <- flags_overview(type = "QRQ",
                                          QRQ_country_data = EU_QRQ_country_it_7,
                                          QRQ_nuts_data = eu_qrq_final_it_7
)

### Final table ======================================================================================================

final_table <- QRQ_flagging_system.df7 %>%
  group_by(scenario) %>%
  summarise(
    flags_POS_iqr = sum(c_flags_POS_iqr, na.rm = T),
    flags_SCORE_iqr = sum(c_flags_SCORE_iqr, na.rm = T),
    flags_CAPITALS_iqr = sum(c_flags_CAPITALS_iqr, na.rm = T),
    total_flags_iqr = sum(total_flags_iqr, na.rm = T),
    total = mean(c(flags_POS_iqr, flags_SCORE_iqr, flags_CAPITALS_iqr, total_flags_iqr, na.rm = T))
  )
