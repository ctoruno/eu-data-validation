## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            MATCH ROLI
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Natalia Rodriguez
##
## Dependencies:      World Justice Project
##
## Creation date:     September 3rd, 2024
##
## This version:      September 3rd, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Exercise 1                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

"%!in%" <- compose("!", "%in%")

overall_ROLI <- ROLI_2023 %>%
  select(Country, `Country Code`, Region, 
         `WJP Rule of Law Index: Overall Score`, 
         starts_with("Factor")
         ) %>%
  filter(Region %in% "EU + EFTA + North America") %>%
  filter(Country %!in% c("Canada", "United Kingdom", "United States", "Norway")) %>%
  rename(ROLI = "WJP Rule of Law Index: Overall Score",
         "F1" = "Factor 1: Constraints on Government Powers",
         "F2" = "Factor 2: Absence of Corruption",
         "F3" = "Factor 3: Open Government",
         "F4" = "Factor 4: Fundamental Rights",
         "F6" = "Factor 6: Regulatory Enforcement",
         "F7" = "Factor 7: Civil Justice",
         "F8" = "Factor 8: Criminal Justice") %>%
  pivot_longer(cols = !c(Country, `Country Code`, Region), 
               names_to = "indicator", values_to = "ROLI_value") %>%
  filter(indicator %!in% c("Factor 5: Order and Security")) %>%
  select(country_name_ltn = Country,
         indicator,
         ROLI_value) %>%
  mutate(country_name_ltn = if_else(country_name_ltn %in% "Slovak Republic", "Slovakia", country_name_ltn))

EU_ROLI_Factors <- EU_QRQ_country_final_scores %>%
  filter(indicator %in% c("p_1A", "p_1B", "p_2", "p_3A", "p_3B",
                          "p_4", "p_6", "p_7", "p_8")) %>%
  mutate(
    indicator = 
      case_when(
        indicator %in% c("p_1A", "p_1B") ~ "F1",
        indicator %in% c("p_2")          ~ "F2",
        indicator %in% c("p_3A", "p_3B") ~ "F3",
        indicator %in% c("p_4")          ~ "F4",
        indicator %in% c("p_6")          ~ "F6",
        indicator %in% c("p_7")          ~ "F7",
        indicator %in% c("p_8")          ~ "F8"
        )
  ) %>%
  group_by(country_name_ltn, indicator) %>%
  summarise(
    EU_value = mean(QRQ_value, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(country_name_ltn) %>%
  mutate(ROLI = mean(EU_value)) %>%
  pivot_wider(id_cols = c(country_name_ltn, ROLI), names_from = indicator, values_from = EU_value) %>%
  pivot_longer(cols = !c(country_name_ltn), names_to = "indicator",values_to = "EU_value")

overall_comparisson <- EU_ROLI_Factors %>%
  left_join(overall_ROLI, by = c("country_name_ltn", "indicator")) %>%
  group_by(indicator) %>%
  mutate(
    rank_EU    = rank(-EU_value),
    rank_ROLI  = rank(-ROLI_value),
    rank_difference = rank_EU - rank_ROLI
  )

results_comparisson_overall <- overall_comparisson %>%
  group_by(country_name_ltn) %>%
  summarise(average_differences = mean(rank_difference, na.rm = T),
            average_abs_differences = mean(abs(rank_difference), na.rm = T))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Exercise 2                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset_ROLI.df <- ROLI_matches_with_EU %>%
  rowwise() %>%
  mutate(
    p_7_1_2 = 
      f_7_1_1,
    p_7_2_1 =
      f_7_1_2,
    p_7_3_1 =
      mean(
        c(f_7_1_3, f_7_1_4, f_7_1_5)
      ),
    p_7_3_2 =
      f_7_5,
    p_7_4_1 = 
      f_7_2,
    p_7_4_3 =
      f_7_3,
    p_7_4_4 =
      f_7_4,
    p_7_5_2 =
      f_7_6,
    p_7_6_1 =
      mean(
        c(f_7_7_1, f_7_7_3)
      ),
    p_7_6_2 =
      f_7_7_4,
    p_7_6_3 =
      f_7_7_2,
    p_8_1_1 =
      f_8_1,
    p_8_1_2 =
      mean(
        c(f_8_4_1, f_8_5_1)
      ),
    p_8_3_1 =
      f_8_2_2,
    p_8_3_2 =
      f_8_2_1,
    p_8_3_3 =
      f_8_4_2,
    p_8_3_4 =
      f_8_5_2,
    p_8_3_5 =
      f_8_6,
    p_8_6_1 =
      f_8_7_1,
    p_8_6_3 =
      f_8_7_3,
    p_8_6_4 =
      f_8_7_4,
    p_8_6_5 =
      f_8_7_2,
    p_8_7_1 =
      f_8_3,
    p_1_01 =
      f_1_1,
    p_1_02 =
      f_1_2,
    p_1_03 =
      f_1_3,
    p_1_04 =
      f_1_4,
    p_1_05 =
      f_1_6,
    p_1_06_2 =
      f_1_5_2,
    p_1_06_3 =
      f_1_5_3,
    p_1_06_5 =
      f_1_5_1,
    p_3_2_1 =
      f_3_3_2,
    p_3_2_2 =
      f_3_3_1,
    p_3_2_3 =
      f_3_3_1,
    p_3_2_4 =
      f_3_3_1,
    p_3_2_5 =
      f_3_3_3,
    p_4_03 =
      f_4_5,
    p_4_04 =
      f_4_7,
    p_4_05_1 =
      f_4_4_2,
    p_4_05_2 =
      f_4_4_3,
    p_4_05_3 =
      f_4_4_3,
    p_4_08 =
      f_4_1,
    p_4_09_2 =
      f_4_8_2,
    p_4_09_4 =
      f_4_8_3,
    p_4_12_1 =
      f_4_4_1,
    p_4_14_1 =
      f_4_3_1,
    p_4_14_3 =
      f_4_3_3,
    p_4_14_4 =
      f_4_3_4,
    p_4_14_5 =
      f_4_3_2,
    p_2_1_1 =
      f_2_3,
    p_2_2_1 =
      f_2_1_1,
    p_2_2_2 = 
      f_2_4,
    p_2_3_1 =
      f_2_1_3,
    p_2_4_1 =
      f_2_2_2,
    p_3_1_2 =
      f_3_1_1,
    p_3_1_3 =
      f_3_2,
    p_6_1_2 =
      f_6_3,
    p_6_2 =
      f_6_5,
    p_6_3 =
      f_6_1
  ) %>%
  ungroup() %>%
  select(country, starts_with("p")) %>%
  pivot_longer(cols = !country, names_to = "indicator", values_to = "ROLI_value") %>%
  rename(country_name_ltn = country) %>%
  mutate(country_name_ltn = if_else(country_name_ltn %in% "Slovak Republic", "Slovakia", country_name_ltn))

comparisson_topics <- data_subset_ROLI.df %>%
  left_join(y = EU_QRQ_country_final_scores_topics %>%
              select(country_name_ltn, indicator, EU_value = QRQ_value),
            by = c("country_name_ltn", "indicator")
  ) %>%
  group_by(indicator) %>%
  mutate(
    rank_EU    = rank(-EU_value),
    rank_ROLI  = rank(-ROLI_value),
    rank_difference = rank_EU - rank_ROLI
  )

plot <- ggplot(comparisson_topics, aes(x = rank_difference)) +
  geom_histogram(binwidth = 1, fill = "#3273ff", color = "black") +
  geom_vline(xintercept = c(-10, -5, 5, 10), color = "#DE003F", linetype = "dashed", size = 0.75) +
  labs(title = "Diferencias de rankings entre scores de EU y ROLI", 
       x = "Diferencias en ranking", 
       y = "Frecuencia de diferencias") +
  theme_minimal() +
  scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, by = 5)) +
  theme(legend.key = element_blank(),
        legend.position = "top",
        axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.5, 
                                          colour = "grey75", 
                                          linetype = "dotted"),
        panel.grid.major.x = element_blank())

ggsave(plot = plot, 
       filename = paste0(getwd(),
                         "/Outputs/Diff_ROLI_EU.svg"),
       width = 189.7883,
       height = 189.7883,
       units  = "mm",
       dpi    = 72,
       device = "svg")

results_comparisson_topics <- comparisson_topics %>%
  group_by(country_name_ltn) %>%
  summarise(average_differences = mean(rank_difference, na.rm = T),
            average_abs_differences = mean(abs(rank_difference), na.rm = T))

resultados <- list(overall_comparisson, results_comparisson_overall, comparisson_topics, results_comparisson_topics)

openxlsx::write.xlsx(x = resultados, 
                     file = paste0(getwd(),
                                   "/Outputs/ROLI_EU.xlsx"))
