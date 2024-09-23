cases_negative <- INTERNAL_ranking_analysis.df %>%
  mutate(
    diff_scores = value - prev_value
  ) %>%
  filter(Trend == "Negative") %>%
  group_by(country_name_ltn) %>%
  summarise(
    avg_diff_ranking = mean(Diff_Rank, na.rm = T),
    avg_diff_scores = mean(diff_scores, na.rm = T)
  )

cases_positive <- INTERNAL_ranking_analysis.df %>%
  mutate(
    diff_scores = value - prev_value
  ) %>%
  filter(Trend == "Positive") %>%
  group_by(country_name_ltn) %>%
  summarise(
    avg_diff_ranking = mean(Diff_Rank, na.rm = T),
    avg_diff_scores = mean(diff_scores, na.rm = T)
  )


# Bribery

bribery_data <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("BRB")) %>%
  mutate(
    across(
      starts_with("BRB"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  mutate(
    BRBA = 
      case_when(
        BRB_permit_A == 1 | BRB_benefits_A == 1 | BRB_id_A == 1 | BRB_school_A == 1 | BRB_health_A == 1 ~ 1,
        is.na(BRB_permit_A) == T & is.na(BRB_benefits_A) == T & is.na(BRB_id_A) == T & is.na(BRB_school_A) == T & is.na(BRB_health_A) == T ~ NA_real_, 
        T ~ 0,
      ),
    BRBB =
      case_when(
        BRB_permit_B == 1 | BRB_benefits_B == 1 | BRB_id_B == 1 | BRB_school_B == 1 | BRB_health_B == 1 ~ 1,
        is.na(BRB_permit_B) == T & is.na(BRB_benefits_B) == T & is.na(BRB_id_B) == T & is.na(BRB_school_B) == T & is.na(BRB_health_B) == T ~ NA_real_,
        BRB_permit_B == 0 | BRB_benefits_B == 0 | BRB_id_B == 0 | BRB_school_B == 0 | BRB_health_B == 0 ~ 0,
      )
  ) %>%
  group_by(country_name_ltn, country_code_nuts) %>%
  summarise(
    across(
      c(BRBA , BRBB),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  mutate(highlight = BRBB > 0.20,
         IQR = 29.5)

writexl::write_xlsx(bribery_data, path = "bribery_country.xlsx")
mean_BRBA<- mean(bribery_data$BRBA, na.rm = TRUE)
mean_BRBB<- mean(bribery_data$BRBB, na.rm = TRUE)

# Calcular el IQR de BRB2
Q1 <- quantile(bribery_data$BRBB, 0.25, na.rm = TRUE)
Q3 <- quantile(bribery_data$BRBB, 0.75, na.rm = TRUE)
IQR_BRB2 <- Q3 - Q1
Q1 <- (Q1-(IQR_BRB2*1.5))*100
Q3 <- (Q3+(IQR_BRB2*1.5))*100

# Supongamos que las dos variables se llaman BRB1 y BRB2
plot <- ggplot(bribery_data, aes(y = BRBA*100, 
                         x = BRBB*100)) +
  # Sombrear la zona fuera del IQR
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Q1), fill = "#FFB1B9", alpha = 0.2) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = Q3, xmax = Inf), fill = "#FFB1B9", alpha = 0.2) +
  # Puntos y otros elementos del gráfico
  geom_vline(xintercept = mean_BRBB*100, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean_BRBA*100, linetype = "dashed", color = "black") +
  geom_text(aes(label = country_code_nuts), vjust = -0.5, hjust = 0.5) + 
  geom_point(aes(color = highlight), size = 3, show.legend = F) +
  scale_color_manual(values = c("FALSE" = "#99D7DD", "TRUE" = "#FA4D57")) +
  labs(title = "Experiences of Corruption in Government Services", 
       subtitle = "Proportion of people who experienced an event of corruption with government services") + 
  xlab("Proportion of people who experienced a corruption experience") +
  ylab("Proportion of people who had contact with government services") +
  theme_minimal()  + 
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0,100,25),
                     labels = paste0(seq(0,100,25), "%"),
                     position = "left") + 
  scale_x_continuous(limits = c(0, 30),
                     breaks = seq(0,30,5),
                     labels = paste0(seq(0,30,5), "%")
                     ) +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3")
    ) 

ggsave(plot = plot, filename = "Bribery.svg")

bribery_data_nuts <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("BRB")) %>%
  mutate(
    across(
      starts_with("BRB"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  mutate(
    BRBA = 
      case_when(
        BRB_permit_A == 1 | BRB_benefits_A == 1 | BRB_id_A == 1 | BRB_school_A == 1 | BRB_health_A == 1 ~ 1,
        is.na(BRB_permit_A) == T & is.na(BRB_benefits_A) == T & is.na(BRB_id_A) == T & is.na(BRB_school_A) == T & is.na(BRB_health_A) == T ~ NA_real_, 
        T ~ 0,
      ),
    BRBB =
      case_when(
        BRB_permit_B == 1 | BRB_benefits_B == 1 | BRB_id_B == 1 | BRB_school_B == 1 | BRB_health_B == 1 ~ 1,
        is.na(BRB_permit_B) == T & is.na(BRB_benefits_B) == T & is.na(BRB_id_B) == T & is.na(BRB_school_B) == T & is.na(BRB_health_B) == T ~ NA_real_,
        BRB_permit_B == 0 | BRB_benefits_B == 0 | BRB_id_B == 0 | BRB_school_B == 0 | BRB_health_B == 0 ~ 0,
        )
  ) %>%
  group_by(nuts_id) %>%
  summarise(
    across(
      c(BRBA , BRBB),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  mutate(highlight = BRBB > 0.20) %>%
  mutate(
    IQR = 28.4
  )

writexl::write_xlsx(bribery_data_nuts, path = "bribery_nuts.xlsx")

mean_BRBA_nuts<- mean(bribery_data_nuts$BRBA, na.rm = TRUE)
mean_BRBB_nuts<- mean(bribery_data_nuts$BRBB, na.rm = TRUE)

# Calcular el IQR de BRB2
Q1 <- quantile(bribery_data_nuts$BRBB, 0.25, na.rm = TRUE)
Q3 <- quantile(bribery_data_nuts$BRBB, 0.75, na.rm = TRUE)
IQR_BRB2 <- Q3 - Q1
Q1 <- (Q1-(IQR_BRB2*1.5))*100
Q3 <- (Q3+(IQR_BRB2*1.5))*100
# Supongamos que las dos variables se llaman BRB1 y BRB2
plot <- ggplot(bribery_data_nuts, 
       aes(y = BRBA*100, 
           x = BRBB*100)) +
  # Sombrear la zona fuera del IQR
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Q1), fill = "#FFB1B9", alpha = 0.2) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = Q3, xmax = Inf), fill = "#FFB1B9", alpha = 0.2) +
  # Puntos y otros elementos del gráfico
  geom_vline(xintercept = mean_BRBB*100, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean_BRBA*100, linetype = "dashed", color = "black") +
  geom_text(aes(label = nuts_id), vjust = -0.5, hjust = 0.5) + 
  geom_point(aes(color = highlight), , size = 3, show.legend = F) +
  scale_color_manual(values = c("FALSE" = "#99D7DD", "TRUE" = "#FA4D57")) +
  labs(title = "Experiences of Corruption in Government Services, by NUTS regions", 
       subtitle = "Proportion of people who experienced an event of corruption with government services") + 
  xlab("Proportion of people who experienced a corruption experience") +
  ylab("Proportion of people who had contact with government services") +
  theme_minimal()  + 
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0,100,25),
                     labels = paste0(seq(0,100,25), "%"),
                     position = "left") + 
  scale_x_continuous(limits = c(0, 30),
                     breaks = seq(0,30,5),
                     labels = paste0(seq(0,30,5), "%")
  ) +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3")
  ) 

ggsave(plot = plot, filename = "Bribery_NUTS.svg")



bribery_data_all <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("BRB")) %>%
  mutate(
    across(
      starts_with("BRB"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  group_by(country_name_ltn, country_code_nuts) %>%
  summarise(
    across(
      starts_with("BRB"),
      ~mean(.x, na.rm = T)
    )
  )%>%
  ungroup() %>%
  mutate(
    across(
      starts_with("BRB"),
      ~IQR(.x, na.rm = TRUE),
      .names = "IQR_{col}"
    ),
    across(
      starts_with("BRB"),
      ~quantile(.x, 0.75, na.rm = TRUE),
      .names = "Q3_{col}"
    ),
    IQR_BRB_permit_B = (Q3_BRB_permit_B +(IQR_BRB_permit_B*1.5)),
    IQR_BRB_benefits_B = (Q3_BRB_benefits_B +(IQR_BRB_benefits_B*1.5)),
    IQR_BRB_id_B = (Q3_BRB_id_B +(IQR_BRB_id_B*1.5)),
    IQR_BRB_school_B = (Q3_BRB_school_B +(IQR_BRB_school_B*1.5)),
    IQR_BRB_health_B = (Q3_BRB_health_B +(IQR_BRB_health_B*1.5))
  ) %>%
  select(country_name_ltn, starts_with("BRB"), starts_with("IQR"))
writexl::write_xlsx(bribery_data_all, path = "bribery_all.xlsx")


bribery_data_nuts_all <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("BRB")) %>%
  mutate(
    across(
      starts_with("BRB"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  group_by(nuts_id) %>%
  summarise(
    across(
      starts_with("BRB"),
      ~mean(.x, na.rm = T)
    )
  )%>%
  ungroup() %>%
  mutate(
    across(
      starts_with("BRB"),
      ~IQR(.x, na.rm = TRUE),
      .names = "IQR_{col}"
    ),
    across(
      starts_with("BRB"),
      ~quantile(.x, 0.75, na.rm = TRUE),
      .names = "Q3_{col}"
    ),
    IQR_BRB_permit_B = (Q3_BRB_permit_B +(IQR_BRB_permit_B*1.5)),
    IQR_BRB_benefits_B = (Q3_BRB_benefits_B +(IQR_BRB_benefits_B*1.5)),
    IQR_BRB_id_B = (Q3_BRB_id_B +(IQR_BRB_id_B*1.5)),
    IQR_BRB_school_B = (Q3_BRB_school_B +(IQR_BRB_school_B*1.5)),
    IQR_BRB_health_B = (Q3_BRB_health_B +(IQR_BRB_health_B*1.5))
  ) %>%
  select(nuts_id, starts_with("BRB"), starts_with("IQR"))
writexl::write_xlsx(bribery_data_nuts_all, path = "bribery_all_nuts.xlsx")

# Discrimination

discrimination_data <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("DIS")) %>%
  select(1:14) %>%
  mutate(
    across(
      starts_with("DIS"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  group_by(country_name_ltn, country_code_nuts) %>%
  summarise(
    across(
      starts_with("DIS"),
      ~mean(.x, na.rm = T)
    )
  ) 

discrimination_data_NUTS <- master_data.df %>%
  select(country_name_ltn, country_code_nuts, nuts_id, starts_with("DIS")) %>%
  select(1:14) %>%
  mutate(
    across(
      starts_with("DIS"),
      ~case_when(
        .x == 2 ~ 0,
        .x == 1 ~ 1
      )
    )
  ) %>%
  group_by(nuts_id) %>%
  summarise(
    across(
      starts_with("DIS"),
      ~mean(.x, na.rm = T)
    )
  ) 

