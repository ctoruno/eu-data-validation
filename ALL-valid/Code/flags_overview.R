flags_overview <- function(
  type = "GPP",
  QRQ_country_data = EU_QRQ_country,
  QRQ_nuts_data = eu_qrq_final,
  TPS_validation,
  ROLI_validation,
  TPS_nuts_validation,
  GPP_validation,
  Positions_validation,
  Scores_validation,
  Capitals_analysis
  ) {

  # GPP ==================================================================================
  
  if(type == "GPP") {
    
    ## 1.1 HTML flags ==================================================================================
    
    # Create a new dataframe 'html_flags' from 'html_flags.df'
    # Select only the 'Country' and 'GPP_Variable_Name' columns and add a new column 'HTML_flag' with all values set to "Red"
    # Ensure the dataframe contains only unique rows
    
    html_flags <- html_flags.df %>%
      select(Country, GPP_Variable_Name) %>%
      mutate("HTML_flag" = "Red") %>%
      distinct()
    
    # Initialize an empty dataframe 'df' with columns 'Country' and 'GPP_Variable_Name'
    
    df <- data.frame(matrix(nrow = 0, ncol = 2))
    colnames(df) <- c("Country", "GPP_Variable_Name")
    
    # Populate 'df' with combinations of unique countries from 'html_flags.df' and 'reportvarslist' which contain the variables from the report
    fullvars<- unique(c(TPS_ranking_analysis.df$question, INTERNAL_ranking_analysis.df$question, html_flags.df$GPP_Variable_Name))
    
    for (i in countrylist) {
      df <- rbind(df, tibble("Country" = rep(i, length(fullvars)),
                             "GPP_Variable_Name" = fullvars))
    }
    
    # Join 'df' with 'html_flags' and remove duplicate rows
    df2 <- left_join(df, html_flags) %>% 
      distinct()
    
    # Replace all NA values in 'df2' with "Green"
    df2[is.na(df2)] <- "Green"
    
    ## 1.2 Ranking flags ==================================================================================
    
    # TPS
    
    # Prepare 'ranking' dataframe by selecting and reshaping relevant columns from 'TPS_ranking_analysis.df'
    
    ranking <- TPS_ranking_analysis.df %>%
      ungroup() %>%
      select(country_name_ltn, question, flagged_questions, Type_Survey, Trend) %>%
      distinct() %>%
      pivot_wider(names_from = Type_Survey, values_from = flagged_questions, values_fn = list) %>%
      unnest(cols = everything()) %>%
      arrange(country_name_ltn, question)
    
    # Join 'df3' with 'ranking' and rename columns for consistency
    
    df3 <- left_join(df2, ranking %>%
                       rename("Country" = "country_name_ltn",
                              "GPP_Variable_Name" = "question",
                              "Population_ranking_flag" = "population",
                              "Expert_ranking_flag" = "expert",
                              "Trend_TPS" = "Trend"))
    
    # Remove trailing text from 'Population_ranking_flag' and 'Expert_ranking_flag' values
    
    df3$Population_ranking_flag <- gsub(" .*", "", df3$Population_ranking_flag)
    df3$Expert_ranking_flag <- gsub(" .*", "", df3$Expert_ranking_flag)
    
    # INTERNAL
    
    # Join 'df4' with 'ranking' and rename columns for consistency
    
    df4 <- df3 %>%
      left_join(INTERNAL_ranking_analysis.df %>%
                  select(Country = country_name_ltn, 
                         GPP_Variable_Name = question, 
                         Internal_ranking_flag = flagged_questions,
                         Trend_GPP = Trend),
                by = c("Country", "GPP_Variable_Name"),
                relationship = "many-to-many"
      )
    
    df4$Internal_ranking_flag <- gsub(" .*", "", df4$Internal_ranking_flag)
    
    ## 1.4 Theorethical framework ==================================================================================
    
    # Select relevant columns from 'master_data.df' and normalize the variables
    
    gpp <- master_data.df %>% 
      select(country_name_ltn, nuts_id, 
             all_of(fullvars))
    
    # Normalize the variables
    normalized <- normalizingvars(gpp, fullvars)
    
    # Aggregate the normalized variables at the country level
    gppaggregate <- normalized %>%
      group_by(country_name_ltn, nuts_id) %>%
      summarise_at(fullvars, mean, na.rm= TRUE) %>%
      pivot_longer(cols = all_of(fullvars), names_to = "GPP_Variable_Name", values_to = "Score") %>%
      left_join(weight.df%>% 
                  select(nuts_id, regionpoppct))%>%
      group_by(country_name_ltn, GPP_Variable_Name)%>%
      mutate(value_weighted = sum(Score*regionpoppct))%>%
      select(country_name_ltn, GPP_Variable_Name, value_weighted)%>%
      distinct()%>%
      rename(Score = value_weighted)%>%
      rename("Country" = "country_name_ltn")
    
    # Prepare 'subp' dataframe by selecting and joining relevant columns from 'metadata' and 'QRQ_description'
    # This step is necessary to join the theoretical framework with the scores and the analyses
    
    subp <- metadata %>%
      select(GPP_Variable_Name = DAU_GPP, Question, subpillar = sub_pillar)
    
    ## 1.5 Join everything ==================================================================================
    
    # Join 'df5' with 'gppaggregate' and 'subp' dataframes
    
    df5 <- left_join(df4, gppaggregate) %>%
      mutate(
        GPP_Variable_Name = if_else(GPP_Variable_Name == "CPA_protest", "CP_protest",
                                    if_else(GPP_Variable_Name == "CPB_consultation", "CP_consultation",
                                    GPP_Variable_Name))
      )
    
    ## 1.6 Flagging system ==================================================================================
    
    # In this step we create the flagging system according to the conditions we established
    # Please remember that the case_when is hierarchical that's means the initial condition is greater than the rest. The order matters.
    
    df6 <- df5 %>%
      mutate(
        Trends = 
          if_else(Trend_TPS == Trend_GPP, "Same trends", "Opposite trends")
      ) %>%
      mutate(
        Final_flag = 
          case_when(
            # If 'HTML_flag', 'Population_ranking_flag', and 'Expert_ranking_flag' are all NA, set 'Final_flag' to "Red"
            is.na(Population_ranking_flag) & is.na(Expert_ranking_flag) & is.na(Internal_ranking_flag) ~ "No information",
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Red" and the trends are the same, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" & Population_ranking_flag == "Red" & Trends == "Same trends"~ "Red", 
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Red" and the trends are opposite, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" & Population_ranking_flag == "Red" & Trends == "Opposite trends"~ "Green",
            
            # If 'Internal Ranking' is "Red", the 'Expert_ranking_flag' is "Red" and the trends are the same, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" & Expert_ranking_flag == "Red" & Trends == "Same trends"~ "Red", 
            
            # If 'Internal Ranking' is "Red", the 'Expert_ranking_flag' is "Red" and the trends are opposite, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" & Expert_ranking_flag == "Red" & Trends == "Opposite trends"~ "Green",
            
            # If 'Internal Ranking' is "Green" and 'Population_ranking_flag' is "Green", set 'Final_flag' to "Green"
            Internal_ranking_flag == "Green" & Population_ranking_flag == "Green" ~ "Green", 
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Green", the trends are same and the 'HTML_flag' or 'Expert_ranking_flag' are "Green", set 'Final_flag' to "Green"
            Internal_ranking_flag == "Red" & Population_ranking_flag == "Green" & Trends == "Same trends" & (Expert_ranking_flag == "Green" | HTML_flag == "Green") ~ "Green",
            
            # If 'Internal Ranking' is "Green", the 'Population_ranking_flag' is "Red", the trends are same and the 'HTML_flag' or 'Expert_ranking_flag' are "Green", set 'Final_flag' to "Green"
            Internal_ranking_flag == "Green" & Population_ranking_flag == "Red" & Trends == "Same trends" & (Expert_ranking_flag == "Green" | HTML_flag == "Green") ~ "Green",
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Green", the trends are same and we don't have information about HTML or experts, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" & Population_ranking_flag == "Green" & Trends == "Same trends" ~ "Red",
            
            # If 'Internal Ranking' is "Green", the 'Population_ranking_flag' is "Red" and the trends are same and we don't have information about HTML or experts, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Green" & Population_ranking_flag == "Red" & Trends == "Same trends" ~ "Red",
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Green" and the trends are opposite, set 'Final_flag' to "Green"
            Internal_ranking_flag == "Red" & Population_ranking_flag == "Green" & Trends == "Opposite trends" ~ "Green",
            
            # If 'Internal Ranking' is "Green", the 'Population_ranking_flag' is "Red" and the trends are opposite, set 'Final_flag' to "Green"
            Internal_ranking_flag == "Green" & Population_ranking_flag == "Red" & Trends == "Opposite trends" ~ "Green",
            
            # If 'Internal Ranking' is "Red", the 'Population_ranking_flag' is "Green" and the trends are opposite, set 'Final_flag' to "Green"
            Internal_ranking_flag == "Red" & Expert_ranking_flag == "Green" & Trends == "Opposite trends" ~ "Green",
            
            # If 'Internal Ranking' is "Green", the 'Population_ranking_flag' is "Red" and the trends are opposite, set 'Final_flag' to "Green"
            Internal_ranking_flag == "Green" & Expert_ranking_flag == "Red" & Trends == "Opposite trends" ~ "Green",
            
            # If 'Internal Ranking' is "Red" and 'Population_ranking_flag' is missing, set 'Final_flag' to "Red"
            Internal_ranking_flag == "Red" ~ "Red", 
            
            # If 'Internal Ranking' is "Green" and 'Population_ranking_flag' is missing, set 'Final_flag' to "Green"
            Internal_ranking_flag == "Green" ~ "Green", 
            
            # If 'Population_ranking_flag' is "Red" and 'Internal Ranking' is missing, set 'Final_flag' to "Red"
            Population_ranking_flag == "Red" ~ "Red", 
            
            # If 'Population_ranking_flag' is "Green" and 'Internal Ranking' is missing, set 'Final_flag' to "Green"
            Population_ranking_flag == "Green" ~ "Green", 
            
            # If 'Expert_ranking_flag' is "Red" and 'HTML_flag' is "Red" and 'Internal Ranking' and 'Population_ranking_flag' is missing, set 'Final_flag' to "Red"
            Expert_ranking_flag == "Red" & HTML_flag == "Red" ~ "Red",
            
            # If 'Expert_ranking_flag' is "Green" and 'HTML_flag' is "Green", and 'Internal Ranking' and 'Population_ranking_flag' is missing, set 'Final_flag' to "Green"
            Expert_ranking_flag == "Green" & HTML_flag == "Green" ~ "Green",
            
            # If 'Expert_ranking_flag' is "Red" and 'HTML_flag' is "Green", and 'Internal Ranking' and 'Population_ranking_flag' is missing, set 'Final_flag' to "Red"
            Expert_ranking_flag == "Red"  & HTML_flag == "Green" ~ "Green",
            
            # If 'Expert_ranking_flag' is "Green" and 'HTML_flag' is "Red", and 'Internal Ranking' and 'Population_ranking_flag' is missing, set 'Final_flag' to "Red"
            Expert_ranking_flag == "Green"  & HTML_flag == "Red" ~ "Green",
            
            # For all other cases, set 'Final_flag' to "Green"
            TRUE ~ NA_character_
          )
      )%>%
      select(Country, GPP_Variable_Name , Score, HTML_flag , Population_ranking_flag , Expert_ranking_flag , Internal_ranking_flag , starts_with("Trend"), Final_flag) %>%
      distinct() %>%
      drop_na(Score)
    
    ## 1.6 Subpillars names ==================================================================================
    
    # In this step we create merge the questions with each subpillars 
    # IMPORTANT: This step will generate duplicates because there are question which corresponds to many subpillars
    
    df7 <- df6 %>%
      left_join(subp, by = "GPP_Variable_Name", relationship = "many-to-many")
    
    # Return the final dataframe 'df7'
    
    
    return(df7)
    
  } else {
    
    # QRQ ==================================================================================
    
    ## TPS flags ==================================================================================
    
    # Assign the dataframe EU_QRQ_country to df1
    df1 <- QRQ_country_data
    QRQ_nuts_data <- QRQ_nuts_data
    TPS_validation <- TPS_validation
    ROLI_validation <- ROLI_validation
    TPS_nuts_validation <- TPS_nuts_validation
    
    # Define the '%!in%' operator to check for elements not in a vector
    "%!in%" <- compose("!", "%in%")
    
    direction_TPS <- df1 %>%
      full_join(TPS_validation %>%
                  select(country_name_ltn, indicator, TPS_flag_iqr,  scenario, Rank_QRQ, Rank_TPS),
                by = c("country_name_ltn", "indicator", "scenario")
      ) %>%
      mutate(
        TPS_flag_iqr = str_replace_all(TPS_flag_iqr, " Flag", ""),
        c_flags_TPS_iqr = 
          case_when(
            TPS_flag_iqr == "Red" ~ 1,
            TPS_flag_iqr == "Green" ~ 0,
            T ~ NA_real_
          )
        ) %>% 
      group_by(country_name_ltn, indicator, scenario) %>%
      mutate(
        c_flags_TPS_iqr = sum(c_flags_TPS_iqr, na.rm = TRUE),
        tps_mean = mean(Rank_TPS),
        diff = Rank_QRQ - Rank_TPS,
        direction = if_else(diff < 0, "Negative", "Positive")
      ) %>%
      select(country_name_ltn, indicator, scenario,direction) %>%
      distinct() %>%
      mutate(
        direction   = first(direction)
      ) %>%
      distinct()
    
    print(table(direction_TPS$scenario, direction_TPS$direction))
    
    # Join EU_QRQ_country with TPS_validation, filter out certain indicators, and calculate TPS flags
    df2 <- df1  %>%
      full_join(TPS_validation %>%
                  select(country_name_ltn, indicator, TPS_flag_iqr,  scenario),
                by = c("country_name_ltn", "indicator", "scenario")
      ) %>%
      mutate(
        TPS_flag_iqr = str_replace_all(TPS_flag_iqr, " Flag", ""),
        c_flags_TPS_iqr = 
          case_when(
            TPS_flag_iqr == "Red" ~ 1,
            TPS_flag_iqr == "Green" ~ 0,
            T ~ NA_real_
          )
      ) %>% 
      group_by(country_name_ltn, indicator, scenario, QRQ_value) %>%
      summarise(c_flags_TPS_iqr = sum(c_flags_TPS_iqr, na.rm = TRUE)) %>%
      left_join(direction_TPS, by = c("country_name_ltn", "indicator", "scenario"), relationship = "many-to-many") %>%
      mutate(
        c_flags_TPS_iqr = if_else(is.na(direction), NA_real_, c_flags_TPS_iqr)
      ) %>%
      rename(trend_TPS = direction) %>%
      distinct()
    
    print(table(df2$scenario, df2$c_flags_TPS_iqr))
    
    ## ROLI flags ==================================================================================
    
    # Join df2 with ROLI_validation, calculate ROLI flags, and summarize total flags per country and indicator
    df3 <- df2 %>%
      full_join(ROLI_validation %>%
                  select(country_name_ltn, indicator, QRQ_value, ROLI_flag_tr, ROLI_flag_iqr, scenario, Trend),
                by = c("country_name_ltn", "indicator", "scenario", "QRQ_value"), relationship = "many-to-many"
      ) %>%
      mutate(
        ROLI_flag_iqr = str_replace_all(ROLI_flag_iqr, " Flag", ""),
        c_flags_ROLI_iqr = if_else(ROLI_flag_tr == "Red", 1, 0)
      ) %>%
      select(country_name_ltn, indicator, QRQ_value, c_flags_TPS_iqr, trend_TPS, c_flags_ROLI_iqr, trend_ROLI = Trend, scenario) %>%
      distinct()
    
    print(table(df3$scenario, df3$c_flags_TPS_iqr))
    
    ## NUTS flags ==================================================================================
    
    # Prepare df4 by renaming columns and filtering indicators
    df4 <- QRQ_nuts_data %>%
      rename(QRQ_NUTS_value = QRQ_value,
             country_name_ltn = country) %>%
      left_join(df3, by = c("country_name_ltn", "indicator", "scenario"), relationship = "many-to-many") %>%
      rename(country_code_nuts = nuts,
             QRQ_country_value = QRQ_value) %>%
      left_join(GPP_validation %>%
                  select(country_name_ltn, country_code_nuts, indicator, 
                         QRQ_NUTS_value = QRQ_value, trend_GPP = Trend, 
                         GPP_flag_iqr, scenario), 
                by = c("country_name_ltn", "country_code_nuts", "indicator", "scenario", "QRQ_NUTS_value"), relationship = "many-to-many") %>%
      mutate(
        GPP_flag_iqr = str_replace_all(GPP_flag_iqr, " Flag", ""),
        c_flags_GPP_iqr = if_else(GPP_flag_iqr == "Red", 1, 0)
      ) %>%
      select(country_name_ltn, country_code_nuts, indicator, 
             QRQ_country_value, QRQ_NUTS_value, 
             trend_TPS, c_flags_TPS_iqr, 
             trend_ROLI, c_flags_ROLI_iqr, 
             trend_GPP, c_flags_GPP_iqr, 
             scenario, best_scenario_final) %>%
      distinct()
    
    df5 <- df4 %>%
      full_join(TPS_nuts_validation %>%
                  select(country_code_nuts, indicator, TPS_NUTS_flag_iqr, QRQ_NUTS_value, trend_TPS_NUTS = Trend, scenario), 
                by = c("country_code_nuts", "indicator", "QRQ_NUTS_value", "scenario"), relationship = "many-to-many") %>%
      distinct() %>%
      mutate(
        TPS_NUTS_flag_iqr = str_replace_all(TPS_NUTS_flag_iqr, " Flag", ""),
        c_flags_TPS_NUTS_iqr = if_else(TPS_NUTS_flag_iqr == "Red", 1, 0)
      ) %>%
      select(country_name_ltn, country_code_nuts, indicator, 
             QRQ_country_value, QRQ_NUTS_value, 
             trend_TPS, c_flags_TPS_iqr, 
             trend_ROLI, c_flags_ROLI_iqr, 
             trend_GPP, c_flags_GPP_iqr, 
             trend_TPS_NUTS, c_flags_TPS_NUTS_iqr,
             scenario, best_scenario_final) %>%
      distinct() %>%
      mutate(
        counter = 1
      ) %>%
      group_by(country_code_nuts, indicator, QRQ_NUTS_value, scenario) %>%
      mutate(
        repeated = sum(counter)
      )
      
    print(table(df5$scenario, df5$c_flags_GPP_iqr))
    
    
    ## Flagging system ==================================================================================
    
    df6 <- df5 %>%
      group_by(country_name_ltn,country_code_nuts,indicator, scenario) %>%
      mutate(total_flags_iqr = sum(c(c_flags_TPS_iqr, c_flags_ROLI_iqr, c_flags_GPP_iqr, c_flags_TPS_NUTS_iqr), na.rm = T),
             total_flags_nuts_iqr = sum(c(c_flags_GPP_iqr, c_flags_TPS_NUTS_iqr))
             ) %>%
      mutate(
        total_flags_iqr = if_else(
          is.na(c_flags_TPS_iqr) & is.na(c_flags_ROLI_iqr) & is.na(c_flags_GPP_iqr) & is.na(c_flags_TPS_NUTS_iqr), NA_real_, total_flags_iqr
          ),
        total_flags_nuts_iqr = if_else(
          is.na(c_flags_GPP_iqr) & is.na(c_flags_TPS_NUTS_iqr), NA_real_, total_flags_iqr
        )
        ) %>%
      select(country_name_ltn, country_code_nuts, indicator, QRQ_country_value, QRQ_NUTS_value,
             trend_TPS, c_flags_TPS_iqr, 
             trend_ROLI, c_flags_ROLI_iqr, 
             trend_GPP, c_flags_GPP_iqr, 
             trend_TPS_NUTS, c_flags_TPS_NUTS_iqr,
             total_flags_iqr, total_flags_nuts_iqr,
             scenario, best_scenario_final) %>%
      arrange(country_code_nuts, indicator, scenario)

    print(table(df6$scenario, df6$total_flags_iqr))
    
    ## Outliers Analyses ==================================================================================
    
    df7 <- df6 %>%
      left_join(Positions_validation %>% 
                  select(Country, `NUTS Region`, Indicator, Scenario, Flag) %>%
                  rename(country_name_ltn = Country,
                         country_code_nuts = `NUTS Region`,
                         indicator = Indicator,
                         scenario = Scenario,
                         c_flags_POS_iqr = Flag), relationship = "many-to-many")%>%
      left_join(Scores_validation %>%
                  select(Country, `NUTS Region`, Indicator, Scenario, Flag) %>%
                  rename(country_name_ltn = Country,
                         country_code_nuts = `NUTS Region`,
                         indicator = Indicator,
                         scenario = Scenario,
                         c_flags_SCORE_iqr = Flag), relationship = "many-to-many") %>%
      left_join(Capitals_analysis %>%
                  select(Country, `NUTS Region`, Indicator, Scenario, capital, Flag) %>%
                  rename(country_name_ltn = Country,
                         country_code_nuts = `NUTS Region`,
                         indicator = Indicator,
                         scenario = Scenario,
                         c_flags_CAPITALS_iqr = Flag), relationship = "many-to-many"
                ) %>%
      distinct()

    df8 <- df7 %>%
      filter(
        indicator %!in% c("p_1_01_1", "p_1_01_2", "p_1_02_1", "p_1_03_1", "p_1_03_2", "p_1_03_3",
                          "p_1_03_4", "p_1_03_5", "p_1_04_1", "p_1_04_2", "p_1_05_1", "p_1_05_2",
                          "p_1_06_1", "p_1_06_2", "p_1_06_3", "p_1_06_4", "p_1_06_5", "p_1_06_6",
                          "p_1_07_1", "p_1_07_2", "p_1_08_1", "p_1_08_2", "p_1_08_3", "p_1_08_4",
                          "p_1_09_1", "p_1_09_2", "p_1_09_3", "p_1_10_1", "p_1_11_1", "p_1_12_1",
                          "p_1_12_2", "p_1_12_3", "p_2_1_1", "p_2_2_1",  "p_2_2_2", "p_2_3_1",
                          "p_2_4_1", "p_2_5_1",  "p_2_5_2", "p_3_1_1",  "p_3_1_2",  "p_3_1_3",
                          "p_3_2_1",  "p_3_2_2", "p_3_2_3",  "p_3_2_4",  "p_3_2_5",  "p_3_2_6",
                          "p_4_01_1", "p_4_02_1", "p_4_03_1", "p_4_04_1", "p_4_05_1", "p_4_05_2",
                          "p_4_05_3", "p_4_06_1", "p_4_07_1", "p_4_08_1", "p_4_08_2", "p_4_08_3",
                          "p_4_09_1", "p_4_09_2", "p_4_09_3", "p_4_09_4", "p_4_10_1", "p_4_11_1",
                          "p_4_11_2", "p_4_11_3", "p_4_12_1", "p_4_13_1", "p_4_14_1", "p_4_14_2",
                          "p_4_14_3", "p_4_14_4", "p_4_14_5", "p_6_1_1",  "p_6_1_2", "p_6_2_1",
                          "p_6_3_1",  "p_6_3_2",  "p_6_3_3",  "p_6_3_4", "p_7_1_1",  "p_7_1_2",
                          "p_7_2_1", "p_7_3_1",  "p_7_3_2", "p_7_4_1",  "p_7_4_2",  "p_7_4_3",  
                          "p_7_4_4", "p_7_5_1",  "p_7_5_2", "p_7_6_1",  "p_7_6_2",  "p_7_6_3",
                          "p_8_1_1",  "p_8_1_2", "p_8_2_1",  "p_8_2_2", "p_8_3_1",  "p_8_3_2",
                          "p_8_3_3",  "p_8_3_4",  "p_8_3_5", "p_8_4_1", "p_8_4_2",  "p_8_4_3",
                          "p_8_5_1", "p_8_6_1",  "p_8_6_2",  "p_8_6_3",  "p_8_6_4",  "p_8_6_5",
                          "p_8_7_1") 
      )%>%
      distinct() %>%
      mutate(
        need_to_review = 
          case_when(
            c_flags_POS_iqr == 1 ~ 1,
            c_flags_SCORE_iqr == 1 & c_flags_CAPITALS_iqr == 1 ~ 1,
            T ~ 0
          )
      )
      
    print(table(df8$scenario, df8$total_flags_iqr))
    print(table(df8$scenario, df8$c_flags_POS_iqr))
    print(table(df8$scenario, df8$c_flags_SCORE_iqr))
    print(table(df8$scenario, df8$c_flags_CAPITALS_iqr))
    
    return(df8)
    
  }
}
