nuts_flags_overview <- function(
    type = "GPP"  
) {
  
  # GPP ==================================================================================
  
  if(type == "GPP") {
    
    ## 1.1 HTML flags ==================================================================================
    
    # Create a new dataframe 'html_flags' from 'html_flags.df'
    # Select only the 'Country' and 'GPP_Variable_Name' columns and add a new column 'HTML_flag' with all values set to "Red"
    # Ensure the dataframe contains only unique rows
    
    html_flags <- nuts_html_flags.df %>%
      select(Country, NUTS, GPP_Variable_Name) %>%
      mutate("HTML_flag" = "Red") %>%
      mutate(NUTS = gsub(":.*", "", NUTS)) %>%
      distinct()
    
    # Initialize an empty dataframe 'df' with columns 'Country' and 'GPP_Variable_Name'
    
    df <- data.frame(matrix(nrow = 0, ncol = 3))
    colnames(df) <- c("NUTS", "Country", "GPP_Variable_Name")
    
    # Populate 'df' with combinations of unique countries from 'html_flags.df' and 'reportvarslist' which contain the variables from the report
    
    for (i in unique(master_data.df$nuts_id)) {
      cy<- master_data.df%>%
        filter(nuts_id==i)
      
      df <- rbind(df, tibble("NUTS" = rep(i, length(reportvarslist)),
                             "Country" = cy$country_name_ltn[[1]],
                             "GPP_Variable_Name" = reportvarslist))
    }
    
    # Join 'df' with 'html_flags' and remove duplicate rows
    df2 <- left_join(df, html_flags) %>% 
      distinct()
    
    # Replace all NA values in 'df2' with "Green"
    df2[is.na(df2)] <- "Green"
    
    ## 1.2 Outliers flags ==================================================================================
    
    # Join 'df2' with 'NUTS_outliers.df', renaming columns for consistency
    
    df3 <- left_join(df2, NUTS_outliers.df %>%
                       rename("NUTS"= "NUTS Region",
                         "GPP_Variable_Name" = "Question",
                              "NUTS_outliers_flag" = "Flag")%>%
                     select(NUTS, GPP_Variable_Name, NUTS_outliers_flag))
    
    df4<- left_join(df3, Question_outliers.df%>%
                     rename("NUTS"= "nuts_id",
                            "GPP_Variable_Name" = "Question",
                            "Question_outliers_flag" = "Flag")%>%
                     select(NUTS, GPP_Variable_Name, Question_outliers_flag))
    
    #df3$Outliers_flag <- gsub(" .*", "", df3$Outliers_flag)
    
    
    ## 1.3 Ranking flags ==================================================================================
    
    # TPS
    
    # Prepare 'ranking' dataframe by selecting and reshaping relevant columns from 'TPS_ranking_analysis.df'
    
    ranking <- TPS_ranking_analysis.df %>%
      ungroup() %>%
      select(country_name_ltn, question, flagged_questions, Type_Survey) %>%
      distinct() %>%
      pivot_wider(names_from = Type_Survey, values_from = flagged_questions, values_fn = list) %>%
      unnest(cols = everything()) %>%
      arrange(country_name_ltn, question)
    
    # Join 'df3' with 'ranking' and rename columns for consistency
    
    df5 <- left_join(df4, ranking %>%
                       rename("Country" = "country_name_ltn",
                              "GPP_Variable_Name" = "question",
                              "Population_ranking_flag" = "population",
                              "Expert_ranking_flag" = "expert"))
    
    
    ## 1.4 Theoretical framework ==================================================================================
    
    # Select relevant columns from 'master_data.df' and normalize the variables
    
    gpp <- master_data.df %>% 
      select(country_name_ltn, nuts_id, all_of(reportvarslist))
    
    # Normalize the variables
    normalized <- normalizingvars(gpp, reportvarslist)
    
    # Aggregate the normalized variables at the country level
    gppaggregate <- normalized %>%
      group_by(country_name_ltn, nuts_id) %>%
      summarise_at(reportvarslist, mean, na.rm= TRUE) %>%
      pivot_longer(cols = all_of(reportvarslist), names_to = "GPP_Variable_Name", values_to = "Score")%>%
      rename("Country" = "country_name_ltn",
             "NUTS" = "nuts_id")
    
    # Prepare 'subp' dataframe by selecting and joining relevant columns from 'metadata' and 'QRQ_description'
    # This step is necessary to join the theoretical framework with the scores and the analyses
    
    subp <- metadata %>%
      select(GPP_Variable_Name = DAU_GPP, Question, subpillar = sub_pillar) %>%
      arrange(subpillar) %>%
      left_join(QRQ_description %>% 
                  select(pillar, pillar_name, pillar_id, subpillar, subpillar_name),
                by = "subpillar") %>%
      distinct()
    
    ## 1.5 Join everything ==================================================================================
    
    # Join 'df4' with 'gppaggregate' and 'subp' dataframes
    
    df6 <- left_join(df4, gppaggregate)
    
    options <- c("Green", "Red", NA_character_)
    
    # Add a new column with a random selection of the options
    #set.seed(123) # Setting seed for reproducibility
    #df6$TPS_flag <- sample(options, size = nrow(df6), replace = TRUE)
    
    df6$TPS_flag <- rep(NA_character_, nrow(df6))
    ## 1.6 Flagging system ==================================================================================
    
    # In this step we create the flagging system according to the conditions we established
    # Please remember that the case_when is hierarchical that's means the initial condition is greater than the rest. The order matters.
    
    df7 <- left_join(df6, subp, relationship = "many-to-many", by = "GPP_Variable_Name") %>%
      mutate(
        Final_flag = 
          case_when(
            # If TPS is NA, NUTS outliers is Red, and Question Outliers is Red set 'Final_flag' to "Red"
            is.na(TPS_flag) & NUTS_outliers_flag == "Red" & Question_outliers_flag == "Red" ~ "Red",
            # If TPS is "Red", NUTS outliers is Green , and Question Outliers is Green set 'Final_flag' to "Green"
            TPS_flag == "Red" &  NUTS_outliers_flag == "Green" & Question_outliers_flag == "Green"~ "Green", 
            # If 'TPS' is "Green", set 'Final_flag' to "Green"
            TPS_flag == "Red" &  NUTS_outliers_flag == "Red" & Question_outliers_flag == "Green"~ "Red", 
            # If 'Population_ranking_flag' is "Red", set 'Final_flag' to "Red"
            TPS_flag == "Red" &  NUTS_outliers_flag == "Green" & Question_outliers_flag == "Red"~ "Red", 
            TPS_flag == "Green" &  NUTS_outliers_flag == "Green" & Question_outliers_flag == "Red"~ "Green", 
            TPS_flag == "Green" &  NUTS_outliers_flag == "Red" & Question_outliers_flag == "Green"~ "Green", 
            # If 'Population_ranking_flag' is "Green" set 'Final_flag' to "Green"
            NUTS_outliers_flag == "Red" & Question_outliers_flag == "Red" ~ "Red", 
            # If 'Expert_ranking_flag' is "Red", set 'Final_flag' to "Red"
            NUTS_outliers_flag == "Red" & Question_outliers_flag == "Green" & HTML_flag == "Red" ~ "Red",
            # If Expert_ranking_flag' is "Green", set 'Final_flag' to "Green"
            NUTS_outliers_flag == "Green" & Question_outliers_flag == "Red" & HTML_flag == "Red" ~ "Red",
            # For all other cases, set 'Final_flag' to "Green"
            TRUE ~ "Green"
          )
      )%>%
      select(Country, NUTS, GPP_Variable_Name , Score, HTML_flag , NUTS_outliers_flag , Question_outliers_flag , TPS_flag , Final_flag , everything())
    
    # Return the final dataframe 'df6'
    
    return(df7)
    
  } else {
    
    # QRQ ==================================================================================
    
    ## TPS flags ==================================================================================
    
    # Assign the dataframe EU_QRQ_country to df1
    df1 <- EU_QRQ_country
    
    # Define the '%!in%' operator to check for elements not in a vector
    "%!in%" <- compose("!", "%in%")
    
    # Join EU_QRQ_country with TPS_validation, filter out certain indicators, and calculate TPS flags
    df2 <- EU_QRQ_country %>%
      left_join(TPS_validation %>%
                  select(country_name_ltn, indicator, TPS_flagged_questions),
                by = c("country_name_ltn", "indicator")
      ) %>%
      filter(indicator %!in% c("p_1", "p_2", "p_3", "p_4", "p_5", "p_6", "p_7", "p_8")) %>%
      mutate(
        TPS_flagged_questions = str_replace_all(TPS_flagged_questions, " Flag", ""),
        country_flags_TPS = if_else(TPS_flagged_questions == "Red", 1, 0)
      ) %>%
      group_by(country_name_ltn, indicator) %>%
      summarise(country_flags_TPS = sum(country_flags_TPS, na.rm = TRUE))
    
    ## ROLI flags ==================================================================================
    
    # Join df2 with ROLI_validation, calculate ROLI flags, and summarize total flags per country and indicator
    df3 <- df2 %>%
      left_join(ROLI_validation %>%
                  select(country_name_ltn, indicator, ROLI_flagged_questions),
                by = c("country_name_ltn", "indicator")
      ) %>%
      mutate(
        ROLI_flagged_questions = str_replace_all(ROLI_flagged_questions, " Flag", ""),
        ROLI_flagged_questions = if_else(ROLI_flagged_questions == "Red", 1, 0)
      ) %>%
      rowwise() %>%
      mutate(
        country_flags = sum(country_flags_TPS + ROLI_flagged_questions)
      ) %>%
      select(country_name_ltn, indicator, country_flags)
    
    ## LONGITUDINAL flags ==================================================================================
    
    # Prepare df4 by renaming columns and filtering indicators
    df4 <- eu_qrq_final %>%
      rename(QRQ_NUTS_value = QRQ_value,
             country_name_ltn = country) %>%
      filter(indicator %!in% c("p_1", "p_2", "p_3", "p_4", "p_5", "p_6", "p_7", "p_8")) %>%
      left_join(df3, by = c("country_name_ltn", "indicator")) %>%
      rename(country_code_nuts = nuts)
    
    ## Flagging system ==================================================================================
    
    # Join df4 with LONG_validation, calculate LONG flags, and summarize total flags per country
    df5 <- df4 %>%
      left_join(LONG_validation %>%
                  select(country_name_ltn, country_code_nuts, indicator, LONG_flagged_questions),
                by = c("country_name_ltn", "country_code_nuts", "indicator")
      ) %>%
      mutate(
        LONG_flagged_questions = str_replace_all(LONG_flagged_questions, " Flag", ""),
        nuts_flags = if_else(
          LONG_flagged_questions == "Green", 0, 1
        )
      ) %>%
      select(!LONG_flagged_questions) %>%
      rowwise() %>%
      mutate(
        total_flags = sum(country_flags, nuts_flags, na.rm = TRUE)
      )%>% 
      left_join(QRQ_description)
    
    
    
    return(df5)
    
  }
  
}
