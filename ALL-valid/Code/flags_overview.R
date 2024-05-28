flags_overview <- function() {
  
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
  
  for (i in unique(html_flags.df$Country)) {
    df <- rbind(df, tibble("Country" = rep(i, length(reportvarslist)),
                           "GPP_Variable_Name" = reportvarslist))
  }
  
  # Join 'df' with 'html_flags' and remove duplicate rows
  df2 <- left_join(df, html_flags) %>% 
    distinct()
  
  # Replace all NA values in 'df2' with "Green"
  df2[is.na(df2)] <- "Green"
  
  ## 1.2 Outliers flags ==================================================================================
  
  # Join 'df2' with 'outlier_analysis.df', renaming columns for consistency
  # Remove trailing text from 'Outliers_flag' values
  df3 <- left_join(df2, outlier_analysis.df %>%
                     rename("GPP_Variable_Name" = "Question",
                            "Outliers_flag" = "Flag"))
  
  df3$Outliers_flag <- gsub(" .*", "", df3$Outliers_flag)
  
  ## 1.3 Ranking flags ==================================================================================
  
  # Prepare 'ranking' dataframe by selecting and reshaping relevant columns from 'TPS_ranking_analysis.df'

  ranking <- TPS_ranking_analysis.df %>%
    ungroup() %>%
    select(country_name_ltn, question, flagged_questions, Type_Survey) %>%
    distinct() %>%
    pivot_wider(names_from = Type_Survey, values_from = flagged_questions, values_fn = list) %>%
    unnest(cols = everything()) %>%
    arrange(country_name_ltn, question)
  
  # Join 'df3' with 'ranking' and rename columns for consistency
  
  df4 <- left_join(df3, ranking %>%
                     rename("Country" = "country_name_ltn",
                            "GPP_Variable_Name" = "question",
                            "Population_ranking_flag" = "population",
                            "Expert_ranking_flag" = "expert"))
  
  # Remove trailing text from 'Population_ranking_flag' and 'Expert_ranking_flag' values
  
  df4$Population_ranking_flag <- gsub(" .*", "", df4$Population_ranking_flag)
  df4$Expert_ranking_flag <- gsub(" .*", "", df4$Expert_ranking_flag)
  
  ## 1.4 Theorethical framework ==================================================================================
  
  # Select relevant columns from 'master_data.df' and normalize the variables
  
  gpp <- master_data.df %>% 
    select(country_name_ltn, all_of(reportvarslist))
  
  # Normalize the variables
  normalized <- normalizingvars(gpp, reportvarslist)
  
  # Aggregate the normalized variables at the country level
  gppaggregate <- normalized %>%
    group_by(country_name_ltn) %>%
    summarise_at(reportvarslist, mean, na.rm = TRUE) %>%
    pivot_longer(cols = all_of(reportvarslist), names_to = "GPP_Variable_Name", values_to = "Score") %>%
    rename("Country" = "country_name_ltn")
  
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

  df5 <- left_join(df4, gppaggregate)
  
  ## 1.6 Flagging system ==================================================================================
  
  # In this step we create the flagging system according to the conditions we established
  # Please remember that the case_when is hierarchical that's means the initial condition is greater than the rest. The order matters.
  
  df6 <- left_join(df5, subp, relationship = "many-to-many", by = "GPP_Variable_Name") %>%
    select(!Outliers_flag) %>%
    mutate(
      Final_flag = 
        case_when(
          # If 'HTML_flag', 'Population_ranking_flag', and 'Expert_ranking_flag' are all NA, set 'Final_flag' to "Red"
          is.na(HTML_flag) & is.na(Population_ranking_flag) & is.na(Expert_ranking_flag) ~ "Red",
          # If either 'Population_ranking_flag' or 'Expert_ranking_flag' is "Red", set 'Final_flag' to "Red"
          Population_ranking_flag == "Red" | Expert_ranking_flag == "Red" ~ "Red",
          # If either 'Population_ranking_flag' or 'Expert_ranking_flag' is "Green", set 'Final_flag' to "Green"
          Population_ranking_flag == "Green" | Expert_ranking_flag == "Green" ~ "Green",
          # If 'HTML_flag' is "Red", set 'Final_flag' to "Red"
          HTML_flag == "Red" ~ "Red",
          # For all other cases, set 'Final_flag' to "Green"
          TRUE ~ "Green"
        )
    )
  
  # Return the final dataframe 'df6'
  
  return(df6)
}
