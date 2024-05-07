## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation -  Flagging System
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 5th, 2024
##
## This version:      March 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
flagging_system <- function(gpp_data.df = master_data.df) {
  
  ## 1.1 Internal Flags  =====================================================================================
  
  # Initialize an empty data frame to store internal flags
  over_time <- data.frame()
  
  # Iterate over unique country names in the master_data.df data frame
  for (i in unique(master_data.df$country_name_ltn)) {
    
    # Filter data specific to the current country
    df <- master_data.df %>%
      filter(country_name_ltn == i)
    
    # Analyze time changes for specific GPP variables for the current country
    time_changes.df <- time_changes(data.df = df,
                                    gppvars = reportvarslist,
                                    country = i, 
                                    type = "full")
    
    # Append time changes data to the over_time data frame
    over_time <- rbind(over_time, time_changes.df)
  }
  
  # Select relevant columns and rename them for internal flags
  internal <- over_time %>%
    select(country, variable, warning) %>%
    rename("time" = warning)
  
  # Handle missing values in the time column
  internal$time <- ifelse(internal$time == "Not enough info", NA_character_, internal$time)
  
  # Perform a left join with outlier analysis data
  internal <- left_join(outlier_analysis.df %>%
                          rename("outliers" = Flag), 
                        internal, 
                        by = join_by("Country" == "country", "Question" == "variable"))
  
  # Handle missing values in the outliers column
  internal$outliers <- ifelse(internal$outliers == "Not Enough Info", NA_character_, internal$outliers)
  
  
  ## 1.2 External Flags  =====================================================================================
  
  # Retrieve information about flagged questions and scores
  ranking <- TPS_ranking_analysis.df %>%
    ungroup() %>%
    select(country_name_ltn, question, flagged_questions, flagged_score, Type_Survey) %>%
    distinct() %>%
    pivot_wider(names_from = Type_Survey, values_from = c(flagged_questions, flagged_score), values_fn = list) %>%
    unnest(cols = everything()) %>%
    select(-flagged_score_expert)
  
  # Join internal flags data with ranking data
  hm <- left_join(internal, ranking, by = join_by("Country" == "country_name_ltn", "Question" == "question")) %>%
    distinct() %>%
    rename("p_ranking" = flagged_questions_population,
           "threshold" = flagged_score_population,
           "e_ranking" = flagged_questions_expert)
  
  # Clean unnecessary characters from certain columns
  hm[, c("outliers", "time", "p_ranking", "e_ranking", "threshold")] <- lapply(hm[, c("outliers", "time", "p_ranking", "e_ranking", "threshold")], gsub, pattern = "\\s.*", replacement = "")
  
  ## Determine Final Flags ===================================================================================
  
  # Determine final flags based on internal and external validations
  final_flags <- hm %>%
    mutate("External_validation" = ifelse(is.na(p_ranking) & is.na(e_ranking) & is.na(threshold), NA_character_,
                                          ifelse(is.na(p_ranking) & is.na(threshold) & e_ranking %in% c("Green", "Red"), paste0(e_ranking, "*"),
                                                 ifelse(is.na(p_ranking) & is.na(threshold) & e_ranking == "Yellow", "Green*",
                                                        ifelse(p_ranking == "Red", "Red", 
                                                               ifelse(e_ranking == "Red" & threshold == "Red", "Red", 
                                                                      ifelse(p_ranking == "Yellow" & e_ranking == "Yellow" & threshold == "Yellow", "Red", "Green")))))),
           
           "Internal_validation" = ifelse(is.na(outliers) & is.na(time), NA_character_,
                                          ifelse(is.na(outliers), paste0(time, "*"),
                                                 ifelse(is.na(time), paste0(outliers, "*"),
                                                        ifelse(outliers == "Red" & time == "Red", "Red", 
                                                               ifelse(outliers == "Green" & time == "Green", "Green",
                                                                      ifelse(outliers == "Green" & time == "Red", "Red",
                                                                             ifelse(outliers == "Red" & time == "Green", "Red", NA_character_))))))),
           
           "Final_flag" = ifelse(is.na(External_validation), "Yellow",
                                 ifelse(is.na(Internal_validation), "Yellow",
                                        ifelse(External_validation == "Red" & Internal_validation == "Red", "Red", 
                                               ifelse(External_validation == "Red" & Internal_validation == "Green", "Yellow",
                                                      ifelse(External_validation == "Green" & Internal_validation == "Red", "Yellow",
                                                             ifelse(External_validation == "Green" & Internal_validation == "Green", "Green", 
                                                                    ifelse(grepl("Red", External_validation) & Internal_validation == "Red", "Red**",
                                                                           ifelse(grepl("Red", External_validation) & Internal_validation == "Green", "Yellow**",
                                                                                  ifelse(grepl("Green", External_validation) & Internal_validation == "Red", "Yellow**",
                                                                                         ifelse(grepl("Green", External_validation) & Internal_validation == "Green", "Green**", 
                                                                                                ifelse(grepl("Red", Internal_validation) & External_validation == "Red", "Red**",
                                                                                                       ifelse(grepl("Red", Internal_validation) & External_validation == "Green", "Yellow**",
                                                                                                              ifelse(grepl("Green", Internal_validation) & External_validation == "Red", "Yellow**",
                                                                                                                     ifelse(grepl("Green", Internal_validation) & External_validation == "Green", "Green**",
                                                                                                                            ifelse(grepl("Red", Internal_validation) & grepl("Red", External_validation), "Red***",
                                                                                                                                   ifelse(grepl("Red", Internal_validation) & grepl("Green", External_validation), "Yellow***",
                                                                                                                                          ifelse(grepl("Green", Internal_validation) & grepl("Red", External_validation), "Yellow***",
                                                                                                                                                 ifelse(grepl("Green", Internal_validation) & grepl("Green", External_validation), "Green***", NA_character_))))))))))))))))))
) %>%
  distinct()

## Additional Data Handling  ===============================================================================

# Retrieve information from codebook.df for flagged variables
info <- codebook.df %>%
  filter(Variable %in% reportvarslist) %>%
  select(Variable, `2023  EU Questionnaire`, Scale, Direction, Description)

# Update scale information for a specific row
info[76, 3] <- "Scale 2"

# Select relevant columns from master_data.df data frame
reportdf <- master_data.df %>%
  select(country_name_ltn, all_of(reportvarslist))
oriented <- reportdf

# Iterate over report variables for data orientation and processing
for (i in reportvarslist) {
  
  # Replace special values with NA
  oriented[[i]] <- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
  
  # Filter information from codebook.df for the current variable
  info2 <- info %>%
    filter(Variable == i)
  max <- parse_number(info2$Scale[1])
  
  # Adjust variable values based on direction and scale
  if (info2$Direction[1] == "Inverse" | info2$Direction[1] == "Proportional (after Transformation)") {
    
    if (max == 4) {
      oriented[[i]] <- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                            ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
    } else if (max == 5) {
      
      oriented[[i]] <- ifelse(oriented[[i]] == 1, 5, ifelse(oriented[[i]] == 2, 4, 
                                                            ifelse(oriented[[i]] == 4, 2, ifelse(oriented[[i]] == 5, 1, NA_real_))))
    } else if (max == 2) {
      oriented[[i]] <- ifelse(oriented[[i]] == 1, 2, ifelse(oriented[[i]] == 2, 1, 
                                                            NA_real_))
    }
  }
  
}

# Retrieve maximum values for each variable
cols_oriented <- names(oriented)[2:length(oriented)]
max_values <- lapply(cols_oriented, function(col_name) {
  
  codebook.df %>% 
    filter(Variable %in% col_name) %>%
    mutate(max_value = 
             case_when(
               Scale == "Scale 2" ~ 2,
               Scale == "Scale 3" ~ 3,
               Scale == "Scale 4" ~ 4,
               Scale == "Scale 5" ~ 5
             )) %>%
    pull(max_value)
})

# Add rows for maximum and minimum values
oriented[nrow(oriented) + 1,] <- c("maxs", max_values)
oriented[nrow(oriented) + 1,] <- c("mins", rep(list(1), ncol(oriented)-1))

# Normalize data using range method
process <- preProcess(oriented, method = c("range"))
normalized <- predict(process, oriented)

# Remove additional rows added for max and min values
normalized <- slice(normalized, 1:(n() - 2))

## Aggregation and Joins  ==================================================================================

# Aggregate data and calculate mean scores for each variable by country
gppaggregate <- normalized %>%
  group_by(country_name_ltn) %>%
  summarise_at(reportvarslist, mean, na.rm = TRUE) %>%
  pivot_longer(cols = all_of(reportvarslist), names_to = "Variable", values_to = "Score")

# Join aggregated data with additional information
add <- left_join(gppaggregate, info %>% select(Variable, `2023  EU Questionnaire`, Description)) %>%
  rename("EU_Name" = `2023  EU Questionnaire`)

# Join with metadata information
subp <- metadata %>%
  select(GPP_Variable_Name, Sub_Pillar, Pillar) %>%
  rbind(variable_list.df %>%
          select(variable, subpillar, pillar) %>%
          rename("GPP_Variable_Name" = "variable", 
                 "Sub_Pillar" = "subpillar",
                 "Pillar" = "pillar")) %>%
  distinct()

# Final join to incorporate all information
add2 <- left_join(add, subp, by = join_by("Variable" == "GPP_Variable_Name"), relationship = "many-to-many")

# Final join to incorporate all flagging information
full <- left_join(final_flags, add2, by = join_by("Country" == "country_name_ltn", "Question" == "Variable"), relationship = "many-to-many")

# Return the final data frame
return(full)
}
