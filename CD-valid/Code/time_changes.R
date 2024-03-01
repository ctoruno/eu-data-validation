time_changes <- function(data.df = master_data.df, 
                         country = args[1],
                         type = "pretest") {
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Arrange the data to merge                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
  list_var_t.test <- codebook.df %>%
    filter(Variable %in% gpp_vars) %>%
    mutate(skip = if_else(str_detect(`Global GPP`, "EU_"), 1, 
                          if_else(Variable %in% c("CPA_protest", "PAB_misinfo",
                                                  "PAB_attackmedia","PAB_emergpower",
                                                  "PAB_manipulelect", "PAB_overcourts",
                                                  "CJP_resprights", "BRB_permit_B"), 1, 0))) %>%
    filter(skip == 0) %>%
    select(Variable) %>%
    pull()
  
  missings <- GPP_previous.df %>%
    filter(country_name_ltn %in% country) %>%
    select(country_name_ltn, year, all_of(list_var_t.test)) %>% 
    pivot_longer(cols = !c(country_name_ltn, year), names_to = "variable", values_to = "values") %>%
    left_join(variable_list.df, by = c("variable" = "variable")) %>%
    drop_na() %>%
    distinct(variable) %>%
    pull()

  GPP.df <- GPP_previous.df %>%
    filter(!is.na(country_name_ltn)) %>%
    select(country_name_ltn, year, all_of(list_var_t.test)) %>%
    filter(country_name_ltn %in% country) %>%
    select(country_name_ltn, year, all_of(list_var_t.test))
  
  GPP.df$BRB_health_B<- ifelse(GPP.df$BRB_health_B== 0, 2, GPP.df$BRB_health_B)
    
  data_subset.df <- data.df %>%
    select(country_name_ltn, year, all_of(list_var_t.test))
  
  data2test <-data_subset.df %>% 
      bind_rows(GPP.df) 
    
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 2.  Standardize and normalize all variables                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  data2test2<- data2test%>%
    select(-year)
  normalized<- normalizingvars(data2test2, list_var_t.test)
  normalized$year<- data2test$year
  
  normalized<- normalized%>%
    mutate(year = as.character(year))
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 3. T-test analysis                                                          ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if (country == "Ireland"){
    
    list_var_t.test<- setdiff(list_var_t.test, c("IRE_govtbudget","IRE_govtcontracts","IRE_disclosure"))
  }
  
  t_test_results <- lapply(list_var_t.test, function(var_name) {
    
    # Subset data for the current variable
    
    data_sub <- normalized %>%
      select(country_name_ltn, year, {{var_name}}) %>%
      arrange(year)
    
    # country_code <- data_sub %>%
    #   select(country_code) %>%
    #   unique() %>%
    #   pull()
    
    # Get the most recent year and calculate the previous year
    
    years <- unique(data_sub$year)
    current_year <- max(years)
    
    if(length(years) == 2) {
      
      previous_year <- min(years)
    } else {
      
      previous_year <- years[[2]]
    }

    # Subset data for the most recent and previous year
    
    recent_year_data <- data_sub %>% 
      filter(year == current_year)
    
    previous_year_data <- data_sub %>% 
      filter(year == previous_year)
    
    # Perform a t-test on the measurements for the two years
    
    current_point <- recent_year_data%>%
      rename(target = all_of({{var_name}})) %>%
      summarise(curr_point = mean(target, na.rm= TRUE))%>%
      pull()
    
    previous_point <- previous_year_data%>%
      rename(target = all_of({{var_name}})) %>%
      summarise(prev_point = mean(target, na.rm= TRUE))%>%
      pull()
    
    difference <- current_point - previous_point
    
    direction <- if_else(difference > 0, "Positive change", "Negative change")
    
    t_test_result <- t.test(x = recent_year_data[[var_name]], 
                            y = previous_year_data[[var_name]])
    
    if (type == "pretest"){
    return(tibble(
      country = country,
      variable = var_name,
      ttestResult = t_test_result$p.value,
      current_score = t_test_result$estimate[[1]],
      previous_score = t_test_result$estimate[[2]],
      warning = if_else(t_test_result$p.value < 0.01, "Red light", 
                        if_else(t_test_result$p.value > 0.01 & t_test_result$p.value < 0.1, "Yellow light", "Green light")),
      direction = direction,
      curr_year = current_year,
      prev_year = previous_year
    ))
   }
    if (type == "full"){
      return(tibble(
        country = country,
        variable = var_name,
        ttestResult = t_test_result$p.value,
        current_score = t_test_result$estimate[[1]],
        previous_score = t_test_result$estimate[[2]],
        warning = if_else(t_test_result$p.value < 0.01, "Red light", "Green light"),
        direction = direction,
        curr_year = current_year,
        prev_year = previous_year
      ))
    }
    
  })
  
  # Combine the results into a single tibble
  time_test.df <- bind_rows(t_test_results) %>%
    left_join(variable_list.df, by = c("variable" = "variable")) %>%
    arrange(pillar) 
  
  return(time_test.df)
}
  