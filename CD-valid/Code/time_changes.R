time_changes <- function(data = master_data.df, 
                         country_code = country_ind,
                         type = "dummy") {
  
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
                                                  "CJP_resprights"), 1, 0))) %>%
    filter(skip == 0) %>%
    select(Variable) %>%
    pull()
  
  missings <- GPP_previous.df %>%
    filter(country_code %in% country_code) %>%
    select(country_code, year, all_of(list_var_t.test)) %>% 
    pivot_longer(cols = !c(country_code, year), names_to = "variable", values_to = "values") %>%
    left_join(variable_list.df, by = c("variable" = "gppvars")) %>%
    drop_na() %>%
    distinct(variable) %>%
    pull()

  GPP.df <- GPP_previous.df %>%
    filter(!is.na(country_code)) %>%
    select(country_code, year, all_of(list_var_t.test)) %>%
    filter(country_code %in% country_ind) %>%
    select(country_code, year, all_of(list_var_t.test))
  
  if (type == "dummy"){
  
  data_subset.df <- data %>%
    mutate(country_code = 
             case_when(
               country_name_ltn == 1 ~ "CZE",
               country_name_ltn == 2 ~ "EST",
               country_name_ltn == 3 ~ "FIN",
               country_name_ltn == 4 ~ "FRA",
               country_name_ltn == 5 ~ "SVN",
               country_name_ltn == 6 ~ "ESP",
               country_name_ltn == 7 ~ "SWE"
             )) %>%
    select(country_code, year, all_of(list_var_t.test))
  
  data2test <- data_subset.df %>% 
    bind_rows(GPP.df) 
  } else if (type == "real"){
    
    data_subset.df <- data%>%
      select(country_code_nuts, year, all_of(list_var_t.test))%>%
      rename(country_code = country_code_nuts)
    
    data2test <-data_subset.df %>% 
      bind_rows(GPP.df) 
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 2.  Standardize and normalize all variables                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  oriented <- data2test
  for(i in list_var_t.test){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
    
  }
  
  ro<- c("JSE_indjudges", "CPA_freevote", "CPA_cleanelec_local", 
         "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", 
         "CPB_freexp", "TRT_police", "TRT_pparties", "CJP_proofburden", 
         "COR_judges", "COR_police", "LEP_indprosecutors","LEP_indpolinv",
         "CJP_fairtrial")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                         ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  

  oriented <- oriented %>%
      mutate(year = as.character(year))
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)

  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 3. T-test analysis                                                          ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  t_test_results <- lapply(list_var_t.test, function(var_name) {
    
    # Subset data for the current variable
    
    data_sub <- normalized %>%
      filter(country_code == country_ind) %>%
      select(country_code, year, {{var_name}}) %>%
      arrange(year)
    
    # country_code <- data_sub %>%
    #   select(country_code) %>%
    #   unique() %>%
    #   pull()
    
    # Get the most recent year and calculate the previous year
    
    years <- unique(data_sub$year)
    current_year <- max(years)
    previous_year <- years[[2]]
    
    # Subset data for the most recent and previous year
    
    recent_year_data <- data_sub %>% 
      filter(year == current_year)
    
    previous_year_data <- data_sub %>% 
      filter(year == previous_year)
    
    # Perform a t-test on the measurements for the two years
    
    t_test_result <- t.test(x = recent_year_data[[var_name]], 
                            y = previous_year_data[[var_name]])
    
    return(tibble(
      country = country_code,
      variable = var_name,
      ttestResult = t_test_result$p.value,
      current_score = t_test_result$estimate[[1]],
      previous_score = t_test_result$estimate[[2]],
      warning = if_else(t_test_result$p.value < 0.01, "Red light", 
                        if_else(t_test_result$p.value > 0.01 & t_test_result$p.value < 0.1, "Yellow light", "Green light")),
      curr_year = current_year,
      prev_year = previous_year
    ))
  })
  
  # Combine the results into a single tibble
  time_test.df <- bind_rows(t_test_results) %>%
    left_join(variable_list.df, by = c("variable" = "gppvars")) %>%
    arrange(pillar) 
  
  return(time_test.df)
}
  