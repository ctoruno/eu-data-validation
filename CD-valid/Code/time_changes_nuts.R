time_changes_nuts <- function(data.df = master_data.df, 
                         country = args[1],
                         gppvars = gpp_vars,
                         type = "pretest") {
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Arrange the data to merge                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  gppvars<- c(variable_list.df$variable, "BRB_permit_B")
  
  list_var_t.test <- codebook.df %>%
    filter(Variable %in% gppvars) %>%
    mutate(skip = if_else(str_detect(`Global GPP`, "EU_"), 1, 
                          if_else(Variable %in% c("CPA_protest", "PAB_misinfo",
                                                  "PAB_attackmedia","PAB_emergpower",
                                                  "PAB_manipulelect", "PAB_overcourts",
                                                  "CJP_resprights", "CP_cso", 
                                                  "CTZ_headgovteval_A", "CTZ_headgovteval_B"), 1, 0))) %>%
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
  
  GPP.df$BRB_permit_A<- ifelse(GPP.df$BRB_permit_A== 0, 2, GPP.df$BRB_permit_A)
  GPP.df$BRB_permit_B<- ifelse(GPP.df$BRB_permit_B== 0, 2, GPP.df$BRB_permit_B)
  GPP.df$BRB_benefits_A<- ifelse(GPP.df$BRB_benefits_A== 0, 2, GPP.df$BRB_benefits_A)
  GPP.df$BRB_benefits_B<- ifelse(GPP.df$BRB_benefits_B== 0, 2, GPP.df$BRB_benefits_B)
  GPP.df$BRB_id_A<- ifelse(GPP.df$BRB_id_A== 0, 2, GPP.df$BRB_id_A)
  GPP.df$BRB_id_B<- ifelse(GPP.df$BRB_id_B== 0, 2, GPP.df$BRB_id_B)
  GPP.df$BRB_school_A<- ifelse(GPP.df$BRB_school_A== 0, 2, GPP.df$BRB_school_A)
  GPP.df$BRB_school_B<- ifelse(GPP.df$BRB_school_B== 0, 2, GPP.df$BRB_school_B)
  GPP.df$BRB_health_A<- ifelse(GPP.df$BRB_health_A== 0, 2, GPP.df$BRB_health_A)
  GPP.df$BRB_health_B<- ifelse(GPP.df$BRB_health_B== 0, 2, GPP.df$BRB_health_B)
    
  data_subset.df <- data.df %>%
    select(country_name_ltn, nuts_ltn, nuts_id, year, all_of(list_var_t.test))
  
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
  
  final<- as.data.frame(matrix(nrow=0, ncol=7))
  colnames(final)<- c("Country", "NUTS", "variable", "current_score", "previous_score", "difference", "direction")
  
  for (i in c(1:length(list_var_t.test))){
    
    data_sub<- normalized%>%
      select(country_name_ltn, nuts_ltn, nuts_id, year, list_var_t.test[[i]]) %>%
      arrange(year)
    
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
    for (j in unique(recent_year_data$nuts_id)){
      
      nutsltn<- unique(recent_year_data%>%
        filter(nuts_id == j)%>%
        select(nuts_ltn))
      
      current_point <- recent_year_data%>%
        filter(nuts_id == j)%>%
        rename(target = all_of(list_var_t.test[[i]])) %>%
        summarise(curr_point = mean(target, na.rm= TRUE))%>%
        pull()
      
      previous_point <- previous_year_data%>%
        rename(target = all_of(list_var_t.test[[i]])) %>%
        summarise(prev_point = mean(target, na.rm= TRUE))%>%
        pull()
      
      difference <- current_point - previous_point
      
      direction <- if_else(difference > 0, "Positive change", "Negative change")
      
      recdat<- recent_year_data%>%
        filter(nuts_id == j)
      
      if (sum(!is.na(recdat[[list_var_t.test[[i]]]])) < 30 | sum(!is.na(previous_year_data[[list_var_t.test[[i]]]]))< 30){
        
        t_test_result<- 99
        
      }else{
        
        t_test_result <- t.test(x = recdat[[list_var_t.test[[i]]]], 
                                y = previous_year_data[[list_var_t.test[[i]]]])
      }
      
      if (class(t_test_result) != "htest"){
        
        f<- tibble(
          country = country,
          nuts = paste0(j, ":", nutsltn),
          variable = list_var_t.test[[i]],
          ttestResult = 99,
          current_score = current_point,
          previous_score = previous_point,
          warning = "Not enough info",
          direction = direction,
          curr_year = current_year,
          prev_year = previous_year
        )
        
      }else{
        
        f<- tibble(
          country = country,
          nuts = paste0(j, ":", nutsltn),
          variable = list_var_t.test[[i]],
          ttestResult = t_test_result$p.value,
          current_score = t_test_result$estimate[[1]],
          previous_score = t_test_result$estimate[[2]],
          warning = if_else(t_test_result$p.value < 0.01, "Red light", "Green light"),
          direction = direction,
          curr_year = current_year,
          prev_year = previous_year
        )
      }

      final<- rbind(final, f)
    }
  }
  
  fin<- final%>%
    left_join(variable_list.df, by = c("variable" = "variable")) %>%
    arrange(pillar) 
  return(fin)
}
  