representativeness <- function(data = fullmerge,
                                 sampling_plan_data = sampling_plans.df) {
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Arrange the data to merge                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  sociodem_sampling_plan.df <- sampling_plan_data%>%
    mutate(rural = as.numeric(rural),
           urban = as.numeric(urban)) %>% 
    group_by(country)%>%
    summarise(across(!c(country_code, NUTS_code), sum))%>%
    mutate(across(!country, ~.x/sample_size))
  
  gpp.df <- data %>%
    mutate(age_category = 
             case_when(
               age > 17 & age < 25 ~ "age_18-24",
               age > 24 & age < 35 ~ "age_25-34",
               age > 34 & age < 45 ~ "age_35-44",
               age > 44 & age < 55 ~ "age_45-54",
               age > 54 & age < 65 ~ "age_55-64",
               age > 65 ~ "age_65"
             ),
           gender = 
             case_when(
               gend == 1 ~ "gender_female",
               gend == 2 ~ "gender_male"
             ),
           sample_size = n(),
           counter = 1) %>%
    select(country_name_ltn, age_category, gender, sample_size, counter) 
  
  
  sociodem_sampling_plan.df <- sampling_plan_data %>%  
    group_by(country_code) %>%
    mutate(rural = as.numeric(rural),
           urban = as.numeric(urban)) %>%
    summarise(
      across(!c(country, NUTS_code), sum)) %>%
    mutate(
      across(!country_code, ~.x/sample_size)
    ) %>%
    select(!starts_with("quintile")) %>%
    select(!c(rural, urban, sample_size)) %>%
    mutate(dataframe = "sampling_plan")
  
  gpp.df <- data %>%
    mutate(age_category = 
             case_when(
               age > 17 & age < 25 ~ "age_18-24",
               age > 24 & age < 35 ~ "age_25-34",
               age > 34 & age < 45 ~ "age_35-44",
               age > 44 & age < 55 ~ "age_45-54",
               age > 54 & age < 65 ~ "age_55-64",
               age > 65 ~ "age_65"
             ),
           gender = 
             case_when(
               gend == 1 ~ "gender_female",
               gend == 2 ~ "gender_male"
             )) %>%
    group_by(country_name_ltn, age_category, gender)%>%
    summarise(sample_size = n())%>%
    select(country_name_ltn, age_category, gender, sample_size) 
  
  gend<- gpp.df%>%
    group_by(country_name_ltn, gender)%>%
    summarise(gend_sample = sum(sample_size))
  
  age<- gpp.df%>%
    group_by(country_name_ltn, age_category)%>%
    summarise(age_sample = sum(sample_size))
#########################################################################
  # Define a list of grouping variables
  group_vars <- c("gender", "age_category")
  
  # Apply the same operations to each grouping variable
  gpp_list <- lapply(group_vars, function(var) {
    gpp.df %>%
      group_by(!!sym(var)) %>%
      summarise(!!paste0(var, "_sum") := sum(counter/sample_size)) %>%
      drop_na() %>%
      pivot_wider(names_from = var, values_from = paste0(var, "_sum"))
  })
  
  # Combine the results into one data frame
  sociodem_gpp.df <- bind_cols(gpp_list) %>%
    mutate(country_code = country_ind,
           dataframe = "gpp")
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 2. Threshold analysis                                                          ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  final_result <- bind_rows(sociodem_gpp.df, sociodem_sampling_plan.df) %>%
    pivot_longer(cols = !c(country_code, dataframe), names_to = "category", values_to = "values") %>%
    pivot_wider(id_cols = c(country_code, category), names_from = c("dataframe"), values_from = "values") %>%
    mutate(difference = abs(sampling_plan - gpp),
           warning = if_else(difference > 0.05, "Yellow light", 
                             if_else(difference > 0.05 & difference > 0.1, "Yellow light", "Green light"))) %>%
    select(country_code, category, sampling_plan, gpp, difference, warning)
  
  
  return(final_result)
  
}
