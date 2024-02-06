representativeness <- function(data = fullmerge,
                                 sampling_plan_data = sampling_plans.df) {
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Arrange the data to merge                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # sociodem_sampling_plan.df <- sampling_plan_data%>%
  #   mutate(rural = as.numeric(rural),
  #          urban = as.numeric(urban)) %>% 
  #   group_by(country)%>%
  #   summarise(across(!c(country_code, NUTS_code), sum))%>%
  #   mutate(across(!country, ~.x/sample_size))
  # gpp.df <- data %>%
  #   mutate(age_category = 
  #            case_when(
  #              age > 17 & age < 25 ~ "age_18-24",
  #              age > 24 & age < 35 ~ "age_25-34",
  #              age > 34 & age < 45 ~ "age_35-44",
  #              age > 44 & age < 55 ~ "age_45-54",
  #              age > 54 & age < 65 ~ "age_55-64",
  #              age > 65 ~ "age_65"
  #            ),
  #          gender = 
  #            case_when(
  #              gend == 1 ~ "gender_female",
  #              gend == 2 ~ "gender_male"
  #            ),
  #          sample_size = n(),
  #          counter = 1) %>%
  #   select(country_name_ltn, age_category, gender, sample_size, counter) 
  # 
  ### COUNTRY LEVEL ###
  
  sociodem_sampling_plan.df <- sampling_plan_data %>%  
    group_by(country) %>%
    mutate(rural = as.numeric(rural),
           urban = as.numeric(urban)) %>%
    summarise(
      across(!c(country_code, NUTS_code), sum)) %>%
    mutate(
      across(!country, ~.x/sample_size)
    ) %>%
    select(!starts_with("quintile")) %>%
    select(!c(rural, urban, sample_size))%>%
    pivot_longer(cols=!country, names_to = "category", values_to = "ideal_prop")
  
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
    summarise(gend_sample = sum(sample_size))%>%
    group_by(country_name_ltn)%>%
    mutate(total = sum(gend_sample))%>%
    ungroup()%>%
    mutate(prop = gend_sample/total)%>%
    select(country_name_ltn, gender, prop)%>%
    rename(category = gender)
  
  age<- gpp.df%>%
    group_by(country_name_ltn, age_category)%>%
    summarise(age_sample = sum(sample_size))%>%
    group_by(country_name_ltn)%>%
    mutate(total = sum(age_sample))%>%
    ungroup()%>%
    mutate(prop = age_sample/total)%>%
    select(country_name_ltn, age_category, prop)%>%
    rename(category = age_category)
  
  samples<- rbind(gend, age)
  
  country<- left_join(sociodem_sampling_plan.df, samples, by = c("country"= "country_name_ltn", "category" = "category"))%>%
    mutate("difference"= ideal_prop-prop)
  colnames(country)<- c("Country", "Category", "Ideal Proportion", "Actual Proportion", "Diffrerence")
  
  ### NUTS LEVEL ###
  
  sociodem_sampling_plan.df <- sampling_plan_data %>%  
    group_by(NUTS_code) %>%
    mutate(rural = as.numeric(rural),
           urban = as.numeric(urban)) %>%
    summarise(
      across(!c(country_code, country), sum)) %>%
    mutate(
      across(!NUTS_code, ~.x/sample_size)
    ) %>%
    select(!starts_with("quintile")) %>%
    select(!c(rural, urban, sample_size))%>%
    pivot_longer(cols=!NUTS_code, names_to = "category", values_to = "ideal_prop")
  
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
    group_by(nuts_id, age_category, gender)%>%
    summarise(sample_size = n())%>%
    select(nuts_id, age_category, gender, sample_size) 
  
  gend<- gpp.df%>%
    group_by(nuts_id, gender)%>%
    summarise(gend_sample = sum(sample_size))%>%
    group_by(nuts_id)%>%
    mutate(total = sum(gend_sample))%>%
    ungroup()%>%
    mutate(prop = gend_sample/total)%>%
    select(nuts_id, gender, prop)%>%
    rename(category = gender)
  
  age<- gpp.df%>%
    group_by(nuts_id, age_category)%>%
    summarise(age_sample = sum(sample_size))%>%
    group_by(nuts_id)%>%
    mutate(total = sum(age_sample))%>%
    ungroup()%>%
    mutate(prop = age_sample/total)%>%
    select(nuts_id, age_category, prop)%>%
    rename(category = age_category)
  
  samples<- rbind(gend, age)
  
  nuts<- left_join(sociodem_sampling_plan.df, samples, by = c("NUTS_code"= "nuts_id", "category" = "category"))%>%
    mutate("difference"= ideal_prop-prop)
  colnames(country)<- c("Country", "Category", "Ideal Proportion", "Actual Proportion", "Diffrerence")

  return(list("country" = country, "nuts" = nuts))
  
}
