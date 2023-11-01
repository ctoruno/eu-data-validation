sociodem_sampling_plan.df <- sampling_plans.df %>%  
  group_by(country_code) %>%
  mutate(rural = as.numeric(rural),
         urban = as.numeric(urban)) %>%
  summarise(
    across(!c(country, NUTS_code), sum)) %>%
  mutate(
    across(!country_code, ~.x/sample_size)
  ) %>%
  select(!starts_with("quintile")) %>%
  select(!c(rural, urban)) %>%
  mutate(dataframe = "sampling_plan")

sociodem_gpp.df <- master_data.df %>%
  mutate(age_category = 
           case_when(
             age > 17 & age < 25 ~ "age_18-24",
             age > 24 & age < 35 ~ "age_25-34",
             age > 34 & age < 45 ~ "age_34-44",
             age > 44 & age < 55 ~ "age_45-54",
             age > 54 & age < 65 ~ "age_55-64",
             age > 65 ~ "age_65"
             ),
         gender = 
           case_when(
             gend == 1 ~ "Female",
             gend == 2 ~ "Male"
           ),
         sample_size = n(),
         country_code = "CZE",
         counter = 1) %>%
  select(country_code, age_category, gender, sample_size, counter) 

# Define a list of grouping variables
group_vars <- c("gender", "age_category")

# Apply the same operations to each grouping variable
result_list <- lapply(group_vars, function(var) {
  sociodem_gpp.df %>%
    group_by(!!sym(var)) %>%
    summarise(!!paste0(var, "_sum") := sum(counter)) %>%
    drop_na() %>%
    pivot_wider(names_from = var, values_from = paste0(var, "_sum"))
})

# Combine the results into one data frame
final_result <- bind_cols(result_list) %>%
   mutate(country_code = country_ind,
          dataframe = "gpp")
  

sociodem_check <- function(data){
  
  EU_GPP <- data %>%
    mutate(
      generation               =  if_else(age < 30 & age > 17, "Less than 30 years", 
                                          if_else(age > 29, "More than 30 years", NA_character_)),
      economic_status          =  if_else(fin == 1 | fin == 2, "Poor",
                                          if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
      gender                   =  if_else(gend == 1, "Male", "Female", NA_character_),
      education                =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                          if_else(edu < 4, "No High Education Level", NA_character_))
    )
  
  data <- EU_GPP
  
  prueba <- imap(c(gender          = "gender",
                   generation      = "generation",
                   economic_status = "economic_status",
                   education       = 'education'),
                 
                 function(sociodem, category){
                   
                   data2table <- data %>%
                     rename(socio = all_of(sociodem))
                   
                   sociodem <- data2table %>%
                     mutate(counter = 1,
                            total = n()) %>%
                     select(socio, counter, total) %>%
                     group_by(socio) %>%
                     summarise(total = sum(counter/total)*100) %>%
                     pivot_wider(names_from = "socio", values_from = "total") %>%
                     rename(var1 = 1, var2 = 2) %>%
                     mutate(difference = abs(var1-var2)) %>%
                     mutate(flag = if_else(difference > 15 & difference < 25, "Big Difference",
                                           if_else(difference > 25, "Huge Difference", "No flag"))) %>%
                     mutate(category = category) %>%
                     select(category, var1, var2, difference, flag)
                   
                   return(sociodem)
                   
                 })
  
  final <- do.call(rbind.data.frame, prueba)
  
  
  return(final)
  
  
}

