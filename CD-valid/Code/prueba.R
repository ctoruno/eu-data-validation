data_map.df <- read_excel("Input/EU2 GPP 2023 Full Datamap.xlsx", 
                          sheet = "Data Map")
codebook.df <- read_excel("Input/EU2 GPP 2023 Codebook.xlsx") %>%
  left_join(data_map.df %>% select(Variable, Scale), 
            by = c("2023  EU Questionnaire" = "Variable"))

countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
               "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
               "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
               "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
               "Slovenia", "Spain", "Sweden")

outPath <- paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/")

variables_total <- codebook.df %>%
  mutate(filtro = 
           if_else(str_detect(Variable, 
                              "TRT_|ATC_|COR_|ORC_|BRB_|IPR_|IRE_|SEC_|DIS_|AJD_|AJR_|AJE_|CPA_|CPB_|LEP_|CJP_|CTZ_|PAB_|JSE_|ROL_"), 
                   1, 0)
  ) %>%
  filter(filtro == 1) %>%
  nrow()

variables_wa2j <- codebook.df %>% 
  filter(str_detect(Variable, 
                    "TRT_|ATC_|COR_|ORC_|BRB_|IPR_|IRE_|SEC_|DIS_|CPA_|CPB_|LEP_|CJP_|CTZ_|PAB_|JSE_|ROL_")) %>%
  filter(!str_detect(Variable, "DIS_exp")) %>%
  nrow()

variables_report <- codebook.df %>%
  filter(Report == 1) %>%
  nrow()

# Initialize a list to store the results for each country
variables_checked <- vector("list", length(countries))
integrated_data <- list()
# Iterate over each country
for (i in seq_along(countries)) {
  # Initialize an empty list to store datasets for each sheet
  country_data <- list()
  
  archivo <- paste0(outPath, countries[i], "/", countries[i], ".xlsx")
  hojas   <- excel_sheets(archivo)
  
  # Iterating over each sheet
  for (hoja in hojas) {
    # Leer los datos de la hoja actual
    datos_hoja <- read_excel(archivo, sheet = hoja)
    # Store the dataset in the list with the sheet name as key
    country_data[[hoja]] <- datos_hoja %>% 
      drop_na()
  }
  
  # Store the list of datasets for the current country in the integrated_data list
  integrated_data[[countries[i]]] <- country_data
  
  # Perform the operation for each country and store the result
  variables_checked[[i]] <- integrated_data[[countries[i]]]$time_changes %>%
    full_join(integrated_data[[countries[i]]]$tps_comparisson,
              by = c("variable" = "GPP_Variable_Name")) %>%
    distinct(variable) %>%
    nrow()
}

# Print the results
names(variables_checked) <- countries
variables_checked

# Calculate total ratio for each country and bind the results
total_vars <- sapply(countries, function(country) {
  cbind(variables_checked[[country]])
})

# Calculate total ratio for each country and bind the results
total_ratios <- sapply(countries, function(country) {
  cbind(variables_checked[[country]]) / variables_total
})

total_ratios_wa2j <- sapply(countries, function(country) {
  cbind(variables_checked[[country]]) / variables_wa2j
})

total_ratios_report <- sapply(countries, function(country) {
  cbind(variables_checked[[country]]) / variables_report
})

tableCountry <- tibble(country = countries,
                       total_vars = total_vars,
                       total_vars_prop = total_ratios,
                       total_vars_wo_a2j_prop = total_ratios_wa2j,
                       total_vars_report = total_ratios_report)
write_xlsx(tableCountry, path = "Outcomes/Pretest/overviewCountry.xlsx")
