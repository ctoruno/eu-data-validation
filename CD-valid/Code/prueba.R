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

countries.ls <- list()
sheets <- c("time_changes", "tps_comparisson")
lista_hojas <- list()

for (i in countries) {
  archivo <- paste0(outPath, i, "/", i, ".xlsx")
  hojas   <- excel_sheets(archivo)
  # Iterar sobre cada hoja y leer sus datos
  for (hoja in hojas) {
    # Leer los datos de la hoja actual
    datos_hoja <- read_excel(archivo, sheet = hoja)
    # Agregar los datos de la hoja a la lista
    lista_hojas[[paste0(i, "-", hoja)]] <- datos_hoja
  }
}

total <- Austria_TC %>% full_join(y = Austria_TPS, by = c("variable" = "GPP_Variable_Name")) %>% distinct(variable)

variables <- codebook.df %>%
  mutate(filtro = 
           if_else(str_detect(Variable, 
                              "TRT_|ATC_|COR_|ORC_|BRB_|IPR_|IRE_|SEC_|DIS_|AJD_|AJR_|AJE_|CPA_|CPB_|LEP_|CJP_|CTZ_|PAB_|JSE_|ROL_"), 
                   1, 0)
  ) %>%
  filter(filtro == 1)

"%!in%" <- compose("!", "%in%")

variables_wa2j <- variables %>% 
  filter(str_detect(Variable, 
                    "TRT_|ATC_|COR_|ORC_|BRB_|IPR_|IRE_|SEC_|DIS_|CPA_|CPB_|LEP_|CJP_|CTZ_|PAB_|JSE_|ROL_")) %>%
  filter(!str_detect(Variable, "DIS_exp"))
