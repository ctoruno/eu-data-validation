library(rio)
library(tidyverse)

index<- readxl::read_xlsx(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/EUS_GPP Report_Outline_240110.xlsx"), sheet = "All questions")

tps<- data.frame()
gpptest<- data.frame()

for (i in c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", 
            "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
            "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")){
  
  df<- import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", 
                          i, 
                          "/", 
                          i, 
                          ".xlsx"))
  comp<- df$tps_comparisson
  comp2<- df$time_changes

  tps<- rbind(tps, comp)
  gpptest<- rbind(gpptest, comp2)
  
}

tps$Flag<- ifelse(is.na(tps$Flag), "NA", tps$Flag )
tps$redflag<- ifelse(tps$Flag== "red", TRUE, FALSE)

tps%>%
  distinct(GPP_Variable_Name)

gpptest$redflag<- ifelse(gpptest$warning == "Red light", TRUE, FALSE)

tps2<- tps%>%
  select(Country, TPS_Variable_Name, GPP_Variable_Name, Type_Survey, Flag, TPS_Year,TPS_Source, redflag, Sub_Pillar)%>%
  group_by(Country, TPS_Variable_Name, Type_Survey, GPP_Variable_Name, Sub_Pillar)%>%
  mutate(sum = sum(redflag))%>%
  distinct(Country, TPS_Variable_Name, GPP_Variable_Name, sum, Sub_Pillar, .keep_all = TRUE)%>%
  pivot_wider(names_from = Type_Survey, values_from = sum)%>%
  select(-c(Flag, redflag))

tps3<- tps2%>%
  group_by(Country, TPS_Variable_Name) %>%
  mutate(GPP_Variable_Name = paste(unique(GPP_Variable_Name), collapse = ", "))%>%
  mutate(population = sum(population), expert = sum(expert))%>%
  distinct()

tps3$popred<- ifelse(tps3$population > 0, TRUE, FALSE)
tps3$expred<- ifelse(tps3$expert > 0, TRUE, FALSE)
  
final<- tps3%>%
  group_by(TPS_Variable_Name, GPP_Variable_Name, TPS_Year, TPS_Source, Sub_Pillar)%>%
  summarise(population = sum(popred, na.rm = TRUE), expert = sum(expred, na.rm = TRUE))%>%
  arrange(TPS_Variable_Name)

final$Type_Survey <- ifelse(final$TPS_Source == "Freedom in the World" |final$TPS_Source == "Varieties of Democracy", "expert", "population")
final<- final%>%
  mutate(Countries_Flagged = population + expert)%>%
  select(TPS_Variable_Name, GPP_Variable_Name, Countries_Flagged, Type_Survey, TPS_Year, TPS_Source, Sub_Pillar)


tps_final<- final%>%
  group_by(GPP_Variable_Name)%>%
  mutate(Sub_Pillar = paste(unique(Sub_Pillar), collapse = ", "))%>%
  distinct()%>%
  arrange(Sub_Pillar)

final2<- tps3%>%
  group_by(Country)%>%
  summarise(population = round(100* sum(popred, na.rm = TRUE)/sum(!is.na(population)), 2), expert = 100* sum(expred, na.rm = TRUE)/sum(!is.na(expert)))

final2[nrow(final2) +1,] <- list("Mean", round(mean(final2$population),2), round(mean(final2$expert),2))
final2<- final2%>%
  mutate(population = paste0(population, "%"), expert= paste0(expert, "%"))


####################
test<- tps3%>%
  group_by(Country)%>%
  summarise(population = sum(popred, na.rm = TRUE), expert = sum(expred, na.rm = TRUE))%>%
  mutate(totals = population + expert)

tpspillar<- tps%>%
  select(Pillar, redflag)
gpppillar<- gpptest%>%
  select(pillar, redflag)

colnames(gpppillar)<- c("Pillar", "redflag")

pillars<- rbind(tpspillar, gpppillar)

redf<- pillars%>%
  group_by(Pillar)%>%
  summarise(prop = sum(redflag)/n())

tpspillar<- tps%>%
  distinct(Country, TPS_Variable_Name, GPP_Variable_Name, Difference, .keep_all = TRUE)%>%
  select(Country, Pillar, redflag)
gpppillar<- gpptest%>%
  select(country, pillar, redflag)
colnames(gpppillar)<- c("Country", "Pillar", "redflag")
pillars<- rbind(tpspillar, gpppillar)

redfcountry<- pillars%>%
  group_by(Country)%>%
  summarise(prop = sum(redflag))

#########################

sub1<- metadata%>%
  select(GPP_Variable_Name, Pillar, Sub_Pillar)

sub2<- variable_list.df%>%
  select(variable, pillar, subpillar)

colnames(sub1)<- c("variable", "pillar", "subpillar")

subpillarcounts<- rbind(sub1, sub2)
subpillarcounts$subpillar<- gsub(":.*", "", subpillarcounts$subpillar)
subpillarcounts<- subpillarcounts%>%
  distinct()%>%
  group_by(subpillar)%>%
  summarise(counts = n())%>%
  print(n=35)

index%>%
  select(`Match 1`, `Match 2`, `Match 3`)

matches<- c(index$`Match 1`, index$`Match 2`, index$`Match 3`)
length(matches)
matches<- matches[!is.na(matches)]

subpillartotals<-tibble(matches)%>%
  mutate(matches = case_when(
    matches == "1.1000000000000001" ~ "1.10",
    matches == "1.1100000000000001" ~ "1.11",
    matches == "1.1200000000000001" ~ "1.12",
    matches == "4.4000000000000004" ~ "4.4",
    matches == "4.5999999999999996" ~ "4.6",
    matches == "5.0999999999999996" ~ "5.1",
    matches == "8.1999999999999993" ~ "8.2",
    matches == "8.3000000000000007" ~ "8.3",
    matches == "8.6999999999999993" ~ "8.7",
    matches == "1.2" ~ "1.02",
    matches == "1.3" ~ "1.03",
    matches == "1.4" ~ "1.04",
    matches == "1.5" ~ "1.05",
    matches == "1.6" ~ "1.06",
    matches == "1.7" ~ "1.07",
    matches == "1.8" ~ "1.08",
    matches == "1.9" ~ "1.09",
    .default = matches
  ))%>%
  group_by(matches)%>%
  summarize(totals = n())%>%
  head(34)

counts<- left_join(subpillartotals, subpillarcounts, by = join_by("matches" == "subpillar"))
counts$counts<- ifelse(is.na(counts$counts), 0, counts$counts)
counts[nrow(counts) +1,]<- list("Total", sum(counts$totals), sum(counts$counts))
counts<- counts%>%
  mutate(percentage = 100* counts/totals)%>%
  select(-totals)
colnames(counts)<- c("Subpillar", "Number of Indicators Analyzed", "Percentage of Indicators Analyzed")

missingsubp<- c("1.01", "2.2", "2.3", "2.5", "4.1", "6.1", "6.2", "6.3", "8.4")

missings<- tibble("Subpillar" = missingsubp, "Number of Indicators Analyzed" = rep(0, length(missingsubp)), "Percentage of Indicators Analyzed"= rep(0, length(missingsubp)))

counts1<- rbind(counts, missings)%>%
  arrange(Subpillar)


######### Pillar level ######

pillarcounts<- rbind(sub1, sub2)
pillarcounts$pillar<- gsub(":.*", "", pillarcounts$pillar)
pillarcounts<- pillarcounts%>%
  distinct()%>%
  group_by(pillar)%>%
  summarise(counts = n())

matches2<- gsub("\\..*", "", matches)

pillartotals<-tibble(matches2)%>%
  group_by(matches2)%>%
  summarize(totals = n())

counts2<- head(left_join(pillartotals, pillarcounts, by = join_by("matches2" == "pillar")),7)
counts2$counts<- ifelse(is.na(counts2$counts), 0, counts2$counts)
counts2[nrow(counts2) +1,]<- list("Total", sum(counts2$totals), sum(counts2$counts))
counts2<- counts2%>%
  mutate(percentage = 100* counts/totals)%>%
  select(-totals)

colnames(counts2)<- c("Pillar", "Number of Indicators Analyzed", "Percentage of Indicators Analyzed")




########## Santi's table

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

reportvars<- codebook.df %>%
  filter(Report == 1)

reportvarslist<- reportvars$Variable

tps4<- tps2%>%
  filter(GPP_Variable_Name %in% reportvarslist)%>%
  group_by(Country, TPS_Variable_Name) %>%
  mutate(GPP_Variable_Name = paste(GPP_Variable_Name, collapse = ", "))%>%
  mutate(population = sum(population), expert = sum(expert))%>%
  distinct()

tps4$popred<- ifelse(tps4$population > 0, TRUE, FALSE)
tps4$expred<- ifelse(tps4$expert > 0, TRUE, FALSE)

tps_vars_redflag<- tps4%>%
  group_by(Country)%>%
  summarise(population = sum(popred, na.rm = TRUE), expert = sum(expred, na.rm = TRUE))%>%
  mutate(Sum = population+expert)

gpp_vars_redflag<- gpptest%>%
  filter(variable %in% reportvarslist)%>%
  group_by(country)%>%
  summarise(overtime = sum(redflag))

total_vars_redflag<- left_join(tps_vars_redflag, gpp_vars_redflag, by = join_by("Country"== "country"))%>%
  mutate(Total = Sum+overtime)%>%
  mutate(prop_vars_report_redflag = Total/length(reportvarslist))%>%
  select(Country, prop_vars_report_redflag)
  

tableCountry <- tibble(country = countries,
                       total_vars = total_vars,
                       total_vars_prop = total_ratios,
                       total_vars_wo_a2j_prop = total_ratios_wa2j,
                       total_vars_report = total_ratios_report)

tableCountry2<- left_join(tableCountry, total_vars_redflag, by = join_by("country" == "Country"))
#write_xlsx(tableCountry, path = "Outcomes/Pretest/overviewCountry.xlsx")

#### NUTS TPS #####

euprojnuts<- readxl::read_xlsx(paste0(path2eu, "/EU Subnational GPP/Requests for Proposals/Appendices/Appendix 2_Selected NUTS Regions and Sample Sizes.xlsx"), sheet = "Selected Regions Overview")

nutscodes<- euprojnuts$...3
nutscodes<- nutscodes[-c(1:5)]

EWC <- read_dta(file.path(path2SP, 
                            "8. Data/TPS/European Working Conditions/EWC_raw.dta",
                            fsep = "/")) 

EWC_nuts<- names(labelled::get_value_labels(EWC$NUTS_1_CODE_2021))
openxlsx::write.xlsx(tibble("Region" = EWC_nuts),
                     paste0(path2eu, "/EU-S Data/",
                            "EWC_nuts_matching.xlsx"))


ESS <- read_dta(file.path(path2SP, 
                            "8. Data/TPS/ESS/ESS_raw.dta",
                            fsep = "/")) 
ESS$region

GCB <- read_dta(file.path(path2SP, 
                            "8. Data/TPS/Global Corruption Barometer/GCB_raw.dta",
                            fsep = "/")) 
SPE_489<- read_dta(file.path(path2SP, 
                             "8. Data/TPS/Eurobarometer/SPE_489_raw.dta",
                             fsep = "/")) 
SPE_489$nuts


ESS$nuts<- ifelse(ESS$regunit==2, gsub(".$", "", ESS$region), ifelse(ESS$regunit == 3,
                                                    gsub(".$", "", ESS$region), ESS$region))

ESS%>%
  filter(nuts %in% nutscodes)



########## NUTS level report #####

reportvarslist

TPSnuts<-metadata%>%
  filter(`NUTS Level`== 1)
length(unique(TPSnuts$GPP_Variable_Name))/length(reportvarslist)

###### complete and incomplete analyses #####

cases1<- unique(gpptest$variable)
cases2<- unique(tps$GPP_Variable_Name)

tps_only<- tps%>%
  filter(GPP_Variable_Name %in% setdiff(cases2, cases1))

cases3<- unique(filter(tps_only, Type_Survey == "population")$GPP_Variable_Name)
cases4<- unique(filter(tps_only, Type_Survey == "expert")$GPP_Variable_Name)

completecases<- tibble("Complete Analyses"= length(intersect(cases1, cases2)), "Only in GPP Analysis"= length(setdiff(cases1, cases2)), "Only in TPS Analysis" = length(setdiff(cases2, cases1)), "Only in Population Polls"= length(setdiff(cases3, cases4)), "Only in Expert Encodings" = length(setdiff(cases4, cases3)))

tables<- list(Country_Overview = tableCountry2, Pillar_indicators = counts2, Subpillar_indicators = counts1, Pillars_proportion_Red = redf, Country_proportion_red = final2, TPS_level = tps_final, Complete_cases=completecases)
openxlsx::write.xlsx(tables,
                     paste0(path2eu, "/EU-S Data/",
                            "tables.xlsx"))

nrow(index%>%
  filter(`Match 1` != "A2J"))
