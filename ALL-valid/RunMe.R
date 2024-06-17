## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Altogether data validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 7th, 2024
##
## This version:      May 7th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("Code/settings.R")

### GPP ======================================================================================================

# Representativeness
source("Code/missing_values.R")
source("Code/time_length.R")
source("Code/representativeness.R")
source("Code/difficulty_score.R")

#Validation
source("../CD-valid/Code/paragraph.R")
source("Code/ranking.R")
source("Code/outlier_analysis.R")
source("Code/flags_overview.R")
source("Code/html_flags.R")
source("Code/diff_rank.R")
source("Code/nuts_html_flags.R")
source("Code/nuts_flags_overview.R")
### QRQ ======================================================================================================

source("Code/qrq_ranking_analysis.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Data loading                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### GPP ======================================================================================================

#### Read Data Merge ======================================================================================================
master_data.df <- read_dta(paste0(path2eu, 
                                  "/EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta"))

#### Read Inputs: supplemental data =======================================================================================

# This is the file which contains all GPP variables
data_map.df <- read_excel(paste0(path2eu, 
                                 "/EU-S Data/eu-data-validation/CD-valid/Input/EU2 GPP 2023 Full Datamap.xlsx"), 
                          sheet = "Data Map")

# This is the codebook with all the information of the GPP vars, we join it with 
#the GPP variables and filter the variables from the report

codebook.df <- read_excel(paste0(path2eu, 
                                 "/EU-S Data/eu-data-validation/CD-valid/Input/EU2 GPP 2023 Codebook.xlsx")
                          ) %>%
  left_join(data_map.df %>% 
              select(Variable, Scale), 
            by = c("2023  EU Questionnaire" = "Variable"))

reportvars.df <- codebook.df %>%
  filter(Report == 1)

reportvarslist <- reportvars.df$Variable # The final list of variables from the report

# This file contains the weight distributions for each country
weight.df<- read_excel(paste0(path2eu, "/EU-S Data/reports/eu-gpp-report/data-viz/inputs/region_labels.xlsx"))

# This file contains the TPS data base which comes from TPS folder

TPS.df <- read_csv(paste0(path2eu,
                          "/EU-S Data/eu-data-validation/CD-valid/Input/TPS_data.csv")) 

# This file contains the latest previous GPP data

GPP_previous.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/eu_merge.dta")) 

# This file contains the match among the GPP and the thematical framework

metadata <- read_excel(paste0(path2eu, 
                             "/EU-S Data/eu-data-validation/CD-valid/Input/Master Map File_EU GPP.xlsx"), 
                       sheet = "Question Coding") %>%
  select(!`Horacio Question ID`, 
         EU_GPP = `EU GPP Full Question`,
         DAU_GPP = `DAU Code`, 
         Question = QUESTION_PART2) %>%
  select(!c(QUESTION_PART1, TOTAL_MATCHES, Infographics, Control, A2J)) %>%
  pivot_longer(cols = !c(EU_GPP, DAU_GPP, Question),
               names_to = "sub_pillar", values_to = "value2filter") %>%
  mutate(
    sub_pillar = 
      case_when(
        sub_pillar == "1.0900000000000001" ~ "1.09",
        sub_pillar == "1.1100000000000001" ~ "1.11",
        sub_pillar == "1.1200000000000001" ~ "1.12",
        sub_pillar == "2.2000000000000002" ~ "2.2",
        sub_pillar == "2.2999999999999998" ~ "2.3",
        sub_pillar == "4.0999999999999996" ~ "4.1",
        sub_pillar == "4.4000000000000004" ~ "4.4",
        sub_pillar == "4.5999999999999996" ~ "4.6",
        sub_pillar == "5.0999999999999996" ~ "5.1",
        sub_pillar == "8.1999999999999993" ~ "8.2",
        sub_pillar == "8.3000000000000007" ~ "8.3",
        sub_pillar == "8.6999999999999993" ~ "8.7",
        T ~ sub_pillar
      )
  ) %>%
  filter(value2filter == 1) %>%
  select(!value2filter)

# This file contains the match between the TPS and our data

metadataTPS <- read_excel(paste0(path2eu,
                                 "/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatps.xlsx"))

# This  file contains the variables to be tested over time

variable_list.df <- read_excel(paste0(path2eu,
                                      "/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatt.xlsx"))

### QRQ ======================================================================================================

#### QRQ scores data ======================================================================================================

##### QRQ s1 ======================================================================================================

eu_qrq_final_s1 <- read_dta(paste0(path2eu,
                                   "/EU-S Data/eu-qrq/1. Data/",
                                   "eu_qrq_nuts.dta")) %>%
  select(country, nuts, starts_with("p_")) %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "QRQ_value") %>%
  mutate(
    scenario = "scenario 1"
  )

EU_QRQ_country_s1 <- read_dta(paste0(path2eu,
                                     "/EU-S Data/eu-qrq/1. Data/",
                                     "eu_qrq_country.dta")) %>%
  select(country, starts_with("p_")) %>%
  pivot_longer(cols = !c(country), 
               names_to = "indicator", values_to = "QRQ_value") %>%
  rename(country_name_ltn = country) %>%
  mutate(
    scenario = "scenario 1"
  )

##### QRQ s2 ======================================================================================================

eu_qrq_final_s2 <- read_dta(paste0(path2eu,
                                   "/EU-S Data/eu-qrq/1. Data/",
                                   "eu_qrq_no_out1.dta")) %>%
  select(country, nuts, starts_with("p_")) %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "QRQ_value")  %>%
  mutate(
    scenario = "scenario 2"
  )

EU_QRQ_country_s2 <- read_dta(paste0(path2eu,
                                     "/EU-S Data/eu-qrq/1. Data/",
                                     "eu_qrq_country_no_out1.dta")) %>%
  select(country, starts_with("p_")) %>%
  pivot_longer(cols = !c(country), 
               names_to = "indicator", values_to = "QRQ_value") %>%
  rename(country_name_ltn = country) %>%
  mutate(
    scenario = "scenario 2"
  )

##### QRQ s3 ======================================================================================================

eu_qrq_final_s3 <- read_dta(paste0(path2eu,
                                   "/EU-S Data/eu-qrq/1. Data/",
                                   "eu_qrq_no_out2.dta")) %>%
  select(country, nuts, starts_with("p_")) %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "QRQ_value")  %>%
  mutate(
    scenario = "scenario 3"
  )

EU_QRQ_country_s3 <- read_dta(paste0(path2eu,
                                     "/EU-S Data/eu-qrq/1. Data/",
                                     "eu_qrq_country_no_out2.dta")) %>%
  select(country, starts_with("p_")) %>%
  pivot_longer(cols = !c(country), 
               names_to = "indicator", values_to = "QRQ_value") %>%
  rename(country_name_ltn = country) %>%
  mutate(
    scenario = "scenario 3"
  )

##### QRQ s4 ======================================================================================================

eu_qrq_final_s4 <- read_dta(paste0(path2eu,
                                   "/EU-S Data/eu-qrq/1. Data/",
                                   "eu_qrq_no_out3.dta")) %>%
  select(country, nuts, starts_with("p_")) %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "QRQ_value")  %>%
  mutate(
    scenario = "scenario 4"
  )

EU_QRQ_country_s4 <- read_dta(paste0(path2eu,
                                     "/EU-S Data/eu-qrq/1. Data/",
                                     "eu_qrq_country_no_out3.dta")) %>%
  select(country, starts_with("p_")) %>%
  pivot_longer(cols = !c(country), 
               names_to = "indicator", values_to = "QRQ_value") %>%
  rename(country_name_ltn = country) %>%
  mutate(
    scenario = "scenario 4"
  )

##### QRQ best score ======================================================================================================

eu_qrq_final_s5 <- readxl::read_xlsx(paste0(path2eu,
                                            "/EU-S Data/eu-data-validation/ALL-valid/Outputs/best_scenario_nuts.xlsx")
                                     ) %>%
  mutate(
    scenario = "best scenario"
  ) %>%
  select(!best_score) %>%
  select(!best_scenario)

EU_QRQ_country_s5 <- readxl::read_xlsx(paste0(path2eu,
                                              "/EU-S Data/eu-data-validation/ALL-valid/Outputs/best_scenario_country.xlsx")
) %>%
  mutate(
    scenario = "best scenario"
  ) %>%
  rename(
    country_name_ltn = country
  )

##### QRQ final ======================================================================================================

eu_qrq_final <- rbind(eu_qrq_final_s1, eu_qrq_final_s2, eu_qrq_final_s3, eu_qrq_final_s4, eu_qrq_final_s5 ) 

EU_QRQ_country <- rbind(EU_QRQ_country_s1, EU_QRQ_country_s2, EU_QRQ_country_s3, EU_QRQ_country_s4, EU_QRQ_country_s5) 
  
#### QRQ TPS scores ======================================================================================================

# These are the TPS scores that we will compare with the QRQ scores

QRQ_codebook<- import_list(paste0(path2eu,
                                 "/EU-S Data/eu-data-validation/ALL-valid/",
                                 "Inputs/EU QRQ Codebook.xlsx"))
QRQ_codebook<- rbind(QRQ_codebook$CJ, QRQ_codebook$CCA, QRQ_codebook$CCB, QRQ_codebook$GOV)

QRQ_TPS <- read_excel(paste0(path2eu,
                             "/EU-S Data/eu-data-validation/ALL-valid/",
                             "Inputs/QRQ_TPS.xlsx")) %>%
  pivot_longer(cols = !c(country_name_ltn, country_code_nuts), 
               names_to = "Variable", 
               values_to = "value")

# These are the matches between the TPS and the QRQ scores

QRQ_Matches_TPS <- read_excel(paste0(path2eu,
                                     "/EU-S Data/eu-data-validation/ALL-valid/",
                                     "Inputs/QRQ Matches TPS.xlsx")) %>%
  drop_na(Variable)

# We merge both databases to know which score belong to each QRQ score

QRQ_TPS_MATCH.df <- QRQ_TPS %>%
  left_join(QRQ_Matches_TPS, by = "Variable", relationship = "many-to-many") %>%
  rename(TPS_variable = Variable,
         TPS_value = value)
# These data describe each indicator, to get a more complete data base we will merge this database with the QRQ_TPS

QRQ_description <- read_excel(paste0(path2eu,
                                     "/EU-S Data/eu-data-validation/ALL-valid/",
                                     "Inputs/QRQ_description.xlsx"))

QRQ_final_TPS <- QRQ_TPS_MATCH.df %>%
  left_join(QRQ_description, by = "indicator")

QRQ_TPS_final <- QRQ_final_TPS %>%
  left_join(EU_QRQ_country, by = c("indicator","country_name_ltn")) %>%
  select(country_name_ltn, country_code_nuts, indicator, QRQ_value, TPS_variable, TPS_value, scenario)

#### QRQ longidutinal scores ======================================================================================================

eu_qrq_final_LONG <- read_dta(paste0(path2eu,
                                     "/EU-S Data/eu-qrq/1. Data/",
                                     "Benchmarks/eu_qrq_final_LONG.dta")) %>%
  select(country, nuts, starts_with("p_")) %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "long_QRQ_value") 

QRQ_LONG_final <- eu_qrq_final_LONG %>%
  left_join(eu_qrq_final, by = c("indicator","country", "nuts")) %>%
  rename(country_name_ltn = country,
         country_code_nuts = nuts)
  
#### QRQ ROLI scores ======================================================================================================

eu_qrq_roli <- read_dta(paste0(path2eu,
                               "/EU-S Data/eu-qrq/1. Data/",
                               "Benchmarks/eu_qrq_ROLI_new.dta")) %>%
  pivot_longer(cols = !country, 
               names_to = "indicator", values_to = "ROLI_QRQ_value") %>%
  rename(country_name_ltn = country)

QRQ_ROLI_final <- eu_qrq_roli %>%
  left_join(EU_QRQ_country, by = c("indicator","country_name_ltn")) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Defining analysis Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### GPP ======================================================================================================

#### Loading results from the analysis functions ===================================================================

# This function brings the flags from the HTMLs

html_flags.df <- html_flags()

# TPS Ranking analysis  

TPS_ranking_analysis.df <- ranking_analysis.fn(gpp_data.df = master_data.df,
                                               tps_data.df = TPS.df,
                                               metadata.df = metadataTPS, 
                                               analysis = "TPS")

# Internal Ranking analysis

INTERNAL_ranking_analysis.df <- ranking_analysis.fn(gpp_data.df = master_data.df,
                                                    tps_data.df = TPS.df,
                                                    metadata.df = metadataTPS, 
                                                    analysis = "INTERNAL")


#### Loading results from the NUTS analysis functions =============================================================

# This function brings the flags from the HTMLs

nuts_html_flags.df <- nuts_html_flags()


# Outliers analyses

NUTS_outliers.df <- outlier_analysis(gpp_data.df = master_data.df, type = "NUTS")

Question_outliers.df <- outlier_analysis(gpp_data.df = master_data.df, type = "question")

### QRQ ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = QRQ_TPS_final, 
                                 analysis = "TPS")

ROLI_validation <-  QRQ_ranking.fn(data = QRQ_ROLI_final, 
                                   analysis = "ROLI")

LONG_validation <- QRQ_ranking.fn(data = QRQ_LONG_final, 
                                  analysis = "LONG")

GPP_validation <- QRQ_ranking.fn(data = master_data.df, 
                                 analysis = "GPP")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Implementing flagging system                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### GPP ======================================================================================================

# Implementing the flagging system that allow us to pick the most problematic variables ===========================

GPP_flagging_system.df <- flags_overview(type = "GPP")


### QRQ ======================================================================================================

# Implementing the flagging system that allow us to pick the best scenario ===========================

QRQ_flagging_system.df <- flags_overview(type = "QRQ")

write_xlsx(QRQ_flagging_system.df, path = paste0(path2eu,
                                                 "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_flagging_system.xlsx")
           )
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Outcome Functions                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### GPP ======================================================================================================

openxlsx::write.xlsx(TPS_ranking_analysis.df, paste0(path2eu,
                                                    "/EU-S Data/eu-data-validation/ALL-valid/Outputs/GPP_external_ranking.xlsx"))

openxlsx::write.xlsx(INTERNAL_ranking_analysis.df, paste0(path2eu,
                                                     "/EU-S Data/eu-data-validation/ALL-valid/Outputs/GPP_internal_ranking.xlsx"))

openxlsx::write.xlsx(GPP_flagging_system.df, paste0(path2eu,
                                                    "/EU-S Data/eu-data-validation/ALL-valid/Outputs/GPP_flagging_system.xlsx"))

### Threshold outputs ========================================================================================

tpsthresh<- TPS_ranking_analysis.df%>%
  select(country_name_ltn, Type_Survey, upper_bound)%>%
  distinct()%>%
  pivot_wider(names_from = Type_Survey, values_from = upper_bound)

rankthresh<- left_join(tpsthresh, INTERNAL_ranking_analysis.df%>%
            select(country_name_ltn, upper_bound)%>%
            distinct()%>%
            rename(internal = upper_bound)
)%>%
  arrange(country_name_ltn)

openxlsx::write.xlsx(rankthresh, paste0(path2eu,
                                    "/EU-S Data/eu-data-validation/ALL-valid/Outputs/ranking_thresholds.xlsx"))

### NUTS GPP =================================================================================================


openxlsx::write.xlsx(NUTS_outliers.df, paste0(path2eu,
                                                     "/EU-S Data/eu-data-validation/ALL-valid/Outputs/GPP_NUTS_outliers.xlsx"))

openxlsx::write.xlsx(Question_outliers.df, paste0(path2eu,
                                                          "/EU-S Data/eu-data-validation/ALL-valid/Outputs/GPP_Question_outliers.xlsx"))

### QRQ ======================================================================================================

openxlsx::write.xlsx(TPS_validation, paste0(path2eu,
                                                    "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_external_ranking.xlsx"))

openxlsx::write.xlsx(ROLI_validation, paste0(path2eu,
                                            "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_internal_ranking.xlsx"))

openxlsx::write.xlsx(LONG_validation, paste0(path2eu,
                                             "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_longitudinal.xlsx"))

openxlsx::write.xlsx(QRQ_flagging_system.df, paste0(path2eu,
                                             "/EU-S Data/eu-data-validation/ALL-valid/Outputs/QRQ_flags.xlsx"))



