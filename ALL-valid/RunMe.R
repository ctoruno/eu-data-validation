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

# This file contains the TPS data base which comes from TPS folder

TPS.df <- read_csv(paste0(path2eu,
                          "/EU-S Data/eu-data-validation/CD-valid/Input/TPS_data.csv")) 

# This file contains the match between the TPS and our data

metadata <- read_excel(paste0(path2eu, 
                             "/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatps.xlsx"))

# This  file contains the variables to be tested over time

variable_list.df <- read_excel(paste0(path2eu,
                                      "/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatt.xlsx"))

### QRQ ======================================================================================================

#### QRQ scores data ======================================================================================================

eu_qrq_final <- read_dta("Inputs/eu_qrq_final.dta") %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "QRQ_value")

EU_QRQ_country <- eu_qrq_final %>%
  group_by(country, indicator) %>%
  summarise(QRQ_value = mean(QRQ_value, na.rm = T)) %>%
  rename(country_name_ltn = country)

#### QRQ TPS scores ======================================================================================================

# These are the TPS scores that we will compare with the QRQ scores

QRQ_TPS <- read_excel("Inputs/QRQ_TPS.xlsx") %>%
  pivot_longer(cols = !c(country_name_ltn, country_code_nuts), 
               names_to = "Variable", 
               values_to = "value")

# These are the matches between the TPS and the QRQ scores

QRQ_Matches_TPS <- read_excel("Inputs/QRQ Matches TPS.xlsx") %>%
  drop_na(Variable)

# We merge both databases to know which score belong to each QRQ score

QRQ_TPS_MATCH.df <- QRQ_TPS %>%
  left_join(QRQ_Matches_TPS, by = "Variable", relationship = "many-to-many") %>%
  rename(TPS_variable = Variable,
         TPS_value = value)
# These data describe each indicator, to get a more complete data base we will merge this database with the QRQ_TPS

QRQ_description <- read_excel("Inputs/QRQ_description.xlsx")

QRQ_final_TPS <- QRQ_TPS_MATCH.df %>%
  left_join(QRQ_description, by = "indicator")

QRQ_TPS_final <- QRQ_final_TPS %>%
  left_join(EU_QRQ_country, by = c("indicator","country_name_ltn")) %>%
  select(country_name_ltn, country_code_nuts, indicator, subpillar_name, QRQ_value, TPS_variable, TPS_value)

#### QRQ longidutinal scores ======================================================================================================

eu_qrq_final_LONG <- read_dta("Inputs/eu_qrq_final_LONG.dta") %>%
  pivot_longer(cols = !c(nuts, country), 
               names_to = "indicator", values_to = "long_QRQ_value") 

QRQ_LONG_final <- eu_qrq_final_LONG %>%
  left_join(eu_qrq_final, by = c("indicator","country", "nuts")) %>%
  rename(country_name_ltn = country,
         country_code_nuts = nuts)
  
#### QRQ ROLI scores ======================================================================================================

eu_qrq_roli <- read_dta("Inputs/eu_qrq_final_country.dta") %>%
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

# Ranking analysis  

TPS_ranking_analysis.df <- TPS_ranking_analysis.fn(gpp_data.df = master_data.df,
                                                   tps_data.df = TPS.df,
                                                   metadata.df = metadata)

# Outliers analysis

outlier_analysis.df <- outlier_analysis(gpp_data.df = master_data.df)

### QRQ ======================================================================================================

TPS_validation <- QRQ_ranking.fn(data = QRQ_TPS_final, analysis = "TPS")

LONG_validation <- QRQ_ranking.fn(data = QRQ_LONG_final, analysis = "LONG")

ROLI_validation <-  QRQ_ranking.fn(data = QRQ_ROLI_final, analysis = "ROLI")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Implementing flagging system                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Implementing the flagging system that allow us to pick the most problematic variables ===========================

flagging_system.df <- flags_overview()

