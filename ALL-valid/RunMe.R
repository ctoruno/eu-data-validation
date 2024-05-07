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

# Read Data Merge ======================================================================================================
master_data.df <- read_dta(paste0(path2eu, 
                                  "/EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta"))

# Read Inputs: supplemental data =======================================================================================

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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Defining analysis Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading results from the analysis functions ===================================================================

# This function brings the flags from the HTMLs

html_flags.df <- html_flags()

# Ranking analysis  

TPS_ranking_analysis.df <- TPS_ranking_analysis.fn(gpp_data.df = master_data.df,
                                                   tps_data.df = TPS.df,
                                                   metadata.df = metadata)

# Outliers analysis

outlier_analysis.df <- outlier_analysis(gpp_data.df = master_data.df)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Implementing flagging system                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Implementing the flagging system that allow us to pick the most problematic variables ===========================

flagging_system.df <- flags_overview()

