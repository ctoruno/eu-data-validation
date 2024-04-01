## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Country data validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (Dhabiby@worldjusticeproject.org)
##                    Carlos A. ToruC1o Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 25th, 2023
##
## This version:      January 17th, 2024
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

#!/usr/bin/env Rscript 
args = commandArgs(trailingOnly=TRUE)

args[1] = "Hungary"
args[2] = "Natalia Rodriguez"
# List of chosen analyses (add/remove as needed)
# Options are "pretest" "html" or "full"
args[3] <- "html"


source("Code/settings.R")
source("Code/sociodem.R")
source("Code/time_changes.R")
source("Code/TPS.R")
source("Code/ranking.R")
source("Code/missing_values.R")
source("Code/time_length.R")
source("Code/representativeness.R")
source("Code/difficulty_score.R")
source("Code/outlier_analysis.R")
source("Code/flagging_system.R")



if (args[3] == "pretest"){
master_data.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/1. PTR/", 
                                         args[1],
                                         "/1. Clean Data", 
                                         "/",
                                         args[1], "_clean.dta"))
} else {
  
  master_data.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/2. FFW/", 
                                           args[1],
                                           "/1. Clean Data", 
                                           "/",
                                           args[1], "_clean.dta"))
}

GPP_previous.df <- haven::read_dta(paste0("Input/eu_merge.dta")) 

TPS.df <- read_csv("Input/TPS_data.csv")

data_map.df <- read_excel("Input/EU2 GPP 2023 Full Datamap.xlsx", sheet = "Data Map")
codebook.df <- read_excel("Input/EU2 GPP 2023 Codebook.xlsx") %>%
  left_join(data_map.df %>% select(Variable, Scale), by = c("2023  EU Questionnaire" = "Variable"))
framework <- read_excel(paste0(path2eu,"/EU-S Research/Design/EU Subnational_Questions_Map.xlsx"), sheet = "GPP and CF Map")

matched_tps <- suppressMessages(import_list("Input/Selected GPP&TPS for QCC.xlsx"))
matched_tps <- matched_tps$`Selection and matching`
variable_list.df <- read_excel("Input/Metadatatt.xlsx")

metadata<- read_excel("Input/Metadatatps.xlsx")
metareport<- read_excel("Input/metareport.xlsx")

if (args[3] == "pretest"){
  sampling_plans.df <- read_excel("Input/Sampling_plan_integrated.xlsx") %>%
    filter(country %in% args[1])
} else if (args[3] == "html"){
  sampling_plans.df <- read_excel("Input/Sampling_plan_integrated.xlsx")
} else if (args[3] == "full"){
  sampling_plans.df <- read_excel("Input/Sampling_plan_integrated.xlsx")
  fullmerge<- read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta"))
  
  reportvars<- codebook.df %>%
    filter(Report == 1)
  
  reportvarslist<- reportvars$Variable
}




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Defining analysis Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####################################################
# Correspondence patterns
# 
# 1. time_changes = t-test function
# 2. tps_comparisson = TPS function
# 3. tps_trend_comparisson = TPS trends function
#
####################################################


# Define analysis functions
if (args[3] != "full"){

time_changes.df <- time_changes(data.df = master_data.df,
                                country = args[1],
                                type = args[3])

tps_comparisson.df <- TPS_function(country = args[1],
                                   gpp     = master_data.df,
                                   tps     = TPS.df,
                                   mat     = metadata,
                                   type    = args[3]
                                   )
}
if (args[3] == "full"){

TPS_ranking_analysis.df <- TPS_ranking_analysis.fn(gpp_data.df = fullmerge,
                                                   tps_data.df = TPS.df,
                                                   metadata.df = metadata)

time_length.df<- time_length(fullf = fullmerge)

missing_values.df<- missing_values(data= fullmerge)

representativeness.df <- representativeness(data = fullmerge,
                                            sampling_plan_data = sampling_plans.df)
  
difficulty_score.df<- difficulty_score(data.df = fullmerge)

outlier_analysis.df<- outlier_analysis(gpp_data.df = fullmerge)

flagging_system.df<- flagging_system(gpp_data.df = fullmerge)

}
# List of analysis functions

if(args[3] != "full") {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df
    #sociodem_comparisson = sociodem_comparisson.df
    )
  
} else {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df,
    tps_trend_comparisson = tps_trend_comparisson.df,
    sociodem_comparisson = sociodem_comparisson.df,
    TPS_ranking_analysis = TPS_ranking_analysis.df,
    Outlier_analysis = outlier_analysis.df,
  )
  
}

analysis.list <- analysis_functions


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (args[3] == "pretest"){
  if (!dir.exists(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", args[1]))){
    dir.create(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", args[1]))
  }
  
  openxlsx::write.xlsx(analysis.list,
                       paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/",
                              args[1],
                              "/",
                              args[1],
                              ".xlsx"))
} else if (args[3] == "html"){
  
  if (!dir.exists(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", args[1]))){
    dir.create(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", args[1]))
  }
  
  openxlsx::write.xlsx(analysis.list,
                       paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/",
                              args[1],
                              "/",
                              args[1],
                              ".xlsx"))
} else if (args[3] == "full"){
  
  
 
  openxlsx::write.xlsx(flagging_system.df,
                       paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/",
                              "flagging_system.xlsx"))
  
  openxlsx::write.xlsx(TPS_ranking_analysis.df,
                       paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/Luxembourg/",
                              "ranking.xlsx"))

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Outcomes function                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


if (args[3] == "pretest"){
  rmarkdown::render("./Code/Country Report Template.Rmd", 
                    params = list(country = args[1], author = args[2], date= Sys.Date()),
                    output_file=paste0(args[1], " Validation Report", ".html"),
                    output_dir = paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", args[1]))
  
} else if (args[3] == "html"){
  
  rmarkdown::render("./Code/Full Fieldwork Report Template.Rmd", 
                    params = list(country = args[1], author = args[2], date= Sys.Date()),
                    output_file=paste0(args[1], " Full Fieldwork Validation Report", ".html"),
                    output_dir = paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", args[1]))

}
