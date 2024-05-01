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
## This version:      April 17th, 2024
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

args[1] = "Netherlands"
args[2] = "Carlos Toruño"
# List of chosen analyses (add/remove as needed)
# Mode options are "pretest" "html" "full" "update"
# "pretest" creates the analyses and HTML report for the pretest data, "html" creates the analyses and full fieldwork HTML report, "full" runs the ranking and outliers analysis and full fieldwork flagging system, and "update" runs all of the full fieldwork HTML reports to update them.
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
source("Code/TPS_nuts.R")
source("Code/time_changes_nuts.R")


if (args[3] == "update"){
  
  reports2update<- c("Bulgaria", "Cyprus", "Hungary", "Luxembourg", "Slovakia")
  authors<- c("Allison Bostrom", "Carlos Toruño", "Natalia Rodriguez", "Carlos Toruño", "Allison Bostrom")
  
} else {
  reports2update<- c(args[1])
  authors<- c(args[2])
  
}

for (i in 1:length(reports2update)){
  
  if (args[3] == "pretest"){
    master_data.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/1. PTR/", 
                                             reports2update[[i]],
                                             "/1. Clean Data", 
                                             "/",
                                             reports2update[[i]], "_clean.dta"))
  } else if (args[3] == "html" | args[3] == "update") {
    
    master_data.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/2. FFW/", 
                                             reports2update[[i]],
                                             "/1. Clean Data", 
                                             "/",
                                             reports2update[[i]], "_clean.dta"))
  } else {
    
    fullmerge <- read_dta(paste0(path2eu, "/EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta"))
  }
  
  GPP_previous.df <- haven::read_dta(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/eu_merge.dta")) 
  
  TPS.df <- read_csv(paste0(path2eu,"/EU-S Data/eu-data-validation/CD-valid/Input/TPS_data.csv"))
  
  data_map.df <- read_excel(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/EU2 GPP 2023 Full Datamap.xlsx"), sheet = "Data Map")
  codebook.df <- read_excel(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/EU2 GPP 2023 Codebook.xlsx")) %>%
    left_join(data_map.df %>% select(Variable, Scale), by = c("2023  EU Questionnaire" = "Variable"))
  framework <- read_excel(paste0(path2eu,"/EU-S Research/Design/EU Subnational_Questions_Map.xlsx"), sheet = "GPP and CF Map")
  
  matched_tps <- suppressMessages(import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/Selected GPP&TPS for QCC.xlsx")))
  matched_tps <- matched_tps$`Selection and matching`
  variable_list.df <- read_excel(paste0(path2eu,"/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatt.xlsx"))
  
  metadata<- read_excel(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/Metadatatps.xlsx"))
  metareport<- read_excel(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/metareport.xlsx"))
  
  sampling_plans.df <- read_excel(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Input/Sampling_plan_integrated.xlsx"))
  
  reportvars<- codebook.df %>%
    filter(Report == 1)
  
  reportvarslist<- reportvars$Variable
  
  
  
  
  
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
                                    country = reports2update[[i]],
                                    type = args[3])
    
    tps_comparisson.df <- TPS_function(country = reports2update[[i]],
                                       gpp     = master_data.df,
                                       tps     = TPS.df,
                                       mat     = metadata,
                                       type    = args[3]
    )
  }
  if (args[3] == "html"){
    
    time_nuts.df<- time_changes_nuts(data.df = master_data.df,
                                     country = reports2update[[i]],
                                     type = args[3])
    
    tps_nuts.df <- TPS_function_nuts(country = reports2update[[i]],
                                     gpp     = master_data.df,
                                     tps     = TPS.df,
                                     mat     = metadata,
                                     type    = args[3])
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
  
  if(args[3] == "pretest") {
    
    analysis_functions <- list(
      time_changes = time_changes.df,
      tps_comparisson = tps_comparisson.df
    )
    
  } else if (args[3] == "html"){
    
    analysis_functions <- list(
      time_changes = time_changes.df,
      tps_comparisson = tps_comparisson.df,
      time_nuts = time_nuts.df,
      tps_nuts = tps_nuts.df
      
    )
    
  }else {
    
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
    if (!dir.exists(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", reports2update[[i]]))){
      dir.create(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", reports2update[[i]]))
    }
    
    openxlsx::write.xlsx(analysis.list,
                         paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/",
                                reports2update[[i]],
                                "/",
                                reports2update[[i]],
                                ".xlsx"))
  } else if (args[3] == "html"){
    
    if (!dir.exists(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", reports2update[[i]]))){
      dir.create(file.path(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", reports2update[[i]]))
    }
    
    openxlsx::write.xlsx(analysis.list,
                         paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/",
                                reports2update[[i]],
                                "/",
                                reports2update[[i]],
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
                      params = list(country = reports2update[[i]], author = authors[[i]], date= Sys.Date()),
                      output_file=paste0(reports2update[[i]], " Validation Report", ".html"),
                      output_dir = paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", reports2update[[i]]))
    
  } else if (args[3] == "html" | args[3] == "update"){
    
    rmarkdown::render("./Code/Full Fieldwork Report Template.Rmd", 
                      params = list(country = reports2update[[i]], author = authors[[i]], date= Sys.Date()),
                      output_file=paste0(reports2update[[i]], " Full Fieldwork Validation Report", ".html"),
                      output_dir = paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", reports2update[[i]]))
    
  } 
}

