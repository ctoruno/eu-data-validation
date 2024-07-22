## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Pre-settings
##
## Author(s):         Carlos A. ToruC1o Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 3rd, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

p_load(char = c(
  
  # Data Loading
  "haven", "readxl", "writexl", "haven", "rio",
  
  # Utilities
  "caret", "readr", "DiagrammeR",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
)
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "ctoruno") {
  path2SP <- paste0("/Users/ctoruno/OneDrive - World Justice Project/Data Analytics")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics")
  
} else if (Sys.info()["user"]=="Dhabiby"){
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics")
  
} else if (Sys.info()["user"] == "nclapacs"){
  path2SP <- paste0("/Users/nclapacs/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "apillai") {
  path2SP <-"/Users/apillai/OneDrive - World Justice Project/Data Analytics/"

}else {
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
  
}


if (Sys.info()["user"] == "ctoruno") {
  path2eu <- paste0("/Users/ctoruno/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2eu <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"]=="Dhabiby"){
  path2eu<- paste0("/Users/Dhabiby/World Justice Project/Research - EU Subnational")
  
} else if (Sys.info()["user"] == "nclapacs"){
  path2eu <- paste0("/Users/nclapacs/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"]=="apillai"){
  path2eu<- paste0("/Users/apillai/OneDrive - World Justice Project/EU Subnational")
  
} else {
  path2eu <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - EU SUBNATIONAL DIRECTORY"
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Re-orienting, normalizing, aggregating                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

normalizingvars<- function(gppctry, 
                           gppvars){
  
  oriented <- gppctry
  
  for(i in gppvars){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
    
  }
  
  ro <- codebook.df %>%
    filter(Direction %in% c("Inverse", "Proportional (after Transformation)")) %>%
    filter(!str_detect(Variable, "DIS")) %>%
    filter(!str_detect(Variable, "BRB")) %>%
    filter(Scale %in% c("Scale 4", "Scale 5")) %>%
    pull(Variable)
  
  ro <- c(ro, 
          "CTZ_laborcond",
          "CTZ_gendereq",
          "CTZ_consrights",
          "CTZ_envprotect",
          "CTZ_euvalues",
          "CTZ_headgovteval",
          "CTZ_localgovteval")
  
  ro2 <- codebook.df %>%
    filter(Direction %in% c("Inverse", "Proportional (after Transformation)")) %>%
    filter(!str_detect(Variable, "DIS")) %>%
    filter(!str_detect(Variable, "BRB")) %>%
    filter(Scale %in% c("Scale 2")) %>%
    pull(Variable)
  
  gppro<- intersect(gppvars, ro)
  gppro2<- intersect(gppvars, ro2)
  
  for(i in gppro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                         ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  for(i in gppro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 2, ifelse(oriented[[i]] == 2, 1, NA_real_))
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  cols_oriented <- names(select_if(oriented, negate(is.character)))
  max_values <- lapply(cols_oriented, function(col_name){
    
    codebook.df %>% 
      filter(Variable %in% col_name) %>%
      mutate(max_value = 
               case_when(
                 Scale == "Scale 2" ~ 2,
                 Scale == "Scale 3" ~ 3,
                 Scale == "Scale 4" ~ 4,
                 Scale == "Scale 5" ~ 5
               )) %>%
      pull(max_value)
  })
  
  oriented[nrow(oriented) + 1,] <- c(rep("maxs", ncol(select_if(oriented, is.character))), max_values)
  oriented[nrow(oriented) + 1,] <- c(rep("mins", ncol(select_if(oriented, is.character))), rep(list(1), ncol(select_if(oriented, negate(is.character)))))
  
  
  process    <- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  normalized <- slice(normalized, 1:(n() - 2))
  
  return(normalized)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data list                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gpp_vars<-c("JSE_indjudges", "ROL_courtrulings_imp", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", 
           "CPA_cleanelec_local", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "PAB_emergpower",
           "PAB_overcourts", "PAB_manipulelect", "PAB_attackmedia", "PAB_misinfo", "ROL_corruption_imp", "LEP_bribesreq",
           "ORC_corimpact", "COR_3year_change", "BRB_health_B", "BRB_permit_B", "ROL_abusepower_imp", "ORC_pconnections",
           "IRE_campaign", "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "CPA_media_freeop",
           "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "CPA_protest", "CPA_cso", "TRT_parliament", "TRT_police",
           "TRT_pparties", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "ROL_equality_imp",
           "CTZ_laborcond_A", "CTZ_laborcond_A", "CPA_freevote", "JSE_equality", "ROL_constprotection_imp",
           "CJP_proofburden", "SEC_orgcrime", "JSE_rightsaware", "JSE_access2assis", "JSE_access2assis", "JSE_affordcosts",
           "JSE_quickresol", "JSE_indjudges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", 
           "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "COR_parliament", "COR_govt_national",
           "COR_govt_local", "COR_police", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CTZ_accountability",
           "CPB_freeassoc", "CPA_law_langaval", "CPB_unions", "CPB_freexp", "CPA_cleanelec_local", "CPB_community", "CPB_freemedia",
           "CPB_freexp_cso", "CPA_partdem_congress", "CPB_freexp_pp", "CPA_partdem_localgvt", "LEP_rightsresp", "LEP_accountability"
           )
countrylist<- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Iterarion function                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Best scenario function
best_scenario.fn <- function(data.df = QRQ_flagging_system.df) {
  
  best_scenario <- data.df %>%
    group_by(country_code_nuts, scenario) %>%
    mutate(final_score = sum(total_flags_iqr, na.rm = T)) %>%
    group_by(country_code_nuts) %>%
    mutate(best_score = min(final_score, na.rm = T)) %>%
    filter(final_score == best_score) %>%
    group_by(country_code_nuts) %>%
    mutate(
      number = as.numeric(str_extract_all(best_scenario_final, "\\d+")),
      number = if_else(is.na(number), 0, number),
      number = if_else(str_detect(scenario, "best scenario"), number + 10, number),
      max_number = max(number),
      num_scenario = max_number %% 10,
      best_scenario_final = paste("scenario", num_scenario)
    ) %>%
    ungroup() %>%
    filter(number == max_number) %>%
    select(country = country_name_ltn, nuts = country_code_nuts, scenario, best_scenario_final, best_score)
  
  nuts_best <- best_scenario %>%
    select(nuts, best_scenario_final, best_score) %>%
    distinct()
  
  final_scores_nuts <- eu_qrq_final %>%
    select(!best_scenario_final) %>%
    left_join(nuts_best, by = c("nuts")) %>%
    filter(scenario == best_scenario_final) %>%
    mutate(scenario = "best scenario") %>%
    select(country, nuts, indicator, scenario, best_scenario_final, best_score, QRQ_value, capital)
  
  final_scores_country <- final_scores_nuts %>%
    left_join(weight.df %>% select(country, nuts = nuts_id, regionpop, regionpoppct),
              by = c("country", "nuts")) %>%
    drop_na() %>%
    group_by(country, indicator) %>%
    mutate(
      region_adjusted = regionpop / sum(regionpop),
      repeated = if_else(regionpoppct == region_adjusted, 1, 0),
      QRQ_country_value = region_adjusted * QRQ_value
    ) %>%
    ungroup() %>%
    group_by(country, indicator, scenario) %>%
    summarise(
      QRQ_value = sum(QRQ_country_value)) %>%
    distinct()
  
  list(final_scores_nuts, final_scores_country, nuts_best)
}

# Main iterative process
run_iterations <- function(iterations = 7) {
  
  eu_qrq_final_it <- eu_qrq_final
  EU_QRQ_country_it <- EU_QRQ_country
  
  results_list <- list()
  
  for (i in 1:iterations) {
    
    print(paste("Iteration", i))
    
    final_scores.ls <- best_scenario.fn(QRQ_flagging_system.df)
    names(final_scores.ls) <- c("final_scores_nuts", "final_scores_country", "nuts_best")
    
    # NUTS LEVEL
    final_scores_nuts_final <- final_scores.ls[["final_scores_nuts"]] %>%
      select(country, nuts, capital, indicator, QRQ_value, scenario, best_scenario_final) %>%
      mutate(scenario = paste("best scenario", i))
    
    eu_qrq_final_it <- eu_qrq_final_it %>%
      rbind(final_scores_nuts_final)
    
    # COUNTRY LEVEL
    final_scores_country_final <- final_scores.ls[["final_scores_country"]] %>%
      select(country_name_ltn = country, indicator, QRQ_value, scenario) %>%
      mutate(
        scenario = paste("best scenario", i),
        best_scenario_final = paste("best scenario", i)
      )
    
    EU_QRQ_country_it <- EU_QRQ_country_it %>%
      rbind(final_scores_country_final)
    
    # QRQ validation
    
    TPS_validation_it <- QRQ_ranking.fn(data = EU_QRQ_country_it, analysis = "TPS")
    ROLI_validation_it <- QRQ_ranking.fn(data = EU_QRQ_country_it, analysis = "ROLI")
    GPP_validation_it <- QRQ_ranking.fn(data = eu_qrq_final_it, analysis = "GPP")
    TPS_nuts_validation_it <- QRQ_ranking.fn(data = eu_qrq_final_it, analysis = "NUTS")
    Positions_validation_it <- qrq_outlier_analysis(data = eu_qrq_final_it, type = "position")
    Scores_validation_it <- qrq_outlier_analysis(data = eu_qrq_final_it, type = "score")
    Capitals_analysis_it <- capitals.fn(data = eu_qrq_final_it)
    
    # QRQ flagging system
    QRQ_flagging_system.df <- flags_overview(type = "QRQ",
                                             QRQ_country_data     = EU_QRQ_country_it,
                                             QRQ_nuts_data        = eu_qrq_final_it,
                                             TPS_validation       = TPS_validation_it,
                                             ROLI_validation      = ROLI_validation_it,
                                             GPP_validation       = GPP_validation_it,
                                             TPS_nuts_validation  = TPS_nuts_validation_it,
                                             Positions_validation = Positions_validation_it,
                                             Scores_validation    = Scores_validation_it,
                                             Capitals_analysis    = Capitals_analysis_it
                                             )
    
    # Final table
    final_table <- QRQ_flagging_system.df %>%
      group_by(scenario) %>%
      summarise(
        flags_POS_iqr        = sum(c_flags_POS_iqr, na.rm = T),
        flags_SCORE_iqr      = sum(c_flags_SCORE_iqr, na.rm = T),
        flags_CAPITALS_iqr   = sum(c_flags_CAPITALS_iqr, na.rm = T),
        total_flags_iqr      = sum(total_flags_iqr, na.rm = T),
        total_flags_nuts_iqr = sum(total_flags_nuts_iqr, na.rm = T),
        NeedToReview         = sum(need_to_review, na.rm = T)
      ) %>%
      drop_na()
    
    print(table(final_table$scenario, final_table$total_flags_iqr))
    print(table(final_table$scenario, final_table$NeedToReview))
    
    # Store the results of this iteration
    
    results_list[[i]] <- list(final_table = final_table, 
                              final_scores = final_scores.ls,
                              QRQ_flagging_system = QRQ_flagging_system.df)
  }
  
  return(results_list)
}
