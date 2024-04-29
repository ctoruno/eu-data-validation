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
  
))

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

normalizingvars<- function(gppctry, gppvars){

  oriented<- gppctry
  
  for(i in gppvars){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
    
  }
  
  ro<- c("JSE_indjudges", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", "CPA_cleanelec_local", 
         "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "LEP_bribesreq", "IRE_campaign",
         "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "TRT_parliament", "TRT_police", 
         "TRT_pparties", "CTZ_laborcond", "JSE_equality", "CJP_proofburden", "JSE_rightsaware", "JSE_access2assis", "JSE_affordcosts", "JSE_quickresol", "COR_judges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "CPB_community", "CPB_freeassoc", "COR_govt_local", "COR_parliament", "ROL_equality_sig", "JSE_polinfluence", "COR_govt_national", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CPA_law_langaval", "CPB_unions", "CPB_freemedia", "CPA_partdem_congress", "CPB_freexp_pp","CPA_partdem_localgvt")
  
  ro2<- c("CPA_protest", "CPA_cso")
  
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
cols_oriented <- names(select_if(oriented, is.numeric))
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
oriented[nrow(oriented) + 1,] <- c(rep("mins", ncol(select_if(oriented, is.character))), rep(list(1), ncol(select_if(oriented, is.numeric))))


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

