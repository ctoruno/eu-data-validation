## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Pre-settings
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
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
  "haven", "readxl", "writexl", "haven",
  
  # Utilities
  "caret",
  
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
  
} else {
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Data list                                                                                  ----
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
           "JSE_quickresol", "JSE_indjudges", "COR_judges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", 
           "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights")

match_indicators <- function() {
  
  ## 1.1 Identifying indicators    =============================================================================
  
  tpsvars<- c("SPE_489_qa6_3", "SPE_489_qa6_5", "SPE_534_QA15_7", "SPE_534_QA15_13", "SPE_507_qb1_4", "SPE_043_QA11_1", "SPE_489_qa8_1", 
              "SPE_489_qa8_3", "FIW_B1", "FIW_D4", "VDM_v2exrescon", "VDM_v2juhccomp", "SPE_507_qb1_2", "VDM_v2csreprss",
              "SPE_507_qb5_2", "SPE_489_qa4_5", "GCB_Q20_2", "SPE_534_QA15_4", "SPE_534_QA6", "SPE_534_QA2", "GCB_Q9_3",
              "SPE_489_qa4_4", "SPE_534_QA15_11", "SPE_534_QA15_10", "FRS_Views on authorities providing information for people in a simple way",
              "FRS_Views on authorities providing information about people's rights and entitlements", "FRS_Views on authorities making information easy to find online",
              "FRS_Views on authorities making information easy to find without using the internet", "SPE_489_qa8_1", 
              "SPE_489_qa8_3", "FIW_B1", "FIW_D4", "ESS_pbldmna", "ESS_volunfp", "ESS_trstprl", "ESS_trstplc", 
              "ESS_trstprt", "SPE_489_qa8_1", "SPE_489_qa8_3", "FIW_B1", "FIW_D4", "SPE_489_qa2_1", "EWC_osh_risk",
              "EWC_work_life_balance", "SPE_507_qb1_4", "ESS_cttresac", "SPE_489_qa5_1", "FIW_F2", "OCI_Criminality avg,",
              "FRS_Views on authorities providing information for people in a simple way", "VDM_v2clacjstm", "VDM_v2clacjstw",
              "SPE_489_qa5_2", "SPE_489_qa5_2", "SPE_489_qa6_3", "GCB_Q5_7", "SPE_489_qa6_1", "SPE_489_qa6_4", 
              "GCB_Q5_6", "SPE_489_qa5_4", "GCB_Q5_7", "SPE_489_qa6_3", "FRS_Perception of the way the police generally treats people",
              "FIW_F2", "VDM_v2cltort"
  )
  
  gppvars<-c("JSE_indjudges", "ROL_courtrulings_imp", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", 
             "CPA_cleanelec_local", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "PAB_emergpower",
             "PAB_overcourts", "PAB_manipulelect", "PAB_attackmedia", "PAB_misinfo", "ROL_corruption_imp", "LEP_bribesreq",
             "ORC_corimpact", "COR_3year_change", "BRB_health_B", "BRB_permit_B", "ROL_abusepower_imp", "ORC_pconnections",
             "IRE_campaign", "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "CPA_media_freeop",
             "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "CPA_protest", "CPA_cso", "TRT_parliament", "TRT_police",
             "TRT_pparties", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "ROL_equality_imp",
             "CTZ_laborcond_A", "CTZ_laborcond_A", "CPA_freevote", "JSE_equality", "ROL_constprotection_imp",
             "CJP_proofburden", "SEC_orgcrime", "JSE_rightsaware", "JSE_access2assis", "JSE_access2assis", "JSE_affordcosts",
             "JSE_quickresol", "JSE_indjudges", "COR_judges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", 
             "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights")
  
  subpillar<- as.character(c(1.02, 1.02, 1.03, 1.03, 1.04, 1.04, 1.05, 1.05, 1.05, 1.05, 1.06, 1.07, 1.09, 1.10, 1.10, 1.11, 1.12, 
                             2.1, 2.1, 2.1, 2.1, 2.4, 2.4, 3.1, 3.1, 3.1, 3.1, 3.1, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2,
                             4.2, 4.2, 4.2, 4.2, 4.3, 4.4, 4.4, 4.5, 4.6, 4.6, 4.6, 5.2, 7.1, 7.2, 7.2, 7.3, 7.3, 7.4, 7.4, 7.5,
                             8.1, 8.1, 8.2, 8.3, 8.3, 8.5, 8.6, 8.7
  ))
  
  pillar<- gsub("\\..*", "", subpillar)
  
  a <- as.data.frame(cbind(tpsvars, gppvars, subpillar, pillar))
  
  return(a)
  
}

