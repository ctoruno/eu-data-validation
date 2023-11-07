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
  "haven", "readxl", "writexl", "haven", "rio",
  
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


if (Sys.info()["user"] == "ctoruno") {
  path2eu <- paste0("/Users/ctoruno/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2eu <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/EU Subnational")
  
} else if (Sys.info()["user"]=="Dhabiby"){
  path2eu<- paste0("/Users/Dhabiby/World Justice Project/Research - EU Subnational")
  
} else {
  path2eu <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - EU SUBNATIONAL DIRECTORY"
  
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
           "JSE_quickresol", "JSE_indjudges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", 
           "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "COR_parliament", "COR_govt_national",
           "COR_govt_local", "COR_police", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CTZ_accountability",
           "CPB_freeassoc", "CPA_law_langaval", "CPB_unions", "CPB_freexp", "CPA_cleanelec_local", "CPB_community", "CPB_freemedia",
           "CPB_freexp_cso", "CPA_partdem_congress", "CPB_freexp_pp", "CPA_partdem_localgvt", "LEP_rightsresp", "LEP_accountability"
           )

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
              "FIW_F2", "VDM_v2cltort", rep("NA", 22)
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
             "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "COR_parliament", "COR_govt_national",
             "COR_govt_local", "COR_police", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CTZ_accountability",
             "CPB_freeassoc", "CPA_law_langaval", "CPB_unions", "CPB_freexp", "CPA_cleanelec_local", "CPB_community", "CPB_freemedia",
             "CPB_freexp_cso", "CPA_partdem_congress", "CPB_freexp_pp", "CPA_partdem_localgvt", "LEP_rightsresp", "LEP_accountability")
  
  subpillar<- as.character(c(1.02, 1.02, 1.03, 1.03, 1.04, 1.04, 1.05, 1.05, 1.05, 1.05, 1.06, 1.07, 1.09, 1.10, 1.10, 1.11, 1.12, 
                             2.1, 2.1, 2.1, 2.1, 2.4, 2.4, 3.1, 3.1, 3.1, 3.1, 3.1, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2,
                             4.2, 4.2, 4.2, 4.2, 4.3, 4.4, 4.4, 4.5, 4.6, 4.6, 4.6, 5.2, 7.1, 7.2, 7.2, 7.3, 7.3, 7.4, 7.4, 7.5,
                             8.1, 8.1, 8.2, 8.3, 8.3, 8.5, 8.6, 8.7, 2.1, 2.1, 2.1, 2.1, 3.1, 3.1, 3.1, 5.1, 1.11, 3.2, 3.1, 4.4,
                             4.2, 1.4, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 8.6, 8.6
  ))
  
  pillar<- gsub("\\..*", "", subpillar)
  
  
  spname<- c("Judicial Constraints", "Independent Oversight", "Elections are free, fair, and secure",
             "Non-governmental checks", "Respect for the legitimacy of the constitutional order, the law making process, and political opponents (absence of authoritarianism)",
             "Respect for judicial independence (absence of authoritarianism)", "Respect for the electoral process (absence of authoritarianism)",
             "Respect for civil liberties (absence of authoritarianism)", "Government officials who abuse their power are sanctioned for misconduct (accountability and sanctions for misconduct)",
             "Government officials who commit crimes are prosecuted and punished (accountability and sanctions for misconduct)",
             "Absence of Bribery", "Absence of Embezzlement and fraud", "Absence of nepotism, favoritism, and patronage", "Right to information is effectively guaranteed", 
             "Civic participation is effectively guaranteed", "Freedoms", "Equality", "Solidarity", "Citizens' Rights",
             "Justice", "People feel safe", "Absence of crime and violence", "Legal security", "People can access quality legal assistance and representation", "Civil justice is people-centered, accessible, efficient, and outcome-oriented",
             "Civil justice is impartial and free from corruption and undue influence", "Civil justice is effectively enforced",
             "Criminal Investigation", "Prosecution and pre-trial process", "Adjudication", "Victim's Rights", "Due process of law",
             "Prisons")
  
  
  pillarname<- c("Constraints on Government Powers", "Absence of Corruption", "Open Government", "Fundamental Rights", 
                 "Order and Security", "Regulatory Enforcement", "Civil Justice", "Criminal Justice")
  
  pillarnew<- c()
  for (i in 1:length(pillar)) {
    
    n<- as.numeric(pillar[i])
    pillarnew[i]<- paste0(pillar[i], ": ", pillarname[n])
    
  }
  
  spnew<-c()
  
  for (i in 1:length(subpillar)){
    
    n<- which(sort(unique(subpillar)) == subpillar[i])
    spnew[i]<- paste0(subpillar[i], ": ", spname[n])
    
  }
  
  
  
  a <- as.data.frame(cbind(tpsvars, gppvars, spnew, pillarnew))
  colnames(a)<- c("tpsvars", "gppvars", "subpillar", "pillar")
  
  return(a)
  
}

