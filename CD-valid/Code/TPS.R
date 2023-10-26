## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation-  GPP & TPS Threshold Comparison
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 26th, 2023
##
## This version:      October 26th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



threshold<- function(gpp, tps, country){
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  gpp<- read_dta("../Input/example_clean.dta")
  tps<- read_csv("../../TPS/TPS_data.csv")
  
  ## 1.1 Identifying indicators    =============================================================================
  
  tpsvars<- c("SPE_489_qa6_3", "SPE_489_qa6_5", "SPE_534_QA15_7", "SPE_534_QA15_13", "SPE_507_qb1_4", "SPE_489_qa8_1", 
              "SPE_489_qa8_3", "FIW_B1", "FIW_D4", "VDM_v2exrescon", "VDM_v2juhccomp", "SPE_507_qb1_2", "VDM_v2csreprss",
              "SPE_507_qb5_2", "SPE_489_qa4_5", "GCB_Q20_2", "SPE_534_QA15_4", "SPE_534_QA6", "SPE_534_QA1", "GCB_Q9_3",
              "SPE_489_qa4_4", "SPE_534_QA15_11", "SPE_534_QA15_10", "FRS_Views on authorities providing information for people in a simple way",
              "FRS_Views on authorities providing information about people's rights and entitlements", "FRS_Views on authorities making information easy to find online",
              "FRS_Views on authorities making information easy to find without using the internet", "SPE_489_qa8_1", 
              "SPE_489_qa8_3", "FIW_B1", "FIW_D4", "ESS_pbldmna", "ESS_volunfp"
  )
  
  
}