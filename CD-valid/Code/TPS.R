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
## This version:      October 27th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 #gpp<- read_dta("../Input/example_clean.dta")
 #tps<- read_csv("../../TPS/TPS_data.csv")
 country<- "Czechia"

TPS_function<- function(gpp, tps, country){
  
  countries<- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain",      
                "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta",      
                "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")
  ns<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
         "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
         "RO", "SE", "SI", "SK")
  
  ind<- which(countries == country)
  cy<- ns[[ind]]
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
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
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  nuts<- c("CZ", "EE", "FI", "FR", "SI", "ES", "SE")
  tps2<- tps%>%
    select(country_code_nuts, all_of(tpsvars))%>%
    filter(country_code_nuts %in% nuts)
  
  gpp$Country<- rep(NA, nrow(gpp))
  cntry<- c(1:7)
  nuts<- c("CZ", "EE", "FI", "FR", "SI", "ES", "SE")
  gpp2<- as_tibble(gpp%>%
                      mutate(Country = case_when(is.na(Country) ~ 
                                                   deframe(tibble(cntry, nuts))[country_name_ltn], 
                                                 TRUE ~ Country))%>%
                      select(Country, everything())%>%
                      select(-`country_name_ltn`)%>%
                      arrange(Country))%>%
    select(Country, all_of(gppvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- gpp2
  for(i in gppvars){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
    
  }
  
  ro<- c("JSE_indjudges", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", "CPA_cleanelec_local", 
         "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "LEP_bribesreq", "IRE_campaign",
         "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "TRT_parliament", "TRT_police", 
         "TRT_pparties", "CTZ_laborcond_A", "JSE_equality", "CJP_proofburden", "JSE_rightsaware", "JSE_access2assis",
         "JSE_affordcosts", "JSE_quickresol", "COR_judges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors",
         "CJP_resprights", "CJP_fairtrial", "CJP_saferights")
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                         ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("CPA_protest", "CPA_cso")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 2, ifelse(oriented[[i]] == 2, 1, NA_real_))
  }

  ## 1.4 Normalize indicators ==================================================================================
    
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  gppaggregate<- normalized%>%
    group_by(Country)%>%
    summarise_at(gppvars, mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Threshold Test                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  final<- as.data.frame(matrix(nrow=0, ncol=10))
  colnames(final)<- c("Country", "GPP_Variable_Name", "GPP_datapoint", "TPS_Variable_Name", "TPS_datapoint", "TPS_Source", 
                      "Difference", "Flag", "Pillar", "Sub_Pillar")
  
  for (i in c(1:length(tpsvars))){
    
    t<- tps2%>%
      filter(country_code_nuts == cy)%>%
      select(country_code_nuts, tpsvars[[i]])
    
    g<- gppaggregate%>%
      filter(Country == cy)%>%
      select(Country, gppvars[[i]])
    
    gp<- g[[1,2]]
    tp<- t[[1,2]]
    
    diff<- abs(gp - tp)

      
      if (startsWith(tpsvars[[i]], "SPE")| startsWith(tpsvars[[i]], "FLE")){
        
        s<- "Eurobarometer"
        
      }
      if (startsWith(tpsvars[[i]], "FIW")){
        
        s<- "Freedom in the World"
        
      }
      if (startsWith(tpsvars[[i]], "VDM")){
        
        s<- "Varieties of Democracy"
        
      }
      if (startsWith(tpsvars[[i]], "ESS")){
        
        s<- "European Social Survey"
        
      }
      if (startsWith(tpsvars[[i]], "WVS")){
        
        s<- "World Values Survey"
        
      }
      if (startsWith(tpsvars[[i]], "FRS")){
        
        s<- "Fundamental Rights Survey"
        
      }
      if (startsWith(tpsvars[[i]], "GCB")){
        
        s<- "Global Corruption Barometer"
        
      }
      if (startsWith(tpsvars[[i]], "GTI")){
        
        s<- "Government Transparency Index"
        
      }
      if (startsWith(tpsvars[[i]], "PII")){
        
        s<- "Public Integrity Index"
        
      }
      if (startsWith(tpsvars[[i]], "OCI")){
        
        s<- "Organized Crime Index"
        
      }
      
    
    f<- tibble("Country" = country, "GPP_Variable_Name" = gppvars[[i]], "GPP_datapoint" = gp, "TPS_Variable_Name" = tpsvars[i], 
           "TPS_datapoint" = tp, "TPS_Source" = s, "Difference" = diff, "Flag" = ifelse(diff > .25, "red", ifelse(diff> .1, "yellow", "green")),
           "Pillar" = pillar[i], "Sub_Pillar"= subpillar[i])
    final<- rbind(final, f)
  }
  
  return(final)
  
}
