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
## This version:      November 28th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 #gpp<- master_data.df
 #tps<- TPS.df
 #country<- "Cyprus"
 #mat<- metadata

TPS_function_nuts<- function(gpp, tps, country, mat, type){
  
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
  
  tpsvars<- mat$TPS_Variable_Name
  gppvars<- mat$GPP_Variable_Name

  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2<- gpp %>% 
    select(country_name_ltn, nuts_id, nuts_ltn, all_of(gppvars)) 

  ## 1.3 Re-orient indicators ==================================================================================
  
  normalized<- normalizingvars(gpp2, gppvars)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  gppaggregate <- normalized%>%
    group_by(country_name_ltn, nuts_id, nuts_ltn)%>%
    summarise_at(gppvars, mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Threshold Test                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #exm$TPS_source<- ifelse(exm$TPS_source == "VDEM", "Varieties of Democracy", "Freedom in the World")
  
  final<- as.data.frame(matrix(nrow=0, ncol=14))
  colnames(final)<- c("Country", "NUTS", "GPP_Variable_Name", "GPP_datapoint", "TPS_Variable_Name", "TPS_datapoint", "TPS_Source", 
                      "TPS_Year", "TPS_Question", "Difference", "Flag", "Pillar", "Sub_Pillar", "Type_Survey")
 
  for (i in c(1:length(tpsvars))){
    
    t<- tps%>%
      filter(country_code_nuts == cy)%>%
      select(country_code_nuts, tpsvars[[i]])
    
    g<- gppaggregate%>%
      ungroup()%>%
      select(nuts_id, nuts_ltn, gppvars[[i]])
    
    for (j in 1:nrow(g)){
    
    gp<- g[[j,3]]
    tp<- t[[1,2]]
    
    diff<- abs(gp - tp)
    
      f<- tibble("Country" = country, "NUTS" = paste0(g[[j,1]], ":", g[[j,2]]), "GPP_Variable_Name" = gppvars[[i]], "GPP_datapoint" = gp, 
                 "TPS_Variable_Name" = tpsvars[[i]], 
                 "TPS_datapoint" = tp, "TPS_Source" = mat$TPS_Source[[i]], 
                 "TPS_Year" = mat$TPS_Year[[i]], "TPS_Question" = mat$TPS_Question[[i]], 
                 "Difference" = diff, "Flag" = ifelse(diff > .35, "red", "green"),
                 "Pillar" = mat$Pillar[[i]], "Sub_Pillar"= mat$Sub_Pillar[[i]], 
                 "Type_Survey" = mat$Type_Survey[[i]], "Match" = mat$Match[[i]], 
                 "2023  EU Questionnaire" = mat$EU_Questionnaire[[i]], "Description" = mat$Description[[i]])
    
    
    final<- rbind(final, f)
    }
  }
  
  return(final)
  
}
