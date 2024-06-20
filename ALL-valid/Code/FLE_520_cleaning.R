## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FLE_520 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 20th, 2024
##
## This version:      June 20th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


FLE_520_cleaning<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================

  
  targetvars<- c("q2a_1","q3")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  #check bulgaria and cyprus, hungary, lithu, portugal, romania, sweden
  
  nutsregs<- c("d12at_r", "d12be_r", "d12bg", "d12hr", "d12cy", "d12cz", "d12dk", "d12ee", "d12de", "d12gr_r", "d12es_r", 
               "d12fi", "d12fr_r", "d12hu", "d12ie", "d12it_r", "d12lt", "d12lu", "d12lv", "d12nl_r", "d12pl_r", "d12pt",
               "d12ro", "d12se_r", "d12si_r", "d12sk", "d12mt")
  
  nutscols<- df%>%
    filter(isocntry %in% cntry)%>%
    select(isocntry, all_of(targetvars), all_of(nutsregs))
  
  for (i in nutsregs){
    
    tmp <- attributes(nutscols[[i]])$labels
    nutscols[[i]] <- names(tmp)[match(nutscols[[i]], tmp)]
    
  }
  
  df2<- nutscols%>%
    mutate(across(everything(), ~ ifelse(str_starts(., "Inap"), "", .)))%>%
    mutate(NUTS = apply(select(., starts_with("d12")), 1, paste, collapse = ""))%>%
    left_join(nutsencoding, by = join_by(NUTS == NUTS_ltn))%>%
    select(isocntry, NUTS_ID, all_of(targetvars))%>%
    arrange(NUTS_ID)
  
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro<- c("q3")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, ifelse(oriented[[i]]== 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("q2a_1")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(!(oriented[[i]] %in% c(1,2,3,4)) , NA_real_, oriented[[i]])
    
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(isocntry, NUTS_ID)%>%
    summarise_at(targetvars, mean, na.rm= TRUE)
  
  colnames(aggregate)<- c("Country", "NUTS", paste0("FLE_520", "_", colnames(aggregate)[-c(1,2)]))
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  return(aggregate)
  
}

