## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FLE_524 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 18th, 2024
##
## This version:      June 18th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


FLE_524_cleaning<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  # p1<- c("Q7_6", "Q7_7", "Q7_8", "Q8_1", "Q8_2", "Q8_3")
  # p2<- c("Q1_1", "Q1_2", "Q3", "Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q5_1", "Q5_2", "Q7_2", "Q7_4", "Q7_5")
  # p6<- c("Q1_3", "Q1_4", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q4_7")
  
  targetvars<- c("q1_2","q1_3","q5_1", "q5_2", "q7_2")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  #check bulgaria and cyprus, hungary, lithu, portugal, romania, sweden
  
  nutsregs<- c("d12at_r", "d12be_r", "d12bg", "d12hr", "d12cy", "d12cz", "d12dk", "d12ee", "d12de", "d12gr_r", "d12es_r", 
               "d12fi", "d12fr_r", "d12hu", "d12ie", "d12it_r", "d12lt", "d12lu", "d12lv", "d12nl_r", "d12pl_r", "d12pt",
               "d12ro", "d12se_r", "d12si_r", "d12sk", "d12mt")
  
  nutscols<- df%>%
    filter(isocntry %in% cntry)%>%
    select(isocntry, all_of(targetvars), all_of(nutsregs))
  
  columns<- names(df%>%select(starts_with("d12")))
  
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
  
  ro<- c("q1_2", "q1_3", "q7_2")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 5, NA_real_, oriented[[i]])
  }
  
  ro2<- c("q5_1", "q5_2")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 6, NA_real_, oriented[[i]])
    
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    group_by(isocntry, NUTS_ID)%>%
    summarise_at(targetvars, mean, na.rm= TRUE)
  
  colnames(aggregate)<- c("Country", "NUTS", paste0("FLE_524", "_", colnames(aggregate)[-c(1,2)]))
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  return(aggregate)
  
}

