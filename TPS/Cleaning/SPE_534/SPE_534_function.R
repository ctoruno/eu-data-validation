## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- SPE_534 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 10th, 2023
##
## This version:      October 10th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#COME BACK TO THIS, 2023 VERSION OF SPE_502

SPE_534_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  targetvars<- c("isocntry", "QA15_5", "QA15_6", "QA15_7", "QA15_13",
                 "QA5", "QA6", "QA7_2", "QA7_4", "QA7_8", "QA7_9", "QA7_12", "QA10", "QA15_1", "QA15_2", "QA15_3", 
                 "QA15_4", "QA15_8", "QA15_9", "QA15_11", "QA15_12")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  aggregate<- data.frame(ctrycode = cntry)
  
  for (n in targetvars){
    n<- "QA15_5"
    f<- df[[n]]
    
    dfq<- f[c(8,11,13,15,17,19),]
    dfq[1,1]<- "q"
    
    colnames(dfq) <- dfq[1,]
    dfq <- dfq[-1, ] 
    
    ## 1.3 Re-orient indicators ================================================================================
    
    if (n %in% p1){
      
      vals<-dfq[[1]]
      new_vals<- c(1,2/3,1/3,0, NA_real_)
    }
    
    if (n %in% p2){
      vals<-dfq[[1]]
      new_vals<- c(0,1/3,2/3,1, NA_real_)
    }
    
    
    ## 1.4 Normalize indicators ================================================================================
    
    dfq[[n]] <- as.numeric(rep(NA, nrow(dfq)))
    
    df2<- dfq%>%
      mutate(!!paste0(n) := case_when(is.na(dfq[[n]]) ~ 
                                        deframe(tibble(vals, new_vals))[q], 
                                      TRUE ~dfq[[n]]))%>%
      select(-c(EU27, q))%>%
      pivot_longer(cols = BE:SE, names_to = "ctrycode", values_to = "count")
    
    df2$count<- as.numeric(df2$count)
    
    df3<- df2%>%
      uncount(count)%>%
      select(ctrycode, paste0(n))
    
    ## 1.5 Aggregate indicators at the country level ===========================================================
    
    agg<- df3%>%
      group_by(ctrycode)%>%
      summarise_at(n, mean, na.rm= TRUE)
    
    aggregate<- left_join(aggregate, agg, by = join_by(ctrycode))
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[isocntry], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`isocntry`)
  
  return(clean)
  
}
