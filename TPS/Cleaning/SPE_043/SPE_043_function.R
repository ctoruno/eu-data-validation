## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- SPE_043 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 11th, 2023
##
## This version:      October 11th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SPE_043_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  p4<- c("QA11_1", "QA11_2", "QA11_3", "QA11_4", "QA11_6", "QA11_7")
  
  targetvars<- c(p4)
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================

  aggregate<- data.frame(ctrycode = cntry)
  
  for (n in targetvars){
    
    f<- df[[n]]
    
    dfq<- f[c(8,11,13,15,17,19),]
    dfq<- dfq[,-1]
    dfq[1,1]<- "q"
    colnames(dfq) <- dfq[1,]
    dfq <- dfq[-1, ] 
    dfq<- dfq[,-2]
    
  ## 1.3 Re-orient indicators ==================================================================================
      
      vals<-dfq[[1]]
      new_vals<- c(1,2/3,1/3,0, NA_real_)

    
  ## 1.4 Normalize indicators ==================================================================================
    
    dfq[[n]] <- as.numeric(rep(NA, nrow(dfq)))
    
    df2<- dfq%>%
      mutate(!!paste0(n) := case_when(is.na(dfq[[n]]) ~ 
                                        deframe(tibble(vals, new_vals))[q], 
                                      TRUE ~dfq[[n]]))%>%
      select(-q)%>%
      pivot_longer(cols = BE:SE, names_to = "ctrycode", values_to = "count")
    
    df2$count<- as.numeric(df2$count)
    
    df3<- df2%>%
      uncount(count)%>%
      select(ctrycode, paste0(n))
    
    df3$ctrycode<- recode(df3$ctrycode, "D-W"="DE", "D-E"="DE")
    
  ## 1.5 Aggregate indicators at the country level =============================================================
    
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
  
  clean<- as_tibble(aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[ctrycode], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`ctrycode`)%>%
      arrange(Country))
  
  return(clean)
  
}


