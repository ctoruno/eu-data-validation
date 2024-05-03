## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FLE_524 Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2023
##
## This version:      October 16th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


FLE_524_clean<- function(df){

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  p1<- c("Q7_6", "Q7_7", "Q7_8", "Q8_1", "Q8_2", "Q8_3")
  p2<- c("Q1_1", "Q1_2", "Q3", "Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q5_1", "Q5_2", "Q7_2", "Q7_4", "Q7_5")
  p6<- c("Q1_3", "Q1_4", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q4_7")
  
  targetvars<- c(p1,p2, p6)
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  aggregate<- data.frame(ctrycode = cntry)
  
  for (n in targetvars){
    
    f<- df[[n]]
    
    dfq<- f[c(11,19,22,25,28,31),]
    dfq[1,1]<- "q"
    
    colnames(dfq) <- dfq[1,]
    dfq <- dfq[-1, ] 
    
    ## 1.3 Re-orient indicators ==================================================================================
    
    if (n %in% p1){
      
      vals<-dfq[[1]]
      new_vals<- c(1,2/3,1/3,0, NA_real_)
    }
    
    if (n %in% p2){
      vals<-dfq[[1]]
      new_vals<- c(0,1/3,2/3,1, NA_real_)
    }
    
    if (n %in% p6){
      
      #if (n != "Q4_7"){
        vals<-dfq[[1]]
        new_vals<- c(0,1/3,2/3,1, NA_real_)
      # }
      # 
      # #review for how to code non-existent
      # if (n == "Q4_7"){
      #   vals<-dfq[[1]]
      #   new_vals<- c(0,0.25,0.5,0.75, 1)
      # }
      
    }
    
    ## 1.4 Normalize indicators ==================================================================================
    
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
    
    ## 1.5 Aggregate indicators at the country level =============================================================
    
    agg<- df3%>%
      group_by(ctrycode)%>%
      summarise_at(n, mean, na.rm= TRUE)
    
    aggregate<- left_join(aggregate, agg, by = join_by(ctrycode))
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  nuts<- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
           "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
           "RO", "SE", "SI", "SK")
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[ctrycode], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`ctrycode`)
  
  return(clean)
  
}

