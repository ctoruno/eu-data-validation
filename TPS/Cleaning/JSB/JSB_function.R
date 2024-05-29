## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- JSB Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 3rd, 2024
##
## This version:      May 3rd, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


JSB_clean<- function(df){

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  # 5,10,17,21,37
  targetvars<- c("Fig(5)DT.Non-criminal", "Fig(10)CR.Non-criminal", "Fig(17)Comp.av.length.NCA", 
                 "Fig(21)Consumers.admin", "Fig(37)Lawyers")
  
  cntry<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", 
            "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  ## 1.2 Sub-setting data  =====================================================================================
  
  aggregate<- data.frame(Country = cntry)
  
  for (n in targetvars){
    
    if (n == "Fig(5)DT.Non-criminal" | n== "Fig(37)Lawyers" |n == "Fig(10)CR.Non-criminal"){
      f<- df[[n]]
      colnames(f) <- f[1,]
      f <- f[-1, ] 

    }
    
    if (n == "Fig(17)Comp.av.length.NCA"){
      
      f<- df[[n]]
      f<- f[c(1, 5)]
      colnames(f) <- c("CC", "2021")
      
    }
    
    if (n == "Fig(21)Consumers.admin"){
      
      f<- df[[n]]
      f<- f[c(2, 10)]
      colnames(f) <- c("CC", "2021")
      
    }
    
    agg<- f%>%
      select("CC", "2021")%>%
      drop_na(CC)%>%
      filter(CC %in% cntry)
    
    colnames(agg)<- c("Country", gsub("Fig\\([0-9]{1,2}\\)", "", n))
    

    ## 1.3 Re-orient indicators ==================================================================================
    
    
    ## 1.4 Normalize indicators ==================================================================================
    
    
    ## 1.5 Aggregate indicators at the country level =============================================================
    
    aggregate<- left_join(aggregate, agg, by = join_by(Country))
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  aggregate[2:ncol(aggregate)] <- suppressWarnings(sapply(aggregate[2:ncol(aggregate)],as.numeric))
  

  
  return(aggregate)
  
}

