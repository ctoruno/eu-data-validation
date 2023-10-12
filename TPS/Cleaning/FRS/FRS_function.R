## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation- FRS Function
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 12th, 2023
##
## This version:      October 12th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FRS_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators    =============================================================================
  
  p4<- c("Importance of having the reporting by the media be free from government influence",
         "Importance of protecting the rights of minority groups",
         "Extent of concern for experiencing political intimidation during election campaigns in the country",
         "Views on the ability of NGOs to do their work free from government intimidation",
         "Awareness of the right to send complaints to the European Ombudsman",
         "Awareness of the right to make a petition to the European Parliament",
         "Awareness of the right to move and live freely in the EU",
         "Awareness of the right to vote in the European Parliament elections when living in another EU Member State",
         "Awareness of a law that forbids discriminating against job applicants - Because of their age",
         "Awareness of a law that forbids discriminating against job applicants - Because they are a man",
         "Awareness of a law that forbids discriminating against job applicants - Because they are a woman",
         "Awareness of a law that forbids discriminating against job applicants - Because they have a disability",
         "Awareness of a law that forbids discriminating against job applicants - Because of their ethnic origin",
         "Awareness of a law that forbids discriminating against job applicants - Because of their skin colour",
         "Awareness of a law that forbids discriminating against job applicants - Because of their religion or belief",
         "Awareness of a law that forbids discriminating against job applicants - Because of their sexual orientation",
         "Awareness of a law that forbids discriminating against job applicants - Because of their gender identity",
         "Awareness of laws that forbid discriminating against job applicants - Because they are overweight/obese",
         "Awareness of the General Data Protection Regulation (GDPR)",
         "Discrimination in employment on any grounds, in the past 5 years",
         "Discrimination in employment, by grounds for feeling discriminated against, in the past 5 years",
         "Discrimination in employment on any grounds, in the past 12 months",
         "Number of discrimination incidents in employment in the past 12 months",
         "Main reasons for feeling discriminated against in employment",
         "Discrimination when using selected services, on any grounds, in the past 5 years",
         "Discrimination when using selected services, by grounds for feeling discriminated against, in the past 5 years",
         "Discrimination when using selected services, on any grounds, in the past 12 months",
         "Number of discrimination incidents when using selected services, in the past 12 months",
         "Main reasons for feeling discriminated against when using selected services",
         "Statements on gender equality - Attention given to sexual harassment at work",
         "Statements on gender equality - Working in a management position",
         "Statements on gender equality - Care responsibilities involving children",
         "Statements on gender equality - Ensuring equal pay",
         "Experience of not being equally treated by public administration or local authorities"
  )
  
  targetvars<- c(p4)
  
  cntry<- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", 
            "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy",  "Lithuania", "Luxembourg",
            "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden",
            "Slovenia", "Slovakia")
  
  ## 1.2 Sub-setting data  =====================================================================================

  f<- df%>%
    filter(question %in% targetvars)%>%
    filter(country %in% cntry)
  
  aggregate<- data.frame(ctrycode = cntry)
  
  for (n in targetvars){
    
    f<- df%>%
      filter(question == n)
    
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
                                 deframe(tibble(cntry, nuts))[ctrycode], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-`ctrycode`)
  
  return(clean)
  
}


