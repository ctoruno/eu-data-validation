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
## This version:      October 23rd, 2023
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
  
  p4<- c(#"Importance of having the reporting by the media be free from government influence",
         #"Importance of protecting the rights of minority groups",
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
         "Discrimination in employment on any grounds, in the past 12 months",
         #"Number of discrimination incidents in employment in the past 12 months",
         "Discrimination when using selected services, on any grounds, in the past 5 years",
         "Discrimination when using selected services, on any grounds, in the past 12 months",
         #"Number of discrimination incidents when using selected services, in the past 12 months",
         "Statements on gender equality - Attention given to sexual harassment at work",
         "Statements on gender equality - Working in a management position",
         "Statements on gender equality - Care responsibilities involving children",
         "Statements on gender equality - Ensuring equal pay",
         "Experience of not being equally treated by public administration or local authorities"
  )
  
  p5<- c("Worry about experiencing a burglary",
         "Worry about experiencing a terrorist attack",
         "Worry about experiencing misuse of online bank account or payment card",
         "Worry about experiencing a theft of mobile phone, wallet or purse",
         "Witnessing a crime against the environment - willingness to intervene",
         "Witnessing an incident of intimate partner violence - willingness to intervene",
         "Witnessing a parent slap their child - willingness to intervene",
         "Experiences of physical violence in the past 5 years",
         "Experiences of physical violence in the past 12 months",
         "Experiences of harassment in the past 5 years",
         "Experiences of harassment in the past 12 months",
         "Experiences of in-person harassment in the past 5 years",
         "Experiences of cyberharassment in the past 5 years",
         "Experiences of in-person harassment in the past 12 months",
         "Experiences of cyberharassment in the past 12 months",
         "Experiences of burglary in the past 5 years",
         "Experiences of burglary in the past 12 months",
         "Experiences of online banking or payment card fraud in the past 5 years",
         "Experiences of online banking or payment card fraud in the past 12 months",
         "Experiences of consumer fraud in the past 5 years",
         "Experiences of consumer fraud in the past 12 months"
         #"Type of perpetrator(s) in the incident of harassment",
         #"Location of the incident of harassment",
         #"Whether the incident of harassment was of a sexual nature",
         #"Description of the perpetrator(s) in the incident of harassment",
         #"Gender of the perpetrator(s) in the incident of harassment",
         #"Type of method for making the purchase when experienced consumer fraud",
         #"Type of perpetrator(s) in the incident of physical violence",
         #"Location of the incident of physical violence",
         #"Whether the incident of physical violence was of a sexual nature",
         #"Description of the perpetrator(s) in the incident of physical violence",
         #"Gender of the perpetrator(s) in the incident of physical violence",
         #"Psychological consequences from the incident of physical violence",
         #"Physical injuries from the incident of physical violence",
         #"Organisations and services contacted following the incident of physical violence",
         #"Whether experience of consumer fraud involved products or services ordered from abroad"
  )
  
  p7<- c("Views on the ability of judges to do their job free from government influence",
         "Views on authorities providing information for people in a simple way",
         "Views on authorities providing information about people's rights and entitlements",
         "Views on authorities making information easy to find online",
         "Views on authorities making information easy to find without using the internet",
         "Awareness of the European Convention on Human Rights",
         "Awareness of the EU Charter of Fundamental Rights",
         "Awareness of the Universal Declaration of Human Rights",
         "Awareness of the national gender equality body",
         "Awareness of the national racial equality body",
         "Awareness of the national human rights institution",
         "Awareness of the national supervisory authority for data protection"
  )
  
  p8<- c(
    "Views on the ability of judges to do their job free from government influence",
    "Perception of the way the police generally treats people",
    #"Reporting the incident of physical violence",
    #"Reasons for not reporting the incident of physical violence to the police",
    "Satisfaction with the way the police handled the incident of physical violence",
    #"Reporting the incident of harassment",
    #"Reasons for not reporting the incident of harassment to the police",
    "Satisfaction with the way the police handled the incident of harassment",
    #"Reporting burglary to the police",
    #"Reasons for not reporting burglary to the police",
    "Reporting online banking or payment card fraud, in total",
    #"Reporting online banking or payment card fraud, by type of authority or service",
    #"Reasons for not reporting online banking or payment card fraud to the police",
    "Reporting consumer fraud, in total"
    #"Reporting consumer fraud, by type of authority or service",
    #"Reasons for not reporting consumer fraud to the police"
  )
  
  targetvars<- unique(c(p4, p5, p7, p8))
  
  cntry<- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", 
            "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy",  "Lithuania", "Luxembourg",
            "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden",
            "Slovenia", "Slovakia")
  
  ## 1.2 Sub-setting data  =====================================================================================

  f<- df%>%
    filter(question %in% targetvars)%>%
    filter(country %in% cntry)
  
  f$percentage<- f$percentage/100
  
  aggregate<- data.frame("cy" = cntry)
  
  for (n in targetvars){
    
    f2<- f%>%
      filter(question == n)
    
  ## 1.3 Re-orient indicators ==================================================================================
    
    vals<- unique(f2$answer)
    
    if (grepl("against job applicants", n)| grepl("Reporting online banking", n)){
      new_vals<- c(1, 0)
    }
    
    if (grepl("on any grounds", n) | grepl("Experiences of", n)){
      new_vals<- c(0,1)
    }
    
    if (grepl("Awareness of the", n) | grepl("Experience of not being", n)| grepl("willingness to intervene", n)|
        grepl("Reporting consumer fraud", n)){
      new_vals<- c(1, 0, NA_real_)
    }
      
    if (grepl("equality - Attention", n) | grepl("equality - Care", n) | grepl("Extent of concern", n) 
        | grepl("Views on authorities", n)){
      new_vals<- c(0, .5, 1, NA_real_)
    }
    
    if (grepl("equality - Ensuring", n) | grepl("equality - Working", n) | grepl("Perception of the way the police", n)){
      new_vals<- c(1, .5, 0, NA_real_)
    }
    
    if (grepl("NGOs", n) | grepl("Worry about experiencing", n) | grepl("ability of judges", n)){
      new_vals<- c(0, 1/3, 2/3, 1, NA_real_)
    }
    
    if (grepl("Satisfaction with the way", n)){
      
      new_vals<- c(0,1, NA_real_)
    }
    
  ## 1.4 Normalize indicators ==================================================================================
    
    f2$prop <- as.numeric(rep(NA, nrow(f2)))
    
    df2<- f2%>%
      mutate(prop := case_when(is.na(prop) ~ 
                                        deframe(tibble(vals, new_vals))[answer], 
                                      TRUE ~prop))%>%
      select(-answer)
  
  ## 1.5 Aggregate indicators at the country level =============================================================
    
    indicatordf<- data.frame((matrix(ncol = 2, nrow = 0)))
    colnames(indicatordf) = c("cy", paste0(n))
    
    for (c in cntry){
      
      df3<- df2%>%
        filter(country == c)
      
      if (is.na(sum(df3$prop))){
        
        no_na<- df3%>%
          filter(!is.na(prop))
        available<- sum(no_na$percentage)
        
        weight<- 1/available
        
        wt<- no_na$percentage*weight
        
        sumct <- sum(wt*no_na$prop)
        
        ind<- data.frame(c, sumct)
        colnames(ind)<- c("cy", paste0(n))
        indicatordf<- rbind(indicatordf, ind)
      }
      
      if (!is.na(sum(df3$prop))){
        
       sumct<- sum(df3$percentage*df3$prop)
       ind<- data.frame(c, sumct)
       colnames(ind)<- c("cy", paste0(n))
       indicatordf<- rbind(indicatordf, ind)
        
      }
      
    }
    
    aggregate<- left_join(aggregate, indicatordf, by = join_by(cy))
    
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Preparing Data                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean<- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[cy], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-cy)
  
  return(clean)
  
}

