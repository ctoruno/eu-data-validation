## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - OCI Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 13th, 2023
##
## This version:      October 13th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


OCI_clean <- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("Country", "Criminality avg,", "Criminal markets avg,", "Human trafficking", "Human smuggling",
                  "Arms trafficking", "Flora crimes", "Fauna crimes", "Non-renewable resource crimes", "Heroin trade", 
                  "Cocaine trade", "Cannabis trade", "Synthetic drug trade", "Criminal actors avg,", "Mafia-style groups", 
                  "Criminal networks", "State-embedded actors","Foreign actors", "Resilience avg,", 
                  "Political leadership and governance","Government transparency and accountability", "International cooperation", 
                  "National policies and laws", "Judicial system and detention", "Law enforcement", "Territorial integrity", 
                  "Anti-money laundering", "Economic regulatory capacity", "Victim and witness support", "Prevention", 
                  "Non-state actors" )
  
  
  cntry <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", 
             "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
             "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
             "Slovenia", "Slovakia")
  
  ## 1.2 Sub-setting data========================================================================================
  
  df2<- as_tibble(df$`2023_dataset`)
  
  df3 <- df2 %>%
    filter(Country %in% cntry )%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  # Need to reverse all Criminality indicators
  
  oriented<- df3
  
  ro<- c("Criminality avg,", "Criminal markets avg,", "Human trafficking", "Human smuggling",
         "Arms trafficking", "Flora crimes", "Fauna crimes", "Non-renewable resource crimes", "Heroin trade", 
         "Cocaine trade", "Cannabis trade", "Synthetic drug trade", "Criminal actors avg,", "Mafia-style groups", 
         "Criminal networks", "State-embedded actors","Foreign actors")
  
  for(i in ro){
    oriented[[i]]<- (oriented[[i]] -11)*-1
  }
  
  
  ## 1.4 Normalize indicators ===================================================================================
  
  oriented[nrow(oriented) + 1,] <- c("mins", rep(list(1), ncol(oriented)-1))
  oriented[nrow(oriented) + 1,] <- c("maxs", rep(list(10), ncol(oriented)-1))
  
  process<- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  normalized2 <- slice(normalized, 1:(n() - 2))
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  # Not needed
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  normalized2$country<- rep(NA, nrow(normalized2))
  
  clean <- normalized2%>%
    mutate(country = case_when(is.na(country) ~ 
                                 deframe(tibble(cntry, nuts))[Country], 
                               TRUE ~ country))%>%
    select(country, everything())%>%
    select(-Country)
  
  clean2<- rename(clean, Country = country)%>%
    arrange(Country)
  
  return(clean2)
}
