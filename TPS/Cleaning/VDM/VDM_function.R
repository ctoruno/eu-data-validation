## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - VDM Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##                    Carlos Toruno   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 28th, 2023
##
## This version:      October 25th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


VDM_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  p1 <- c("country_name", "v2x_liberal", "v2xlg_legcon", "v2psoppaut", "v2cltrnslw", "v2lgqstexp", 
                  "v2lginvstp", "v2lgoppart", "v2x_jucon", "v2juhcind", "v2juncind", "v2lgotovst", 
                  "v2x_polyarchy", "v2x_suffr", "v2xel_frefair", "v2x_elecoff", "v2xdd_dd", "v2elembaut", 
                  "v2elembcap", "v2elrgstry", "v2elvotbuy", "v2elirreg", "v2elintim", "v2elpeace", "v2x_libdem", 
                  "v2x_freexp_altinf", "v2x_frassoc_thick", "v2merange", "v2meslfcen", "v2mecrit", "v2cldiscm", 
                  "v2cldiscw", "v2psbars", "v2cscnsult", "v2exrescon", "v2jucomp", "v2juhccomp", "v2mecenefm", 
                  "v2meharjrn", "v2cseeorgs", "v2csreprss")
  
  p4<- c("v2cltort", "v2clkill", "v2clslavem", "v2clslavef", "v2clprptym", "v2clprptyw", "v2clrelig", "v2xcl_rol", 
         "v2csgender", "v2clacjust", "v2clsocgrp", "v2pepwrgen", "v2pepwrses", "v2pepwrsoc", "v2clfmove", 
         "v2cldmovem","v2cldmovew")
  
  p7<- c("v2clacjstm", "v2clacjstw")
  
  val<- c("v2caassemb", "v2jupoatck", "v2exbribe", "v2lgcrrpt", "v2exembez", "v2elffelr")
  
  targetvars <- c(p1, p4, p7, val)

  cntry<- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", 
            "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", 
            "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")

  ## 1.2 Sub-setting data=======================================================================================

  dfv <- df%>%
    select(all_of(targetvars))%>%
    filter(country_name %in% cntry)

  ## 1.3 Re-orient indicators ==================================================================================

  # Not needed

  ## 1.4 Normalize indicators ==================================================================================

  changescale<- c("v2psoppaut", "v2cltrnslw", "v2lgqstexp","v2lginvstp", "v2lgoppart", "v2juhcind", "v2juncind", 
                  "v2lgotovst", "v2elembaut", "v2elembcap", "v2elrgstry", "v2elvotbuy", "v2elirreg", "v2elintim", 
                  "v2elpeace", "v2merange", "v2meslfcen", "v2mecrit", "v2cldiscm", "v2cldiscw", "v2psbars",
                  "v2cscnsult", "v2exrescon", "v2jucomp", "v2juhccomp", "v2mecenefm", "v2meharjrn", "v2cseeorgs",
                  "v2csreprss", "v2cltort", "v2clkill", "v2clslavem", "v2clslavef", "v2clprptym", "v2clprptyw", 
                  "v2clrelig", "v2csgender", "v2clacjust", "v2clsocgrp", "v2pepwrgen", "v2pepwrses", "v2pepwrsoc", 
                  "v2clfmove", "v2cldmovem","v2cldmovew", "v2clacjstm", "v2clacjstw", "v2caassemb", "v2jupoatck", 
                  "v2exbribe", "v2lgcrrpt", "v2exembez", "v2elffelr")
  
  nochange<- setdiff(targetvars, changescale)
  
  df2a <- dfv%>%
    select(all_of(changescale))
  df2b <- dfv%>%
    select(all_of(nochange))

  process    <- preProcess(df2a, method = c("range"))
  normalized <- predict(process, df2a)
  
  dfnorm <- cbind(df2b, normalized)

  df3<- dfnorm %>%
    select(all_of(targetvars))
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate <- df3%>%
    group_by(country_name) %>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  aggregate$Country <- rep(NA, nrow(aggregate))
  
  nuts <- c("AT", "BE", "BU", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
          "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
          "RO", "SE", "SI", "SK")

  clean <- aggregate %>%
    mutate(Country = case_when(is.na(Country) ~ 
                               deframe(tibble(cntry, nuts))[country_name], 
                             TRUE ~ Country)) %>%
  select(Country, everything()) %>%
  select(-country_name)%>%
    arrange(Country)

  return(clean)

}
