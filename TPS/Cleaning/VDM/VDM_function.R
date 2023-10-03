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
## This version:      October 3rd, 2023
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
  
  targetvars <- c("country_name", "v2x_liberal", "v2xlg_legcon", "v2psoppaut", "v2cltrnslw", "v2lgqstexp", 
                  "v2lginvstp", "v2lgoppart", "v2x_jucon", "v2juhcind", "v2juncind", "v2lgotovst", 
                  "v2x_polyarchy", "v2x_suffr", "v2xel_frefair", "v2x_elecoff", "v2xdd_dd", "v2elembaut", 
                  "v2elembcap", "v2elrgstry", "v2elvotbuy", "v2elirreg", "v2elintim", "v2elpeace", "v2x_libdem", 
                  "v2x_freexp_altinf", "v2x_frassoc_thick", "v2merange", "v2meslfcen", "v2mecrit", "v2cldiscm", 
                  "v2cldiscw", "v2psbars", "v2cscnsult", "v2exrescon", "v2jucomp", "v2juhccomp", "v2mecenefm", 
                  "v2meharjrn", "v2cseeorgs", "v2csreprss")

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

  mm <- dfv[c(4:8, 10:12, 18:24, 28:41)]
  s  <- dfv[c(1:3, 9, 13:17, 25:27)]

  process    <- preProcess(mm, method = c("range"))
  normalized <- predict(process, mm)
  

  dfnorm <- cbind(s, normalized )

  df2 <- dfnorm %>%
    select(all_of(targetvars))
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate <- df2%>%
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
  select(-country_name)

  return(clean)

}
