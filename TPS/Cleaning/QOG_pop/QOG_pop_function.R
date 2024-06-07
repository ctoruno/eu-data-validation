## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation - QOG Function
##
## Author:            Dalia Habiby    (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 3rd, 2024
##
## This version:      May 6th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

QOG_pop_clean <- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  targetvars <- c("country", "q14", "q15", "q16",
                 "q17_1", "q17_2", "q18_1", "q18_2", "q18_3", "q18_4")
  #"corruptionp", "corruption_subPer", "corruption_subExp" , "q19_1", "q19_2", "q19_3", "q19_4"

  # targetvars <- c("cname", "proff2", "proff3", "impar1")
  
  cntry <- c("1","2","3","5","6","11","7", "8", "12", "26", "9", "10", "4", "13", "14", "15", "17", "18", "16", "19",
            "20", "21", "22", "23", "27", "25", "24")

  # cntry <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", 
  #            "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy",  "Lithuania", "Luxembourg",
  #            "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden",
  #            "Slovenia", "Slovakia")
  # 
  
  ## 1.2 Sub-setting data========================================================================================
  
  df2 <- df%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ===================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the below vector to reorient (ro)
  
  ro<- c("q14", "q15", "q16", "q17_1", "q17_2")

  for(i in ro){

    oriented[[i]]<- ifelse(oriented[[i]] == 1, 10, ifelse(oriented[[i]] == 2, 9,
                                  ifelse(oriented[[i]] == 3, 8, ifelse(oriented[[i]] == 4, 7,
                                  ifelse(oriented[[i]] == 5, 6, ifelse(oriented[[i]] == 6, 5,
                                  ifelse(oriented[[i]] == 7, 4, ifelse(oriented[[i]] == 8, 3,
                                  ifelse(oriented[[i]] == 9, 2, ifelse(oriented[[i]] == 10, 1, NA_real_))))))))))
  }

  no<- setdiff(targetvars[-1], ro)

  for(i in no){

    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1, 2), oriented[[i]], NA_real_)

  }
  
  ## 1.4 Normalize indicators ===================================================================================
  
  oriented$country<- as.character(oriented$country)
  
  process <- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  
  ## 1.5 Aggregate indicators at the country level ===============================================================
  
  aggregate<- normalized%>%
    filter(country %in% cntry)%>%
    group_by(country)%>%
    summarise_at(targetvars[-1], mean, na.rm= TRUE)%>%
    arrange(country)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                         ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  aggregate$Country<- rep(NA, nrow(aggregate))
  
  clean <- aggregate%>%
    mutate(Country = case_when(is.na(Country) ~ 
                                 deframe(tibble(cntry, nuts))[country], 
                               TRUE ~ Country))%>%
    select(Country, everything())%>%
    select(-country)
  
  return(clean)
}
