sociodem_check <- function(){
  
  EU_GPP <- read_dta("EU-GPP_test - renamed.dta") %>%
    mutate(
      generation               =  if_else(age < 30 & age > 17, "Less than 30 years", 
                                          if_else(age > 29, "More than 30 years", NA_character_)),
      economic_status          =  if_else(fin == 1 | fin == 2, "Poor",
                                          if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
      gender                   =  if_else(gend == 1, "Male", "Female", NA_character_),
      education                =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                          if_else(edu < 4, "No High Education Level", NA_character_))
    )
  
  data <- EU_GPP
  
  prueba <- imap(c(gender          = "gender",
                   generation      = "generation",
                   economic_status = "economic_status",
                   education       = 'education'),
                 
                 function(sociodem, category){
                   
                   data2table <- data %>%
                     rename(socio = all_of(sociodem))
                   
                   sociodem <- data2table %>%
                     mutate(counter = 1,
                            total = n()) %>%
                     select(socio, counter, total) %>%
                     group_by(socio) %>%
                     summarise(total = sum(counter/total)*100) %>%
                     pivot_wider(names_from = "socio", values_from = "total") %>%
                     rename(var1 = 1, var2 = 2) %>%
                     mutate(difference = abs(var1-var2)) %>%
                     mutate(flag = if_else(difference > 15 & difference < 25, "Big Difference",
                                           if_else(difference > 25, "Huge Difference", "No flag"))) %>%
                     mutate(category = category) %>%
                     select(category, var1, var2, difference, flag)
                   
                   return(sociodem)
                   
                 })
  
  final <- do.call(rbind.data.frame, prueba)
  
  
  return(final)
  
  
}

a <- openxlsx::write.xlsx(sociodem_data, "prueba.xlsx")

conditionalFormatting(
  a,
  cols = 4,
  rule = NULL,
  style = NULL,
  type = "expression",
  ...
)
