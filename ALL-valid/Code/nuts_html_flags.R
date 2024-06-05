nuts_html_flags<- function(){
  
  
  subp<- metadata%>%
    select(GPP_Variable_Name = DAU_GPP, Question, subpillar = sub_pillar) %>%
    arrange(subpillar) %>%
    left_join(QRQ_description %>% select(pillar, pillar_name, pillar_id, subpillar, subpillar_name),
              by = "subpillar") %>%
    distinct() %>%
    select(GPP_Variable_Name, Sub_Pillar = subpillar, Pillar = pillar)%>%
    rbind(variable_list.df%>%
            select(variable, subpillar, pillar)%>%
            rename("GPP_Variable_Name" = "variable", 
                   "Sub_Pillar" = "subpillar",
                   "Pillar" = "pillar"))%>%
    distinct()
  
  df<- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(df)<- c("Country", "NUTS", "GPP_Variable_Name", "Sub_Pillar", "Pillar")
  for (i in 1:length(countrylist)){
    
    par<- paragraph(countrylist[i], type = "full")
    spmatches <- str_extract_all(par, "[0-9]\\.[0-9]{1,2}:")
    spmatches <- unlist(spmatches)
    
    flags<- import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", 
                               countrylist[i], 
                               "/", 
                               countrylist[i], 
                               ".xlsx"))
    
    tm<- flags$time_nuts
    tps<- flags$tps_nuts
    
    if (length(spmatches) >0){
      tpssum <- tps[grepl(paste(spmatches, collapse="|"), tps$`Sub_Pillar`), ]
      tpssum <- tpssum%>%
        filter(Flag== "red")%>%
        select(Country, NUTS, GPP_Variable_Name)
      ttsum  <- tm[grepl(paste(spmatches, collapse="|"), tm$`subpillar`), ]
      ttsum  <- ttsum%>%
        filter(warning == "Red light")%>%
        select(country, nuts, variable)%>%
        rename(GPP_Variable_Name = variable,
               NUTS = nuts,
               Country = country)
      
      vars<- rbind(tpssum, ttsum)%>%
        left_join(subp, by = join_by(GPP_Variable_Name))%>%
        distinct()
      
      df<- rbind(df, vars)
    }
  }
  return(df)
}
