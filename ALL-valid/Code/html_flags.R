html_flags<- function(){
  
  
  subp<- metadata%>%
    select(GPP_Variable_Name = DAU_GPP, Question, subpillar = sub_pillar) %>%
    arrange(subpillar) %>%
    left_join(QRQ_description %>% select(pillar = `Pillar score`, 
                                         pillar_name = Pillar, 
                                         subpillar = `Sub-pillar score`, 
                                         subpillar_name = `Sub-pillar` ),
              by = "subpillar") %>%
    distinct() %>%
    select(GPP_Variable_Name, Sub_Pillar = subpillar, Pillar = pillar)%>%
    rbind(variable_list.df%>%
            select(variable, subpillar, pillar)%>%
            rename("GPP_Variable_Name" = "variable", 
                   "Sub_Pillar" = "subpillar",
                   "Pillar" = "pillar"))%>%
    distinct()
  
  df<- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df)<- c("Country", "GPP_Variable_Name", "Sub_Pillar", "Pillar")
  for (i in 1:length(countrylist)){
    
    par<- paragraph(countrylist[i], type = "full")
    spmatches <- str_extract_all(par, "[0-9]\\.[0-9]{1,2}:")
    spmatches <- unlist(spmatches)
    
    flags<- import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", 
                            countrylist[i], 
                            "/", 
                            countrylist[i], 
                            ".xlsx"))
    
    tm<- flags$time_changes
    tps<- flags$tps_comparisson
    
    if (length(spmatches) >0){
      tpssum <- tps[grepl(paste(spmatches, collapse="|"), tps$`Sub_Pillar`), ]
      tpssum <- tpssum%>%
        filter(Flag== "red")%>%
        select(Country, GPP_Variable_Name)
      ttsum  <- tm[grepl(paste(spmatches, collapse="|"), tm$`subpillar`), ]
      ttsum  <- ttsum%>%
        filter(warning == "Red light")%>%
        select(country, variable)%>%
        rename(GPP_Variable_Name = variable,
               Country = country)
      
      vars<- rbind(tpssum, ttsum)%>%
        left_join(subp, by = join_by(GPP_Variable_Name))%>%
        distinct()
    
    df<- rbind(df, vars)
    }
  }
  return(df)
}
