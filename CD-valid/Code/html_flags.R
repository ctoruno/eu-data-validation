html_flags<- function(){
  
  
  subp<- metadata%>%
    select(GPP_Variable_Name, Sub_Pillar, Pillar)%>%
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
    
    if (length(spmatches) >0){
    vars<- subp[grepl(paste(spmatches, collapse="|"), subp$Sub_Pillar), ]
    vars2<- vars%>%
      mutate(Country = countrylist[i])%>%
      select(Country, everything())
    
    df<- rbind(df, vars2)
    }
  }
  return(df)
}
