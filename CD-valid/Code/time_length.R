time_length <- function(fullf = fullmerge) {
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Select data to analyze                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  socio<- data.frame(matrix(nrow=0, ncol=5))
  colnames(socio)<- c("Country", "Group", "Average_Time", "Time_With_A2J", "Time_No_A2J") 

  for (i in unique(fullf$country_name_ltn)){
    
    df1<-fullf%>%
      filter(country_name_ltn == i)
    
    for (k in c("gend", 'income_quintile', 'urban')){
    
      for (j in unique(df1[[k]])){
      
        df<- df1%>%
          filter(!!sym(k) == j)
      
        dfa2j<- df%>%
          filter(!is.na(avg_time_withA2J))
      
        dfno<- df%>%
          filter(is.na(avg_time_withA2J))
      
        socio[nrow(socio)+1,]<- c(i, paste0(k, j), mean(df$interview_length), mean(dfa2j$interview_length), mean(dfno$interview_length))
        
      }
    }
  }
  
  cntry<- fullf%>%
    group_by(country_name_ltn)%>%
    summarise("Average_Time" = mean(interview_length), "Time_With_A2J" = mean(avg_time_withA2J, na.rm=TRUE), "Time_No_A2J" = mean(avg_time_noA2J, na.rm =TRUE))%>%
    arrange(desc(Average_Time))
  
  nuts<- fullf%>%
    group_by(nuts_id)%>%
    summarise("Average_Time" = mean(interview_length), "Time_With_A2J" = mean(avg_time_withA2J, na.rm=TRUE), "Time_No_A2J" = mean(avg_time_noA2J, na.rm =TRUE))%>%
    arrange(desc(Average_Time))
  
    
  return(list("country" = cntry, "nuts" = nuts, "sociodem"= socio))
}
  