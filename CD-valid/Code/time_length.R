time_length <- function(data.df = fullmerge, 
                         country = args[1]) {
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Select data to analyze                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  fullf<- fullmerge%>%
    filter(country_name_ltn == country)
  
  
  final<- data.frame(matrix(nrow=0, ncol=4))
  colnames(final)<- c("Group", "Average_Time", "Time_With_A2J", "Time_No_A2J") 
  final[nrow(final)+1,]<- c(country, mean(fullf$interview_length), mean(fullf$avg_time_withA2J, na.rm = TRUE), mean(fullf$avg_time_noA2J, na.rm = TRUE))
  
  for (i in c("gend", 'income_quintile', 'urban')){
    
    for (j in unique(fullf[[i]])){
      
      df<- fullf%>%
        filter(!!sym(i) == j)
      
      dfa2j<- df%>%
        filter(!is.na(avg_time_withA2J))
      
      dfno<- df%>%
        filter(is.na(avg_time_withA2J))
      
      final[nrow(final)+1,]<- c(paste0(i, j), mean(df$interview_length), mean(dfa2j$interview_length), mean(dfno$interview_length))
      
    }
  }
    
  return(final)
  
  
}
  