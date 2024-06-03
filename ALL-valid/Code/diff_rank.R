diff_rank<- function(data, type){
  
  if (type == "TPS"){
    df<- data%>%
      group_by(country_name_ltn, Type_Survey)%>%
      mutate(Med_diff = median(Diff_Rank),
             iqr = IQR(Diff_Rank, na.rm = TRUE),
             q1 = quantile(Diff_Rank, prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]],
             q3 = quantile(Diff_Rank, prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]],
             lower_bound = q1 - (1.5*iqr),
             upper_bound = q3 + (1.5*iqr))%>%
      ungroup()%>%
      mutate(flagged_questions = ifelse(Diff_Rank>upper_bound, "Red", "Green"))
  }
  else{
    df<- data%>%
      group_by(country_name_ltn)%>%
      mutate(Med_diff = median(Diff_Rank),
             iqr = IQR(Diff_Rank, na.rm = TRUE),
             q1 = quantile(Diff_Rank, prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]],
             q3 = quantile(Diff_Rank, prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]],
             lower_bound = q1 - (1.5*iqr),
             upper_bound = q3 + (1.5*iqr))%>%
      ungroup()%>%
      mutate(flagged_questions = ifelse(Diff_Rank>upper_bound, "Red", "Green"))
    
  }
    
  return(df)
}
