difficulty_score<- function(data.df = fullmerge){

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 1.  Create Difficulty Score                                                                 ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  data.df<- data.df%>%
    filter(method == "Face-to-Face")
# Recode qpil variable
  data.df$qpi1_norm <- ifelse(data.df$qpi1 == 2, 0, data.df$qpi1)

# Loop through variables qpi2a to qpi2f
for (x in c("qpi2a", "qpi2b", "qpi2c", "qpi2d", "qpi2e", "qpi2f")) {
  # Create a new variable with suffix '_norm'
  data.df[[paste0(x, "_norm")]] <- ifelse(data.df[[x]] < 98, (data.df[[x]] - 1) / 2, data.df[[x]])
}

  difficulty_score <- rowMeans(data.df[, c("qpi1_norm", "qpi2a_norm", "qpi2b_norm", "qpi2c_norm", "qpi2d_norm", "qpi2e_norm", "qpi2f_norm")])
  
  data.df$difficulty_score<- difficulty_score

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 2.  Generate Tables                                                                        ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  socio<- data.frame(matrix(nrow=0, ncol=3))
  colnames(socio)<- c("Country", "Group", "Difficulty_Score") 
  
  for (i in unique(data.df$country_name_ltn)){
    
    df1<-data.df%>%
      filter(country_name_ltn == i)
    
    for (k in c("gend", 'income_quintile', 'urban')){
      
      for (j in unique(df1[[k]])){
        
        df<- df1%>%
          filter(!!sym(k) == j)
        
        socio[nrow(socio)+1,]<- c(i, paste0(k, j), mean(df$difficulty_score))
        
      }
    }
  }
  
  cntry<- data.df%>%
    group_by(country_name_ltn)%>%
    summarise("Difficulty Score" = mean(difficulty_score))%>%
    arrange(desc(`Difficulty Score`))
  
  nuts<- data.df%>%
    group_by(nuts_id)%>%
    summarise("Difficulty Score" = mean(difficulty_score))%>%
    arrange(desc(`Difficulty Score`))
  
  
  return(list("country" = cntry, "nuts" = nuts, "sociodem"= socio))

}
