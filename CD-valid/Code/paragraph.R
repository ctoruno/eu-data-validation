paragraph<- function(country){
  
  if (country == "Greece"){
    
    p<- c("We find that there are significant differences in all of the analyses of the Greece pretest data. In the T-test, we found large differences
          in all of the Pillars. However, this can be explained by the large difference between the comparison year and the current year, 2017. In terms
          of the Threshold analyses, we found that the red flags with large discrepancies consistently have negative differences in both the comparisons
          to other population polls and to expert surveys. In general, people are more negative in our survey. This trend could be caused in part by
          less than perfect matches between our questions and Third Party Source questions. We recommend to research if the Rule of Law has deteriorated
          substantially in Greece in the last few years concerning the flagged Pillars. The DAU point person for this anlaysis is Santiago Pardo.")
    
    return(gsub("[\r\n]", " ", p))
    
    
  }
  
  
}
