paragraph<- function(country){
  
  if (country == "Greece"){
    
    p<- c( 
    "
    In general, we have found significant differences in Pillars 4, 5, 7, and 8.
    //
    Regarding Pillar 4, we have identified large disparities when comparing the pretest data with our own 2017 data and the data from the TPS (Third-Party Source). 
    Notably, the most significant declines are observed in the areas of media freedom (q39b_G1, q39g_G2), freedom of speech within civil society organizations and political parties (q39h_G2, q39i_G2). 
    It is essential to note that, in most cases where differences are highlighted, our pretest data indicates more negative perceptions compared to other sources. 
    Also, please consider that the comparison data is over two years old and there may have been developments in recent years that have affected the perception of fundamental freedoms in the country.
    Therefore, it's important to explain what these changes are due to.
    //
    For Pillar 5, we have found significant differences in comparison to our 2017 data. 
    However, this could be attributed to the time gap between the two data sets. 
    Please, explore whether security conditions or perceptions in Greece have deteriorated in recent years. 
    //
    Pillars 7 and 8 have shown substantial drops, particularly in subpillars related to the impartiality of judges (q44j_G2). 
    Therefore, it is necessary to analyze if any specific factors account for this decline in the perception of the judicial system's efficiency and impartiality. 
    This is especially crucial when we consider that this analysis reveals extremely divergent results compared to external data in subpillar 1.3 - Independent Oversight.
    //
    It's worth emphasizing that people's perceptions consistently decrease negatively when compared to the data collected in 2017. 
    Additionally, there are substantial differences with other sources such as Freedom House and the Eurobarometer. 
    It's important to keep in mind that we are highlighting the disparities we have found in our analysis, but there may be some changes that should be flagged even if they lack statistical significance. 
    Therefore, we recommend a comprehensive review of other pillars while considering the year, accuracy, and source of comparison.
    //
    Finally, we recommend to research if the Rule of Law has deteriorated substantially in Greece in the last few years concerning the flagged Pillars. 
    //
    In conclusion, we strongly recommend conducting in-depth research to determine whether the Rule of Law has significantly deteriorated in Greece, particularly concerning the flagged Pillars. 
    The designated point person for this analysis within the DAU is Santiago Pardo.
   "
    )
    
    return(gsub("[\r\n]", " ", p))
    
    
  }
  
  
}
