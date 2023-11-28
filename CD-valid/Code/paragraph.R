paragraph<- function(country){
  
  if (country == "Greece"){
    
    p<- c( 
    " <b>In general, we have found significant differences in Pillars 4, 5, 7, and 8.</b>
    <br>
    <br>   
    <ul> 
    
    <ul>    
    <li>Pillar 4:         
    <ul>            
    <li>We have identified large disparities when comparing the pretest data with our own 2017 data and the data from the TPS (Third-Party Source).</li>            
    <li>The most significant declines are observed in the areas of media freedom (q39b_G1, q39g_G2), freedom of speech within civil society organizations and political parties (q39h_G2, q39i_G2).</li>            
    <li>In most cases where differences are highlighted, our pretest data indicates more negative perceptions compared to other sources.</li>            
    <li>The TPS comparison data is over two years old, and there may have been developments in recent years that have affected the perception of fundamental freedoms in the country.</li>    
    <li>We found significant differences between the pretest data and TPS regarding the right to legal protection from an independent court (q59e). However, the TPS data is four years old. </li>        
    </ul>    
    </li>
    <br>
    <li>Pillar 5:         
    <ul>            
    <li>We have found significant differences in comparison to our 2017 data. However, this could be attributed to the time gap between the two data sets.</li>            
    <li>Please explore whether security conditions or perceptions in Greece have deteriorated in recent years.</li>        
    </ul>  
    </li>
    <br>
    <li>Pillars 7 and 8:         
    <ul>            
    <li>Substantial drops, particularly in subpillars related to the impartiality of judges (q44j_G2).</li>            
    <li>Therefore, it is necessary to analyze if any specific factors account for this decline in the perception of the judicial system's efficiency and impartiality.</li>            
    <li>This is especially crucial when we consider that this analysis reveals extremely divergent results compared to external data in subpillar 1.3 - Independent Oversight.</li>        
    </ul>    
    </li>
    </ul>
    <br>   
    It's worth emphasizing that people's perceptions consistently decrease negatively when compared to the data collected in 2017. 
    Additionally, there are substantial differences with other sources such as Freedom House. 
    It's important to keep in mind that we are highlighting the disparities we have found in our analysis, but there may be some changes that should be flagged even if they lack statistical significance. 
    Therefore, we recommend a comprehensive review of other pillars while considering the year, accuracy, and source of comparison.
    <br>  
    <br>   
    Finally, we suggest that the qualitative research should focus on the aforementioned aspects of the Rule of Law. We believe that other red flags found during the validation did not show a consistent pattern or the comparison points were not recent enough or sufficiently comparable to concern us.
    <br>  
    <br>   
    The designated point person for this analysis within the DAU is Santiago Pardo.
    <br>  
    <br>"
    )

    
  }
  
  if (country == "Cyprus"){
    
    p<- c("
    <b>In general, we did not find any significant issues during the data validation process.</b>
    <br>
    Some minor issues that we believe are worth noticing are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>We did not find any specific issue when comparing the pretest data to the previous GPP wave in Cyprus. </li>            
    <li>Since the comparison data is from only 2 years ago, the lack of significant differences is expected.</li>            
    <li>There is only one red flag in the t-test, regarding paying a bribe to expedite a process (q7e). However, this flag is raised because there is no variation among the pretest observations. </li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>We have found significant differences in comparison to third party public opinion polls. However, this could be attributed to a combination of low matches and/or old data (especially from 2019).</li>            
    <li>The most significant positive differences were found in subpillars related to independence of judges (q44j_G2) and freedom of media (q39b_G1). Although, the direction of the difference is not consistent among other related indicators. </li>        
    <li>Some negative differences were found in subpillars related to freedom of CSOs (q44j_G2) and consistently in the cost of justice (q44d_G2 and q44e_G2). We suggest further research regarding the access to justice in Cyprus. </li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>We found some significant differences in absence of authoritarianism (q53_G1). </li>            
    <li>There are also many flags in freedom of assembly (q39e_G1) and freedom of speech (q39e_G2). However, these come from differences with Freedom House, whose experts codings for Cyprus reflect a perfect score for the country.</li>            
    </ul>    
    </li>
    </ul>
    <br>   
          
          ")
    
  }
  
  return(HTML(p))
}

# <li>Regarding Pillar 4, we have identified large disparities when comparing the pretest data with our own 2017 data and the data from the TPS (Third-Party Source). 
# Notably, the most significant declines are observed in the areas of media freedom (q39b_G1, q39g_G2), freedom of speech within civil society organizations and political parties (q39h_G2, q39i_G2). 
# It is essential to note that, in most cases where differences are highlighted, our pretest data indicates more negative perceptions compared to other sources. 
# Also, please consider that the comparison data is over two years old and there may have been developments in recent years that have affected the perception of fundamental freedoms in the country.
# Therefore, it's important to explain what these changes are due to. </li>
#     <br>   
#     <br>   
#     <li>For Pillar 5, we have found significant differences in comparison to our 2017 data. 
#     However, this could be attributed to the time gap between the two data sets. 
#     Please, explore whether security conditions or perceptions in Greece have deteriorated in recent years. </li>
#     <br>   
#     <br>   
#     <li> Pillars 7 and 8 have shown substantial drops, particularly in subpillars related to the impartiality of judges (q44j_G2). 
#     Therefore, it is necessary to analyze if any specific factors account for this decline in the perception of the judicial system's efficiency and impartiality. 
# This is especially crucial when we consider that this analysis reveals extremely divergent results compared to external data in subpillar 1.3 - Independent Oversight. </li>
#   </ul>

#gsub("[\r\n]", " ", p)