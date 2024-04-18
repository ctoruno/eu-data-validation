## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Paragraph                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

paragraph<- function(country, type){
  
  if (country == "Austria"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Austria                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found some issues in all of the analyses.</b>
    <br>
    Some issues that we believe are worth noting are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>There are some differences between the 2017 data and the current pretest data. However, the previous round is very old, and these differences could be fine. </li> 
    <li>However, check Pillar 1, 3, and 4 and focus on the questions with red flags.</li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Some of the Public Opinion Surveys have higher scores than the GPP data. Check Pillars 3, 4, and 7 and focus on the questions with red and yellow flags.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>The expert data shows significant differences from the pretest data. Most of the expert data points have a medium-low match with the GPP questions. </li>  
    <li>However, please check the questions that have red and medium coloring and have medium-high matches in Pillars 1 and 4.</li>
    </ul>    
    </li>
    </ul>
    <br>   
    The DAU point person for this report is Natalia Rodriguez.
    <br>
    <br>")
    
  }
  
  if (country == "Belgium"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Belgium                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found some issues in all of the analyses.</b>
    <br>
    Some issues that we believe are worth noting are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>Most of the changes over time with the previous round of the GPP (2018) are negative. Pay special attention to the red flags in Pillars 1, 3, and 4. </li> 
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Review the questions with yellow/red coloring in pillars P3, P4 and P7, with special focus on the high matching TPS.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>There are significant differences between the expert data and the pretest data. However, most of the expert data has a low match with the GPP. </li>  
    <li>Please review P1 and P4 and focus mainly on the red flags and high matching TPS.</li>
    </ul>    
    </li>
    </ul>
    <br>   
    The DAU point person for this report is Natalia Rodriguez.
    <br>
    <br>")
    
  }
  
  if (country == "Bulgaria"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Bulgaria                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (type == "pretest"){ 
      p<- c("<b>In general, we found no significant issues.</b>
    <br>
    Some issues that we believe are worth noting are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>Despite the comparative data being old, distinguishable changes over time were not identified. Consequently, we recommend directing the research focus toward aspects anticipated to have evolved over the years, which are not listed below.</li> 
    <li>Significant decreases were observed in Media Freedom (q39b_G1), Accountability (q57_G1 and q57G2), Corruption in Parliament (q3a) and Corruption in National Government (q3b).</li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy differences were uncovered, these could potentially be attributed to outdated data.</li>          
    <li>The pretest data revealed a more favorable score than the TPS indicator in relation to CSO Freedom of Opinion (q39h_G2).</li>
    <li>Contrarily, the pretest data was consistently lower than the TPS indicators concerning Right to Protection in Court (q59e), Deficiency of Courts (44a_G2, 44d_G2, 44e_G2), and the Performance of Prosecutors (q43f_G2).</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>In general, our pretest scores are lower than TPS expert scores across all flag colors. </li>  
    <li>We found consistently lower scores in terms of the Independence of the Judiciary (q44j_G2 and q44i_G2) and Absence of Authoritarianism (q52_G2, q53_G1,and q56_G1). However, many of these may be caused by low question match or perfect expert scores.</li>          
    <li>Although, there is a positive change regarding Equality (q58a).</li>    
    <li> Since these disparities don't correspond with consistently similar results from other tests, concerns regarding their validity are mitigated. </li>
    </ul>    
    </li>
    </ul>
    <br>   
    The DAU point person for this report is Carlos Toruño.
    <br>
    <br>
 ")
    } else {
      p<- c("
            <b>Insights summary</b>
            <br>
            We are seeing that across time, a few pillars are indicating changes. Most only have one or two indicators, however pillar 2 has 16 comparisons. Although many of these have red flags, some are positive changes while others are negative changes and overall encompass experience and opinion questions. In comparison to the TPS public opinion polls, we only have 2 red flags. This indicates that our data is in accordance with other population surveys, which is one of the most important goals. In terms of the TPS expert surveys, there are a few pillars flagged, however we are conscious that we trust these scores less. 
            <br>
            <br>
            Given that we expect to see large changes from our previous data in Bulgaria, we are noting sub pillars that are flagged in both the GPP and TPS analyses. In this regard, all the topics that are flagged in the time comparison but supported by green flags in the TPS are considered as something normal in the context of Bulgaria Therefore, what we are highlighting are the discrepancies found in the data in two aspects: the ones that are consistent in both analyses, and also the ones that are not supported by another analysis.
            <br>
            <br>
            <b> Sub Pillars to Research </b>
            <ul>
            <li> Pillar 5. Security
            <ul>
            <li> Positive trend in 5.1: People feel safe</li>
            <ul>
            <li> When we asked how safe people feel walking in their neighborhood at night, we found a score of 0.664 when our previous score was 0.52. Although the difference between scores is not very large, the t-test indicates that overall, individuals are answering more positively than before. 
            </li>
            </ul>
            <li> Positive trend in 5.2: Absence of crime and violence </li>
            <ul>
            <li> When we asked if people were aware of organized crime occuring in their neighborhoods, we found a score of 0.935, while the average criminality score in the Organized Crime Index was 0.483. Although the comparison is from a low match, the concepts are still related and therefore we should give context about organized crime rates. </li>
            </ul>
            </ul>
            </li>
            </ul>
            All these discrepancies between the data are also consistent at the NUTS level; we observe that the significant differences persist across all NUTS regions for all sub-pillars flagged.
            <br>
            <br>
            The topic areas highlighted above are what our data is telling us through the given analyses. However, it is still important to take into account the media reports and other qualitative background research to potentially identify any other sub-pillar that should be researched more thoroughly. 
            ")
    }
    
  }

  if (country == "Croatia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Croatia                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found significant differences from the TPS Expert Data.</b>
    <br>
    There are some issues that are worth noting:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>The pretest data is generally more negative compared to the previous round (2018). Please review Pillars 1, 3, and 4. </li>
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:  
     <ul>            
    <li>Some of the Public Opinion Surveys have higher scores than the GPP data. Check Pillars 4, 7, and 8, and focus on the questions with red and yellow flags.</li>            
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:  
    <ul>            
    <li>The expert data shows significant differences from the pretest data. The expert data has lower scores compared to the GPP. </li>
    <li>Please check the red flags in Pillars 1, 3, 4, and 7, but focus on the questions with a higher match level (medium/high). </li>            
    </ul>    
    </li>
    </ul>
    <br> 
    The designated point person for this analysis within the DAU is Natalia Rodriguez.
    <br>  
    <br>
   
          "
      )
    
  }
  
  if (country == "Cyprus"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Cyprus                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (type == "pretest"){
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
    <li>In general, our pretest scores are lower than TPS expert scores across all flag colors. </li>  
    <li>We found some significant differences in absence of authoritarianism (q52_G2, q53_G1,and q56_G1) and Independence of the Judiciary (q44j_G2 and q44i_G2). </li> 
    <li>There are also many flags in freedom of assembly (q39e_G1) and freedom of speech (q39e_G2). However, these come from differences with Freedom House, whose experts codings for Cyprus reflect a perfect score for the country.</li>     
    <li> Since these disparities don't correspond with consistently similar results from other tests, concerns regarding their validity are mitigated. </li>
    </ul>    
    </li>
    </ul>
    <br>   
          
          ")
    } else {
      p<- c("
            <b>Insights summary</b>
            <br>
            We are seeing that across time, a few pillars are indicating changes. Most only have a few indicators, however pillar 2 has 16 comparisons. Although many of these have red flags, some are positive changes while others are negative changes and overall encompass experience and opinion questions. In comparison to the TPS public opinion polls, we only have 3 red flags. This indicates that our data is in accordance with other population surveys, which is one of the most important goals. In terms of the TPS expert surveys, we are mostly highlighting Pillar 8, however we are conscious that we trust these scores less.  Certainly, the primary source disparity is with Experts sources like Freedom in the World and V-Dem, where certain variables yield significantly high scores. In such instances, we advise prioritizing a discussion on the validity of our scores rather than clarifying disparities between the sources.
            <br>
            <br>
            Given that we do not expect to see large changes from our previous data in Cyprus, we are noting sub pillars that are flagged in both the GPP and TPS analyses. In this regard, all the topics that are flagged in the time comparison but supported by green flags in the TPS are considered as something normal in the context of Cyprus. Therefore, what we are highlighting are the discrepancies found in the data in two aspects: the ones that are consistent in both analyses, and also the ones that are not supported by another analysis.
            <br>
            <br>
            <b> Sub Pillars to Research </b>
            <ul>
            <li> Pillar 8. Criminal Justice
            <ul>
            <li> Negative trend in 8.6: Due process of law </li>
            <ul>
            <li> When we asked how confident people are that the criminal justice system guarantees a fair trial of all accused people, we found a score of 0.434, while Freedom in the World found a score of 1 when they asked if due process prevails in civil and criminal matters. Furthermore, when we asked if people thought that the criminal justice system treats those accused as innocent until proven guilty, we found a score of 0.481 while Freedom in the World assigned a score of 1 for the question regarding if due process prevails in civil and criminal matters. Although these questions have low matches, they are still related and should be complemented with research about the rights of the accused and due process.</li>
            </ul>
            <li> Negative trend in 8.7: Prisons</li>
            <ul>
            <li> When we asked how confident people are that the criminal justice system guarantees the safety and human rights of people deprived of their liberty, we found a score of 0.45, while V-Dem found a score of 0.838 when they asked if there was freedom from torture. Although the questions have a low match, they are still related and should be complemented with research about safety and human rights in the criminal justice system. 
            </li>
            </ul>
            </ul>
            </li>
            </ul>
            The topic areas highlighted above are what our data is telling us through the given analyses. However, it is still important to take into account the media reports and other qualitative background research to potentially identify any other sub-pillar that should be researched more thoroughly. 
            ")
    }
    
  }
  
  if (country == "Czechia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Czechia                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with comparisons regarding Civic Participation and Justice.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul> 
    <li>GPP Over Time:         
    <ul>            
    <li>We found some instances where our pretest data is lower than the previous GPP in Civic Participation (q39b_G1, and q39d_G1) which are consistent with the other analyses. </li>            
    </ul>  
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is higher than the Public Opinion TPS comparison indicators in relation to Civic Participation (q39d_G1) and consistently lower Justice (q59e, q44d_G2, q44e_G2, q44f_G1, q44g_G1, and q43f_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators, specifically related to Justice in Pillars 1, 3, 4, 7, and 8. We suggest futher research into these areas. </li>            
    <li> Many of these comparisons involved very high scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Czechia so highly. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "Denmark"){
   
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Denmark                                                                                      ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found some issues in all of the analyses.</b>
    <br>
    Some issues that we believe are worth noting are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>No further checks needed. </li> 
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>The public opinion data for Denmark is more positive compared to the pretest data, particularly in P1, P4, and P7.</li>
    <li>Please complement the analysis with qualitative research to assess if the pretest data is too negative.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>The expert data shows several red flags, particularly in P1, P2, and P7. The expert data is significantly more positive than the GPP. </li>  
    <li>However, check the pillars mentioned above and complement the analysis with qualitative research to assess if the pretest data is too negative. </li>
    </ul>    
    </li>
    </ul>
    <br>   
    The DAU point person for this report is Natalia Rodriguez.
    <br>
    <br>")
    
  }
  
  if (country == "Estonia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Estonia                                                                                     ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is consistently lower than the Public Opinion TPS comparison indicators in relation to Justice (q44a_g2, q44d_G2, q44e_G2, and q44h_G1).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators in Pillar 1. We suggest futher research into the relevant topic areas. </li>            
    <li> Many of these comparisons involved near-perfect scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Estonia so highly. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "Finland"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Finland                                                                                      ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("<b>In general, we only found issues with the TPS Expert Data</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores regarding Absence of Authoritarianism (q52_G2 and q53_G2), Constraints (q39e_G2 and q44i_G2), and Free and Fair Elections (q39c_G1). We suggest futher research into these topic areas. </li>            
    <li> Some of these comparisons could be attributed to low question match. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "France"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## France                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is consistently different from the Public Opinion TPS comparison indicators in relation to Justice (q44a_g2, q44d_G2, q44e_G2, and q44f_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators in Pillar 1. We suggest futher research into the relevant topic areas. </li>            
    <li> Some of these comparisons may be influenced by low question match. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "Germany"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Germany                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found some issues in all of the analyses.</b>
    <br>
    Some issues that we believe are worth noting are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>No further checks needed. </li> 
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>The civic participation questions in Pillar 3 are consistently more negative than the public opinion TPS. </li>
    <li>Please review these questions and the justice questions in Pillar 7.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>The pretest data is consistently more negative compared to the expert data.  </li>  
    <li>Pay special attention to the red and yellow flags for questions with high-match level in pillars 1, 2, 4, 7, and 8. </li>
    </ul>    
    </li>
    </ul>
    <br>   
    The DAU point person for this report is Natalia Rodriguez.
    <br>
    <br>")
    
  }
  
  
  if (country == "Greece"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Greece                                                                                ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c( 
    " <b>In general, we have found significant differences in Pillars 4, 5, 7, and 8.  Overall, almost all of the comparisons with expert sources are significant and negative.</b>
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
  
  if (country == "Hungary"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Hungary                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (type == "pretest"){
      p<- c("
    <b>Overall, we encountered no significant issues during the data validation process. We are aware that numerous flags appeared in the GPP validation over time, but this can be attributed to the time gap between comparisons. Additionally, these differences do not align with TPS public opinions, which are remarkably similar. As a result, while these discrepancies are intriguing and warrant understanding, they are not substantial enough to cast doubt on the polling company.</b>
    <br>
    <br>
    Some issues that we believe are worth noticing are:
    <br>  
    <br>
    <ul>    
    <li><b>GPP Over Time:</b>
    <br>
    <br>
    <ul>            
    <li>We observed significant differences in Civil Justice topics, particularly in questions <b>q44j_G2</b> and <b>q3d</b>. Upon comparison with the TPS, we noted consistent discrepancies, especially in variables <b>q44a_G2</b>, <b>q44d_G2</b>, and <b>q44e_G2</b>. We recommend further research on this question, primarily because this subpillar consistently flags in all tests.</li>
    </ul>
    <ul>            
    <li>We noted substantial differences in Criminal Justice variables. Nearly all the variables related to police, prosecutors, and judges show significant negativity in the pretest data. Upon comparison with the TPS, we observed that these negative discrepancies also exist for certain sources. Hence, we recommend further investigation into these differences.</li>
    </ul>
    <ul>            
    <li>Note: We are aware of discrepancies in the first four pillars. However, it's important to highlight that Hungary has undergone significant changes in these topics over the last seven years. In fact, these four factors in the ROLI are the ones that have experienced the most decline in Hungary.</li>
    </ul>  
    </li>
    <br>
    <li><b>TPS Public Opinion Polls and TPS expert data:</b>
    <br>
    <br>
    <ul>            
    <li>The most noteworthy positive variances were observed within subpillars associated with Criminal Justice. Negative disparities were identified in subpillars concerning people's awareness of their rights <b>(q44a_G2)</b>, and consistently in the cost of justice <b>(q44d_G2 and q44e_G2)</b>. We recommend to explain better this differences.</li>    
    <li>As mentioned, there are differences in Criminal Justice variables related to criminal investigation (<b>q3g</b>), the respect of victims' rights (<b>q44f_G1</b>), and perceptions about prosecutors and judges (<b>q43f_G2</b> and <b>q3d</b>). We will focus on these discrepancies, as we have observed their persistence in the GPP comparisons over the years.</li>
    <li>While significant differences are noticeable across almost all subpillars compared to the expert data, these disparities don't correspond with consistently similar results from other tests. As a result, concerns regarding their validity are mitigated.</li>
    </ul>
    </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Santiago Pardo.
    <br>  
    <br>          
          ")
    } else{
      p<- c("
            <b>Insights summary</b>
            <br>
            We are seeing that across time, all of the pillars in the GPP are changing. For the most part, these changes indicate more negative opinions about the rule of law with some positive trends in select sub pillars. Nevertheless, the result of these trends in terms of our full fieldwork data are similar to what other sources are observing. Therefore, according to our data, people in Hungary have a decreased view of the rule of law, which is in accordance with the third party source data.
            <br>
            <br>
            Given that we expect to see changes from our previous data in Hungary, we are noting sub pillars that are flagged in the TPS analyses. In this regard, all the topics that are flagged in the time comparisson but supported by green flags in the TPS are considered as something normal in the context of Hungary. Therefore, what we are highlighting are the discrepancies found in the data in two aspects: the ones that are consistent in both analyses, and also the ones that are not supported in the TPS.
            <br>
            <br>
            <b> Sub Pillars to Research </b>
            <ul>
            <li> Pillar 4. Fundamental Rights
            <ul>
            <li> Negative trend in 4.4: Solidarity </li>
            <ul>
            <li> When we asked if people thought that workers’ freedom to form labor unions and negotiate with employers is important, we found a score of 0.46 when our previous score was 0.61. Additionaly, when we asked if working conditions are favorable, we found a score of 0.36 while the European Social Survey found a score of 0.74 in 2021. These large discrepancies should be investigated through context about both unionization and working conditions. </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 7. Civil Justice
            <ul>
            <li> Negative trend in 7.1: Legal Security </li>
            <ul>
            <li> When we asked if people are aware of their rights when they face a legal problem we found an score of 0.34, while the Fundamental Rights Survey shows a score of 0.758 when they asked about the views on authorities providing information for people in a simple way. The large discrepancy between these scores should be explained further. Since this source of comparison is a low match, we recommend to give a context about the knowledge of people about their rights when they faced a legal problem. </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 8. Criminal Justice 
            <ul>
            <li> Negative trend in 8.7: Prisons </li>
            <ul>
            <li> When we asked how confident people are that the criminal justice system guarantees the safety and human rights of people deprived of their liberty, we found a score of 0.47, while Varieties of Democracy found a score of 0.835 when they asked if there was freedom from torture. Although the questions have a low match, they are still related and should be complemented with research about safety and human rights in the criminal justice system. </li>
            </ul>
            </ul>
            </li>
            </ul>
            All these discrepancies between the data are also consistent at the NUTS level; we observe that the significant differences persist across all NUTS regions for all sub-pillars flagged.
            <br>
            <br>
            The topic areas highlighted above are what our data is telling us through the given analyses. However, it is still important to take into account the media reports and other qualitative background research to potentially identify any other sub-pillar that should be researched more thoroughly. 
            ")
    }
    
  }
  
  if (country == "Ireland"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Ireland                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found significant issues with the TPS expert sources during the data validation process.</b>
    <br>
    <br>
    Some issues that we believe are worth noticing are:
    <br>  
    <br>
    <ul>    
    <li><b>GPP Over Time:</b>
    <br>
    <br>
    <ul>            
    <li>We did not find any specific issue when comparing the pretest data to the previous GPP wave in Ireland. </li>            
    <li>As the comparison data hails from only two years ago, it's not surprising that there aren't significant differences. This indicates a satisfactory level of consistency initially. </li>
    </ul>    
    </li>
    <br>
    <li><b>TPS Public Opinion Polls: </b>
    <br>
    <br>
    <ul>            
    <li>Notable disparities have been identified between third-party public opinion polls conducted during elections and the actual free voting results <b>(q39d_G1)</b>. This variance might be linked to the timing of the third-party survey, which occurred a year after the elections and could mirror certain dissatisfaction with the process.</li>    
    <li>The most noteworthy positive variances were observed within subpillars associated with Open Government, particularly concerning media freedom <b>(q39b_G1)</b> and the freedom of civil society to express dissenting opinions against the government <b>(q39h_G2)</b>. These variations remain consistent across other public opinion polls. We recommend to explain better this differences.</li>    
    <li>Negative disparities were identified in subpillars concerning people's awareness of their rights <b>(q44a_G2)</b> and consistently in the cost of justice <b>(q44d_G2 and q44e_G2)</b>. We recommend conducting further research specifically focusing on access to justice in Ireland.</li>    
    </ul>
    </li>
    <br>
    <li><b>TPS expert data: </b>
    <br>
    <br>
    <ul>
    <li> In general, our pretest scores are lower than TPS expert scores across all flag colors. </li>
    <li> We found consistently lower scores in terms of the Independence of the Judiciary (q44j_G2 and q44i_G2) and Absence of Authoritarianism (q52_G2 and q56_G1). However, many of these may be caused by low question match or perfect expert scores.</li>  
    <li> There are also many flags in freedom of assembly (q39e_G1) and freedom of speech (q39e_G2). However, these come from differences with Freedom House, whose experts codings for Cyprus reflect a perfect score for the country. </li>
    <li> Since these disparities don't correspond with consistently similar results from other tests, concerns regarding their validity are mitigated. </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Santiago Pardo.
    <br>  
    <br> 
          ")
    
  }
  
  if (country == "Italy"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Italy                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is consistently lower than the Public Opinion TPS comparison indicators in relation to Solidarity (q45c_G1 and q45c_G2) and Justice (q59e, q44d_G2, q44e_G2, q44f_G1, and q43f_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in almost all of the indicators in Pillar 1, Pillar 2, and Pillar 7. We suggest futher research into these topic areas. </li>            
    <li> Many of these comparisons involved very high scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Italy so highly. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Natalia Rodriguez.
    <br>  
    <br>")
    
  }
  
  if (country == "Latvia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Latvia                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we did not find any significant inconsistency with other Public Opinion Polls or our own 2021 data.</b>
    <br>  
    <br>
    <ul>    
    <li><b>GPP Over Time:</b>
    <br>
    <br>
    <ul>            
    <li>We did not find any significant changes from our last GPP round in Latvia. If it was expected that there should have been significant changes in any topic area, we are not seeng it. </li>            
    </ul>    
    </li>
    <br>
    <li><b>TPS Public Opinion Polls and TPS expert data:</b>
    <br>
    <br>
    <ul>            
    <li>We did not find any significant differences from other public opinion polls.</li>    
    <li>Regarding third party expert surveys, our data is scoring consistently lower than expert opinions, especially in Pillars 1.</li>
    </ul>
    </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>          
          ")
    
  }
  
  if (country == "Lithuania"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Lithuania                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we did not find any significant issues during the data validation process.</b>
    <br>
    <br>
    Some minor issues that we believe are worth noticing are:
    <br>  
    <br>
    <ul>    
    <li><b>GPP Over Time:</b>
    <br>
    <br>
    <ul>            
    <li>We find significative differences on Civil Justice subpillar, specially in question <b>q44j_G2</b>. However, when we compare this question with the TPS, we find that the data points are similar. We suggest further research regarding this question, mainly because this subpillar is consistently flagging in all the tests. </li>            
    </ul>    
    </li>
    <br>
    <li><b>TPS Public Opinion Polls and TPS expert data:</b>
    <br>
    <br>
    <ul>            
    <li>The most noteworthy positive variances were observed within subpillars associated with Criminal Justice. Negative disparities were identified in subpillars concerning people's awareness of their rights <b>(q44a_G2)</b>, and consistently in the cost of justice <b>(q44d_G2 and q44e_G2)</b>. We recommend to explain better this differences.</li>    
    <li>While significant differences are noticeable across almost all subpillars compared to the expert data, these disparities don't correspond with consistently similar results from other tests. As a result, concerns regarding their validity are mitigated.</li>
    </ul>
    </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Santiago Pardo.
    <br>  
    <br>          
          ")
    
  }
  
  if (country == "Luxembourg"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Luxembourg                                                                                       ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (type == "pretest"){
    p<- c("
          <b>In general, we only found significant issues with the TPS expert sources during the data validation process.</b>
    <br>
    <br>
    Some comparisons that we believe are worth noting are:
    <br>  
    <br>
    <ul>    
    <li><b>GPP Over Time:</b>
    <br>
    <br>
    <ul>            
    <li> We do not have any comments for the GPP over time comparisons. </li>            
    </ul>    
    </li>
    <br>
    <li><b>TPS Public Opinion Polls:</b>
    <br>
    <br>
    <ul>            
    <li>The most noteworthy differences were observed within subpillars associated with Justice (q59e, q44e_G2, and q43f_G2). </li>    
    <li>These are all negative differences, meaning that our pretest data is lower than other public opinion polls. However, these come from comparisons with our oldest TPS data. </li>
    </ul>
    </li>
    <br>
    <li><b>TPS Expert Data:</b>
    <br>
    <br>
    <ul>            
    <li> These comparisons indicate significant differences in Pillar 1, Pillar 7, and Pillar 8, which are negative for the most part. </li>
    </ul>
    </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    } else{
      p<- c("
            <b>Insights summary</b>
            <br>
            We are seeing that across time, most of the pillars are not changing drastically over time or indicate discrepancies with the TPS Public Opinion Polls. Pillar 1 is flagged in the GPP Over Time, however there is only one indicator level analysis and it is an opinion question. Pillar 8 is flagged in the TPS Expert Surveys, however the analyses have medium to low matches. Therefore, according to our data, people in Luxembourg have a similar view of the rule of law as they did in 2021, which is in accordance with the third party source data. Certainly, the primary source disparity is with Freedom in the World, where certain variables yield significantly high scores. In such instances, we advise prioritizing a discussion on the validity of our scores rather than clarifying disparities between the sources.
            <br>
            <br>
            Given that we do not expect to see changes from our previous data in Luxembourg, we are noting sub pillars that are flagged in both the GPP and TPS analyses. In this regard, all the topics that are flagged in the time comparison but supported by green flags in the TPS are considered as something normal in the context of Luxembourg Therefore, what we are highlighting are the discrepancies found in the data in two aspects: the ones that are consistent in both analyses, and also the ones that are not supported in the TPS.
            <br>
            <br>
            <b> Sub Pillars to Research </b>
            <ul>
            <li> Pillar 1. Constraints on Government Powers
            <ul>
            <li> Positive trend in 1.11: Government officials who abuse their power are sanctioned for misconduct (accountability and sanctions for misconduct) </li>
            <ul>
            <li> When we asked if people a hypothetical situation about a government official taking money for personal benefit, we found a score of 0.738 when our previous score was 0.653. This indicates that people think it is more likely that the official is punished appropriately. Although the difference between scores is not very large, the t-test indicates that overall, individuals are answering more positively than before. </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 4. Fundamental Rights
            <ul>
            <li> Negative trend in 4.6: Justice </li>
            <ul>
            <li> When we asked if people thought that the criminal justice system treats those accused as innocent until proven guilty, we found a score of 0.592 while Freedom in the World assigned a score of 1 for the question regarding if due process prevails in civil and criminal matters. The large discrepancy between these scores should be explained further. Since this source of comparison is includes a strict expert encoding, we recommend to give a context about the rights of the accused and due process. </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 7. Civil Justice
            <ul>
            <li> Negative trend in 7.1: Legal Security </li>
            <ul>
            <li> When we asked if people are aware of their rights when they face a legal problem we found a score of 0.438, while the Fundamental Rights Survey shows a score of 0.825 when they asked about the views on authorities providing information for people in a simple way. The large discrepancy between these scores should be explained further. Since this source of comparison is a low match, we recommend to give a context about the knowledge of people about their rights when they faced a legal problem. </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 8. Criminal Justice 
            <ul>
            <li> Negative trend in 8.6: Due process of law </li>
            <ul>
            <li> When we asked how confident people are that the criminal justice system guarantees a fair trial of all accused people, we found a score of 0.633, while Freedom in the World found a score of 1 when they asked if due process prevaisl in civil and criminal matters. In the same analysis identified in Sub Pillar 4.6, when we asked if people thought that the criminal justice system treats those accused as innocent until proven guilty, we found a score of 0.592 while Freedom in the World assigned a score of 1 for the question regarding if due process prevails in civil and criminal matters. Although these questions have low matches, they are still related and should be complemented with research about the rights of the accused and due process. </li>
            </ul>
            </ul>
            </li>
            </ul>
            The topic areas highlighted above are what our data is telling us through the given analyses. However, it is still important to take into account the media reports and other qualitative background research to potentially identify any other sub-pillar that should be researched more thoroughly. 
            ")
    }
  }
  
  if (country == "Malta"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Malta                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("<b>In general, we did not find any significant issues during the data validation process.</b>
    <br>
    There are some minor issues that are worth noting:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>We did not find any specific issue when comparing the pretest data to the previous GPP wave in Malta </li>            
    <li>Since the comparison data is from only 2 years ago, the lack of significant differences is expected, and Malta is not known for having any large changes.</li>            
    <li>There are only a few red flags in the t-test. However, these flags are in questions regarding the respondents' experiences and opinions, which are prone to change. </li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:  
     <ul>            
    <li>We have found a few significant differences in comparison to third party public opinion polls in Pillars 4, 5, and 7. However, this could be attributed to a combination of low matches and/or old data (especially from 2019).</li>            
    <li>The most significant differences were found in subpillars related to fundamental freedoms (q39b_g1 and q39h_g2), justice (q59e) and civil justice (q44a_G2 and q44e_G2). </li>        
    <li>The direction of the difference is not consistent among the related indicators.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:  
    <ul>            
    <li>There are differences between the pretest data and the expert TPS in Pillars 2 and 8. </li>
    <li>We found some yellow flag differences in absence of bribery (q3a and q4d). </li>            
    <li>There are also flags in due process (q44h_G1) and prisons (q44k_G1 and q44g_G1). However, these come from differences with low matched expert questions.</li>            
    </ul>    
    </li>
    </ul>
    <br> 
    The designated point person for this analysis within the DAU is Natalia Rodriguez.
    <br>  
    <br>
   
          ")
    
  }
  
  if (country == "Netherlands"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Netherlands                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is consistently lower than the Public Opinion TPS comparison indicators in relation to Solidarity (q45c_G1 and q45c_G2) and Justice (q59e, q44d_G2, q44e_G2, q44f_G1, and q43f_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators in Pillar 1 and Pillar 2. We suggest futher research into these topic areas. </li>            
    <li> However, many of the red flags noted across the TPS Expert comparisons are close to the cutoff between yellow and red flags. Therefore, the differences in scores are not as alarming. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Natalia Rodriguez.
    <br>  
    <br>")
    
  }
  
  if (country == "Poland"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Poland                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("There is no paragraph")
    
  }
  
  if (country == "Portugal"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Portugal                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    
    <b>In general, we did not find any significant issues during the data validation process.</b>
    <br>
    Some minor issues that we believe are worth noticing are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li>Despite the comparative data being old, distinguishable changes over time were not identified. Consequently, we recommend directing the research focus toward aspects anticipated to have evolved over the years, which are not listed below.</li> 
    <li>Significant improvements were observed in Media Freedom (q39b_G1), Freedom of Speech (q39e_G2), and Open Government (q3c and q3a).</li>            
    <li>Contrarily, noteworthy deterioration was found in indicators related to the transparency of Government budget (q9a) and contracts (q9b).</li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>The pretest data revealed more favorable scores than the TPS comparison indicators in relation to Freedom to Vote (q39d_G1), Media Freedom (q39b_G1), Freedom of Assembly (q39h_G2), and Police Performance (q43e_G2).</li>        
    <li>Contrarily, the pretest data consistently exhibited lower scores than the TPS comparison indicators concerning Working Conditions (q45c_G1 and q45c_G2), Access to Justice (q44d_G2 and q44e_G2), and the Performance of Prosecutors (q43f_G2).</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that TPS expert scores are systemically lower than our pretest scores in terms of the Independence of Judges (q44j_G2 and q43_G1), Anti-corruption Government Efforts (q4d), and Equality before the Law (q59a).</li>            
    <li> We found lower scores across many comparisons, and consistently in terms of the Independence of the Judiciary (q44j_G2 and q44i_G2) and Absence of Authoritarianism (q52_G2 and q56_G1). However, many of these may be caused by low question match or perfect expert scores.</li>  
    <li> Since these disparities don't correspond with consistently similar results from other tests, concerns regarding their validity are mitigated. </li>
    </ul>    
    </li>
    </ul>
    <br>   
          
    ")
    
  }
  
  if (country == "Romania"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Romania                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c(" 
    <b>In general, we did not find any significant issues during the data validation process.</b>
    <br>
    <br>
    Some minor issues that we believe are worth noticing are:
    <br> 
    <br>
    <ul>    
    <li>TPS Public Opinion Polls and Expert Data:      
    <br>
    <br>
    <ul>            
    <li>We observed significant differences in Civil Justice basic matters, particularly in topics about corruption (<b>q44j_G2</b>), enforcement (<b>q44j_G2</b>), accessibility(<b>q44d_G2</b>), and efficiency (<b>q44e_G2</b>). We recommend further research on this question, primarily because this subpillar consistently flags in both tests. In this case, the perceptions are critical towards the justice system.</li>
    <li>The pretest data revealed more favorable scores than the TPS comparison indicators in relation to Fundamental Freedoms, in special with Freedom to Vote (<b>q39d_G1</b>), Media Freedom (<b>q39b_G1</b>), and Freedom of Assembly (<b>q39h_G2</b>).</li>        
    </ul>
    </li>
    </ul>
    <br> 
    <br>  
    The designated point person for this analysis within the DAU is Santiago Pardo.
    <br>  
    <br>
    ")
    
  }
  
  if (country == "Slovakia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Slovakia                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (type == "pretest"){
    p<- c("
          <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is consistently lower than the Public Opinion TPS comparison indicators in relation to Justice (q59e, q44a_G2, q44d_G2, q44e_G2, and q43f_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in almost all of the indicators in Pillar 1 and Pillar 4. We suggest futher research into these topic areas. </li>            
    <li> We also found that our pretest data is lower than the expert scores in Pillar 8. </li>  
    <li> Many of these comparisons involved near-perfect scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Slovakia so highly. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>  ")
    } else {
      p<- c("
            <b>Insights summary</b>
            <br>
            We are seeing that across time, a few pillars are indicating changes. Most only have one or two indicators, however pillar 3 has 25 comparisons. Although many of these have red flags, some are positive changes while others are negative changes and overall encompass opinion questions. In comparison to the TPS public opinion polls, we have 0 red flags! This indicates that our data is in accordance with other population surveys, which is one of the most important goals. In terms of the TPS expert surveys, there are a few pillars flagged, however we are conscious that we trust these scores less. 
            <br>
            <br>
            Given that we do not expect to see large changes from our previous data in Slovakia, we are noting sub pillars that are flagged in either the GPP and TPS analyses. In this regard, all the topics that are flagged in the time comparison but supported by green flags in the TPS are considered as something normal in the context of Slovakia Therefore, what we are highlighting are the discrepancies found in the data in two aspects: the ones that are consistent in both analyses, and also the ones that are not supported by another analysis.
            <br>
            <br>
            <b> Sub Pillars to Research </b>
            <ul>
            <li> Pillar 1. Constraints on Government Powers
            <ul>
            <li> Negative trend in 1.06:  Respect for the legitimacy of the constitutional order, the law making process, and political opponents (absence of authoritarianism) </li>
            <ul>
            <li> When we asked if people agreed that emergency powers are utilized to circumvent institutional checks and balances, we found a score of 0.318 while Freedom in the World provided a score of 0.822 when asking if members of the executive respect the constitution. The large discrepancy between these scores should be explained further. Since this source of comparison is a low match with an expert TPS, we recommend to give context about respect for checks and balances. 
            </li>
            </ul>
            </ul>
            </li>
            <li> Pillar 5. Security
            <ul>
            <li> Negative trend in 5.1: People feel safe </li>
            <ul>
            <li> When we asked how safe people feel walking in their neighborhood at night, we found a score of 0.52 when our previous score was 0.58. Although the difference between scores is not very large, the t-test indicates that overall, individuals are answering more negatively than before. This inconsistency is observed across all NUTS regions.</li>
            </ul>
            </ul>
            </li>
            <li> Pillar 8. Criminal Justice 
            <ul>
            <li> Positive trend in 8.5: Victim's Rights </li>
            <ul>
            <li> When we asked how confident people are that the criminal justice system allows all victims of crime to seek justice regardless of who they are, we found a score of 0.618, when our previous score was 0.550. Furthermore, when we asked if people thought that the criminal justice system provides victims of crime with the service and support they need, we found a score of 0.618 when our previous score was .579. Although this subpillar has a green flag in the TPS public opinion poll analysis, we are still flagging it because that green flag is the result of a low matched comparison and the previous GPP data is most updated. </li>
            <li> At the NUTS level, discrepancies are evident in the regions 'SK01: Bratislavský kraj' and 'SK03: Stredné Slovensko', where the scores are significantly higher than the rest of the country. Finding further explanations for the improvements in these regions' criminal justice systems would better clarify the overall explanation.</li>              
            </ul>
            </ul>
            </li>
            </ul>
            The topic areas highlighted above are what our data is telling us through the given analyses. However, it is still important to take into account the media reports and other qualitative background research to potentially identify any other sub-pillar that should be researched more thoroughly. 
            ")
    }
  }
  
  if (country == "Slovenia"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Slovenia                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we found significant issues in all of the analyses.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <ul> 
    <ul>    
    <li>GPP Over Time:         
    <ul>            
    <li> We found that our pretest data is lower than the previous GPP in regards to Absence of Corruption (q7e, q3a, and q3b), Civic Participation (q1j, q39e_G1, q39c_G2 and q39g_G1), and Civil Justice (q3d, q44j_G2).</li>            
    <li> Since the previous GPP data is from 2017, we expect significant changes to be present.</li>            
    </ul>    
    </li>
    <br>
    <li>TPS Public Opinion Polls:  
     <ul>            
    <li>We have found that our pretest data is lower than TPS Public Opinion Polls in comparisons regarding Justice (q59e, q44d_G2, q44e_G2, q43f_G2, and q44f_G1) and Solidarity (q45c_G1 and q45c_G2).</li>            
    <li>The direction of the difference is not consistent among the related indicators.</li>
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:  
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators in Pillars 1, 3, and 4. We suggest futher research into these topic areas. </li>            
    <li> Most of these comparisons involved near-perfect scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Slovenia so highly. </li>          
    </ul>    
    </li>
    </ul>
    <br> 
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "Spain"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Spain                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS comparisons.</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>    
    <li>TPS Public Opinion Polls:         
    <ul>            
    <li>Although some noteworthy distinctions were uncovered, these could potentially be attributed to the outdated data or low matches between GPP questions and TPS indicators.</li>            
    <li>Our pretest data is significantly different from the Public Opinion TPS comparison indicators in relation to Justice (q59e, q44a_G2, q44d_G2, q44e_G2, q43e_G2, and q43f_G2) and Freedoms (q39b_G1, q39h_G2).</li>        
    </ul>  
    </li>
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in almost all of the indicators in Pillar 1 and Pillar 7. We suggest futher research into these topic areas. </li>            
    <li> We also found that our pretest data is lower than the expert scores in Pillar 8. </li>  
    <li> Many of these comparisons involved high scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Spain so highly compared to what their citizens think. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }
  
  if (country == "Sweden"){
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Sweden                                                                                        ----
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    p<- c("
    <b>In general, we only found issues with the TPS Expert Data</b>
    <br>
    Some issues we would like to note are:
    <br>   
    <br>
    <ul> 
    <ul>  
    <br>
    <li>TPS Expert Data:         
    <ul>            
    <li>Analysis indicates that our pretest scores are systemically lower than TPS expert scores in many of the indicators in Pillars 1, 2, 3, and 8. We suggest futher research into these topic areas. </li>            
    <li> Most of these comparisons involved near-perfect scores from the TPS expert evaluations. It may be worthwhile to investigate why experts scored Sweden so highly compared to what the citizens think. </li>
    </ul>    
    </li>
    </ul>
    <br>  
    The designated point person for this analysis within the DAU is Carlos Toruño.
    <br>  
    <br>")
    
  }

  
  return(HTML(p))
}

