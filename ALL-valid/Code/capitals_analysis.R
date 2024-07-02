capitals.fn <- function(
    
  data = eu_qrq_final
  
  ) {
  
  "%!in%" <- compose("!", "%in%")
  
  capital_mean <- data %>%
    filter(capital == 1) %>%
    group_by(nuts, country, indicator, scenario) %>%
    summarise(
      capital_value = mean(QRQ_value)
    ) %>%
    distinct() %>%
    select(!nuts)
  
  region_diff <- data %>%
    left_join(capital_mean, 
              by = c("country", "indicator", "scenario")) %>%
    mutate(
      difference = abs(capital_value - QRQ_value)
    ) %>%
    select(!nuts.y,
           nuts = nuts.x)
  
  results<- data.frame(matrix(nrow = 0, ncol = 8))
  
  for (i in unique(region_diff$nuts)){
    
    for (k in unique(region_diff$scenario)){
      
      datas<- region_diff%>%
        filter(scenario == k)%>%
        filter(nuts == i)
      
      iqr<- IQR(datas$difference, na.rm = TRUE)
      q1 <- quantile(datas$difference, prob=c(.25,.5,.75),  na.rm = TRUE, type=5)[[1]]
      q2 <- median(datas$difference, na.rm = TRUE)
      q3 <- quantile(datas$difference, prob=c(.25,.5,.75), na.rm = TRUE, type=5)[[3]]
      lower_bound<- q1 - (1.5*iqr)
      upper_bound<- q3 + (1.5*iqr)
      
      
      df<- datas%>%
        mutate("Lower Bound" = lower_bound,
               "Q1" = q1,
               "Median" = q2,
               "Q3" = q3,
               "Upper Bound" = upper_bound,
               "IQR"= iqr,
               Flag = if_else(difference>upper_bound | difference < lower_bound, 1, 0)
        )%>%
        rename(Country = country,
               "NUTS Region" = nuts,
               Indicator = indicator,
               Scenario = scenario)
      
      results<- rbind(results, df) %>%
        filter(Indicator %!in% c("p_1_01_1", "p_1_01_2", "p_1_02_1", "p_1_03_1", "p_1_03_2", "p_1_03_3",
                                 "p_1_03_4", "p_1_03_5", "p_1_04_1", "p_1_04_2", "p_1_05_1", "p_1_05_2",
                                 "p_1_06_1", "p_1_06_2", "p_1_06_3", "p_1_06_4", "p_1_06_5", "p_1_06_6",
                                 "p_1_07_1", "p_1_07_2", "p_1_08_1", "p_1_08_2", "p_1_08_3", "p_1_08_4",
                                 "p_1_09_1", "p_1_09_2", "p_1_09_3", "p_1_10_1", "p_1_11_1", "p_1_12_1",
                                 "p_1_12_2", "p_1_12_3", "p_2_1_1", "p_2_2_1",  "p_2_2_2", "p_2_3_1",
                                 "p_2_4_1", "p_2_5_1",  "p_2_5_2", "p_3_1_1",  "p_3_1_2",  "p_3_1_3",
                                 "p_3_2_1",  "p_3_2_2", "p_3_2_3",  "p_3_2_4",  "p_3_2_5",  "p_3_2_6",
                                 "p_4_01_1", "p_4_02_1", "p_4_03_1", "p_4_04_1", "p_4_05_1", "p_4_05_2",
                                 "p_4_05_3", "p_4_06_1", "p_4_07_1", "p_4_08_1", "p_4_08_2", "p_4_08_3",
                                 "p_4_09_1", "p_4_09_2", "p_4_09_3", "p_4_09_4", "p_4_10_1", "p_4_11_1",
                                 "p_4_11_2", "p_4_11_3", "p_4_12_1", "p_4_13_1", "p_4_14_1", "p_4_14_2",
                                 "p_4_14_3", "p_4_14_4", "p_4_14_5", "p_6_1_1",  "p_6_1_2", "p_6_2_1",
                                 "p_6_3_1",  "p_6_3_2",  "p_6_3_3",  "p_6_3_4", "p_7_1_1",  "p_7_1_2",
                                 "p_7_2_1", "p_7_3_1",  "p_7_3_2", "p_7_4_1",  "p_7_4_2",  "p_7_4_3",  
                                 "p_7_4_4", "p_7_5_1",  "p_7_5_2", "p_7_6_1",  "p_7_6_2",  "p_7_6_3",
                                 "p_8_1_1",  "p_8_1_2", "p_8_2_1",  "p_8_2_2", "p_8_3_1",  "p_8_3_2",
                                 "p_8_3_3",  "p_8_3_4",  "p_8_3_5", "p_8_4_1", "p_8_4_2",  "p_8_4_3",
                                 "p_8_5_1", "p_8_6_1",  "p_8_6_2",  "p_8_6_3",  "p_8_6_4",  "p_8_6_5",
                                 "p_8_7_1"))
      
      
    }
    
  }
  
  return(results)
  
}
