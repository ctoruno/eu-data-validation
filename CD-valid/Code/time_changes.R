time_changes <- function(data, country) {
  
  # Ensure the data frame is sorted by year within each country
  
  data <- data %>% 
    filter(country == country) %>%
    arrange(country, year)
  
    # Get the most recent year and calculate the previous year
    years <- unique(country_data$Year)
    current_year <- max(years)
    previous_year <- current_year - 1
    
    # Subset data for the most recent and found previous year
    recent_year_data <- country_data %>% 
      filter(Year == current_year)
    
    previous_year_data <- country_data %>% 
      filter(Year == previous_year)
    
    # Perform a t-test on the measurements for the two years
    t_test_result <-
      t.test(recent_year_data$Measurement, previous_year_data$Measurement)
    
    # Return the t-test result along with the country name
    tibble(
      country = country,
      ttestResult = t_test_result
    )
  }
  
  return(t_test_results)
}
