# Define a function named html_flags
html_flags <- function() {
  
  # Combine metadata about GPP variables with information from variable_list.df
  # and select unique rows
  subp <- metadata %>%
    select(GPP_Variable_Name, Sub_Pillar, Pillar) %>%
    rbind(variable_list.df %>%
            select(variable, subpillar, pillar) %>%
            rename("GPP_Variable_Name" = "variable", 
                   "Sub_Pillar" = "subpillar",
                   "Pillar" = "pillar")) %>%
    distinct()
  
  # Create an empty data frame with four columns
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df) <- c("Country", "GPP_Variable_Name", "Sub_Pillar", "Pillar")
  
  # Iterate over each country in the countrylist
  for (i in 1:length(countrylist)) {
    
    # Generate a paragraph for the current country
    par <- paragraph(countrylist[i], type = "full")
    
    # Extract substrings matching a specific pattern
    spmatches <- str_extract_all(par, "[0-9]\\.[0-9]{1,2}:")
    spmatches <- unlist(spmatches)
    
    # If any substrings matching the pattern are found
    if (length(spmatches) > 0) {
      
      # Select rows from subp where the Sub_Pillar column matches any of the substrings
      vars <- subp[grepl(paste(spmatches, collapse="|"), subp$Sub_Pillar), ]
      
      # Add a "Country" column to vars with the value being the current country
      vars2 <- vars %>%
        mutate(Country = countrylist[i]) %>%
        select(Country, everything())
      
      # Append vars2 to the df data frame
      df <- rbind(df, vars2)
    }
  }
  
  # Return the resulting data frame
  return(df)
}
