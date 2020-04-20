#' Almond Yield Function
#' 
#' This function deterines yield anomolies based on Lobell et al. 
#' @param minimum temperature
#' @param percipitation
#' @return yearly yield anomoly of almond production




clim <- read.table("clim.txt",  sep=" ", header=T)


almond_yield <- function( clim_data = clim,
                          a=-0.015,
                          b=-0.0046,
                          c=-0.07,
                          d=0.0043,
                          e=0.28) {
  
  # Add in some error checking into the function
  
  # Make sure the climate data input is a dataframe
  if(class(clim_data) != "data.frame") return("Climate data input must be a data frame")
  
  # Make sure the climate data input contains the columns year, month, precip, tmin_c and tmax_c
  if(!all(has_name(clim_data, c("month", "year",  "month", "precip", "tmin_c", "tmax_c")))) return("Climate data input must contain the following columns: year, month, precip, tmin_c and tmax_c")
  
  # Make sure that the input for precipitation is larger than 0
  clim_data$precip = ifelse(clim_data$precip < 0, return("Input for precipitation must be a value larger than 0"), clim_data$precip)
  
  # Make sure that the maximum tempertaure will be larger than the minimum temperature
  clim_data$tmin_c = ifelse(clim_data$tmin_c > clim_data$tmax_c, return("Input for maximum temperature must be larger than input for minimum temperature"), clim_data$tmin_c)
  
  # Average monthly maximum daily temperature, and monthly precipitation from a data frame called clim  with columns year, month, precip and tmax_c
  
  clim_month <-  clim_data %>%
    group_by(month, year) %>%
    summarize(meantmin = mean(tmin_c),
              meantmax = mean(tmax_c),
              precip=sum(precip))
  
  # Filter Jan and Feb data
  jan <- clim_month %>% 
    filter(month==1)
  
  feb <- clim_month %>% 
    filter(month==2)
  
  # Change column names
  colnames(jan) <- c("month", "year", "Tn", "Tm", "P")
  colnames(feb) <- c("month", "year", "Tn", "Tm", "P")
  
  # Data structure for yield annomalies
  yield_df <- data.frame(year = jan$year, YA = NA)
  
  # Loop through each year
  for (i in 1:length(yield_df$year)) {
    yield_df$YA[i] = a*feb$Tn[i] + b*(feb$Tn[i]^2) +c*jan$P[i] + d*(jan$P[i]^2) + e
  }
  
  # Calculate max and min yields
  max_yield <- yield_df %>% 
    arrange(-abs(YA)) %>% 
    head(1)
  
  min_yield <- yield_df %>% 
    arrange(abs(YA)) %>% 
    head(1)
  
  # Change column names of max and min yields
  colnames(max_yield) <- c("Year", "Maximum Yield Anomaly")
  colnames(min_yield) <- c("Year", "Minimum Yield Anomaly")
  
  # Create list with three elements
  yield_list <- list(yield_df, max_yield, min_yield)
  
  # Return list
  return(yield_list)
}

almond_yield()