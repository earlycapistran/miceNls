# =============================================================================
# Get Growth Rate
# earlycapistran@comunidad.unam.mx - March 2021
# =============================================================================

#' @title Calculate annual growth rate
#' 
#' @description  Calculates annual growth rate based on the equation:
#' N = N0 * e ** rt
#' 
#' Where N is the end value, N0 is the starting value, r is the rate
#' of growth and t is elapsed time. 
#' This equation is solved for 'r' such that: 
#' r = ln(N/N0) / t
#'
#' @param data A data frame with one column for a "time" and a column for 
#' the dependent variable
#' @param start_year A numerical value corresponding to start year
#' @param end_year A numerical value corresponding to end year
#' @return Returns numerical value for annual growth rate. Prints growth rate
#' as a percentage.
#' 
#' @export
#' 
#' @usage
#' getGrowthRate(data, start_year, end_year)

getGrowthRate <- function(data, start_year, end_year) {
  time <- NULL
  colnames(data) <- c("time", "y")
  # Get rows with start and end values
  start_value <- subset(data, time == start_year)$y 
  end_value <- subset(data, time == end_year)$y
  # Calculate elapsed time
  n_years <- end_year - start_year 
  # Calculate rate of change
  rate <- (log(end_value/start_value))/n_years 
  rate <- round(rate, digits = 3)
  # Convert to percentage
  rate_perc <- round(rate, digits = 3) * 100 
  cat("Annual growth rate", "\n", rate_perc, "%")
  return(rate)
}
