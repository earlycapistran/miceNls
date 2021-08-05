#' Calculates the predicted value at time 'n' using the model:
#
# N = N0 * e ** rt
#
# Where N is the end value, N0 is the starting value, r is the rate
# of growth and t is elapsed time. 
#'
#' The predicted value is then compared to the observed value for time 'n'
#'
#' @param data A data frame with one column for a "time" and a column for 
#' the dependent variable
#' @param start_year A numerical value corresponding to start year
#' @param end_year A numerical value corresponding to end year
#' @param rate A numerical value for the rate of change
#' @return Prints predicted and observed value at time 'n'
#' @export
#' @usage
#' getRateComp(data, start_year, end_year, rate)

getRateComp <- function(data, start_year, end_year, rate){
  year <- NULL
  colnames(data) <- c("year", "y")
  start_value <- subset(data, year == start_year)$y
  obs_end_value <- subset(data, year == end_year)$y
  obs_end_value <- round(obs_end_value, digits = 3)
  elapsed_time <- end_year - start_year
  pred_end_value <- start_value * exp(rate * elapsed_time)
  pred_end_value <- round(pred_end_value, digits = 3)
  cat("Predicted value at the end of elapsed time", "\n", pred_end_value, "\n", 
      "Observed value at the end of elapsed time", "\n", obs_end_value)
}