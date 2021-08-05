# =============================================================================
# Get Descriptive Statistics
# earlycapistran@comunidad.unam.mx - April 2021
# =============================================================================

#' Calculates descriptive statistics (mean, median, standard deviation, 
#' minumum, maximum, and n)
#'
#' @param data A data frame
#' @param variable A column with a numerical variable of interest
#' @return A dataframe with descriptive statistics
#' 
#' @export
#' @usage
#' getDescStats(data, variable)
#' 
#' @examples 
#' getDescStats(iris, Sepal.Length)
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr summarise
#' @importFrom magrittr %>% 
#' @importFrom stats median
#' @importFrom stats sd
#' 

getDescStats <- function(data, variable) {
  var <- enquo(variable)  
  desc_stats <- data %>% 
    dplyr::summarize(mean = mean(!!var), 
              median = median(!!var),
              sd = sd(!!var), 
              min = min(!!var), 
              max = max(!!var),
              n = length(!!var))
  return(desc_stats)
}