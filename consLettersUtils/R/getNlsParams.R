# =============================================================================
# Get NLS parameters
# earlycapistran@comunidad.unam.mx - January 2021
# =============================================================================

#' @title Get NLS parameter estimates
#' 
#' Extracts parameter estimates from an 'nls' model object
#'
#' @param nls An 'nls' model object
#' @return A list of parameter estimates
#' @export
#' @usage 
#' getNlsParams(nls)
#' 
#' @importFrom stats nls

getNlsParams <- function(nls) {
  a <-summary(nls)$parameters[1]
  b <- summary(nls)$parameters[2]
  result <- list(a=a, b=b)
  return(result)
}
