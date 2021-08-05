# =============================================================================
# getNlsConfInt
# earlycapistran@comunidad.unam.mx - August 2020
# =============================================================================

#' @title Get NLS confidence intervals
#' 
#' @description  
#' Calculates 95% confidence intervals for 'nls' objects using the 'as.lm.nls'
#' function by Walmes Zediani of the Universidade Federal do Paraná
#' (http://www.leg.ufpr.br/~walmes/cursoR/ciaeear/as.lm.nls.R). Returns a 
#' data frame with upper and lower bounds, fitted values, and values for
#' predictor variable to facilitate plotting.
#' 
#' @param nls An 'nls' object
#' @param data A dataframe with data used to fit model
#' @param xVarName A character string with the name of the predictor 
#' variable in the dataframe 'data'
#' @return A data frame with fitted values, upper and lower bounds, and 
#' a column with values for predictor variable
#' @export
#' @usage
#' getNlsConfInt(nls, data, xVarName)
#' 
#' @importFrom stats predict
#' @importFrom stats nls

getNlsConfInt <- function(nls, data, xVarName) {
    ci_fit <- predict(as.lm.nls(nls), interval = "confidence") 
    ci_fit <- as.data.frame(ci_fit)
    ci_fit$xVarName <- data[[xVarName]]
    colnames(ci_fit) <- c("fit", "lwr", "upr", xVarName)
    return(ci_fit)
}