# =============================================================================
# Get starting values for exponential model
# earlycapistran@comunidad.unam.mx - March 2021
# =============================================================================

#' @title Get starting values for an exponential model
#' 
#' @description Obtains approximate starting values for exponential model 
#' 'y~a*exp(b*x)' 
#' by linearizing the exponential model to 
#' 'ln(y)~ln(a)+bx' 
#' and transforming parameter 'a'. Values generated with this function
#' may not be apropriate for other nonlinear models.
#'
#' @param x A vector or dataframe column of an independent variable
#' @param y A vector or dataframe column of a dependent variable
#' @return list
#' 
#' @export
#' @usage
#' getExpStartVal(x, y)
#' 
#' @importFrom stats lm
#' @importFrom stats coef

getExpStartVal <-  function(x, y) {
  # Assign x and y variables to dataframe columns
  # colnames(data)[colnames(data) == x] <- "x" 
  # colnames(data)[colnames(data) == y] <- "y" 
  # Run linearized version of exponential model (ln(y)  ~ ln(a) + bx)
  lm.log <- lm(log(y) ~ x) 
  # Extract parameters and transform 'a' to get approximate starting values
  a <- as.numeric(exp(coef(lm.log)[1]))
  b <- as.numeric(coef(lm.log)[2])
  sv <- list(a=a,b=b)
  cat("Approximate starting values for exponential model defined as", 
      "\n", 'y ~ a * exp(b*x)', "\n")
  svResult <- cbind("a (intercept)" = a, "b (slope)" = b)
  rownames(svResult) <- c("")
  print(svResult)
  return(sv)
}