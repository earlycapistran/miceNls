# =============================================================================
# R^2 for NLS
# earlycapistran@comunidad.unam.mx - July 2020
# =============================================================================

#' @title R^2 values for NLS models
#'
#' @description Returns R^2 and adjusted R^2 for nonlinear least squares (nls) 
#' models.
#' 
#' Adapted from the cuasi-R^2 calculation from 'easynls' by
#' Emmanuel Arnhold: https://rdrr.io/cran/easynls/
#' 
#' @param nls An object of class 'nls'
#' @return R^2 value and adjusted R^2 value
#' @export
#' @usage
#' getNlsR2(nls)
#' 
#' @importFrom stats nls
#' @importFrom stats var
#' @importFrom stats deviance
#' @importFrom stats coef

getNlsR2 <- function(nls) {
  if (class(nls) != "nls") 
    stop("The object must have class 'nls'")
  
  gl <- length(fitted(nls)) - 1
  sqt <- var((fitted(nls) + resid(nls))) * gl
  r2 <- (sqt - deviance(nls))/sqt
  r2=round(r2,4)
  p1 <- (gl/((gl + 1) - (length(coef(nls) + 1))) * (1 - r2))
  adjR2 <- 1 - p1
  adjR2=round(adjR2,4)
  rResult <- cbind("R^2" = r2, "Adjusted R^2" = adjR2)
  rownames(rResult) <- c("")
  print(rResult)
}
