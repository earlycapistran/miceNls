# =============================================================================
# R^2 for nonlinear least squares regression with multiply imputed repeated 
# analyses
# earlycapistran@comunidad.unam.mx - July 2020
# =============================================================================

#'  Returns a R^2 value for nonlinear least squares (nls) 
#'  regression. This version is for use with 'mice' and does
#'  not return adjusted R^2.
#'  Adapted from the R^2 calculation from 'easynls' by
#'  Emmanuel Arnhold: https://rdrr.io/cran/easynls/
#' 
#' @param nls An object of class 'nls'
#' @return R^2 value
#' @export
#' @usage
#' getMiceR2(nls)
#' 
#' @importFrom stats fitted
#' @importFrom stats resid
#' @importFrom stats deviance

getMiceR2 <- function(nls) {
  gl <- length(fitted(nls)) - 1
  sqt <- var((fitted(nls) + resid(nls))) * gl
  r1 <- (sqt - deviance(nls))/sqt
  r1=round(r1,4)
  return(r1)
}
