# =============================================================================
# Residual analysis for NLS models
# earlycapistran@comunidad.unam.mx - July 2020
# =============================================================================

#' This function evaluates residuals from 'nls' model objects. It returns 
#' a normality plot, residuals vs. fitted, and a lag-plot with a trend line 
#' (lm(residual ~ residual -1)) to evaluate autocorrelation. It also 
#' provides a Shapiro-Wilk normality test, Levene test for homogeneity of 
#' variance, a Runs test for randomness, and a t-test for mean zero. 
#' 
#' @param nls an object of class 'nls' 
#' @return Residual plots: normality, residuals vs. fitted data, autocorrelation 
#' (lagged residuals vs. residuals with fitted linear model). 
#' 
#' Residual tests: Mean zero, Shapiro-Wilk normality test, Levene Test for 
#' homogeneity of variance, and Run's test for randomness
#' 
#' @export
#' 
#' @usage
#' analyseNlsResiduals(nls)
#' 
#' @importFrom magrittr %>% 
#' @importFrom car leveneTest
#' @importFrom DescTools RunsTest
#' @importFrom stats dt
#' @importFrom stats residuals
#' @importFrom stats fitted
#' @importFrom stats shapiro.test
#' @importFrom stats sd
#' @importFrom stats qqnorm
#' @importFrom stats qqline


# .............................................................................
# analyseNlsResiduals
# .............................................................................

# To run this function, you must have car, and DescTools installed
analyseNlsResiduals <- function(nls) {
  if (class(nls) != "nls") 
    stop("Use only with 'nls' objects")
  
  # Load and prepare data -----------------------------------------------------
  resi <- residuals(nls)
  resDf <- as.data.frame(resi) %>% select(resi)
  fit <- fitted(nls)
  # Subset residuals by sign
  resDf$sign <- as.factor(ifelse(resDf < 0, "negative", "positive"))
  # Store lagged residuals
  resDf$resi_lag <- c(resDf$resi[-1], NA) 
  
  # Make plots ----------------------------------------------------------------
  # Normality
  par(mfrow = c(2, 2))
  qqnorm(resDf$resi)
  qqline(resDf$resi)
  # Residual vs. fitted values
  res.plot <- plot(x = fit, y = resi,
       xlab = "Fitted values", 
       ylab = "Residuals",
       main = "Residuals versus fitted values")
  abline(h=0)
  # Lag plot with trend line (residuals vs. lagged residuals)
  plot(resDf$resi, c(resDf$resi[-1], +  NA),
       xlab = "Residuals", 
       ylab = "Lagged residuals",
       main = "Autocorrelation") 
  abline(lm(resDf$resi ~ resDf$resi_lag))
  par(mfrow = c(1, 1)) 
  
  # Run tests -----------------------------------------------------------------
  norm <- stats::shapiro.test(resDf$resi)
  levene <- car::leveneTest(resDf$resi ~ sign, data = resDf)
  runs <- DescTools::RunsTest(resDf$resi)
  result <- list(normality = norm, levene = levene, runs = runs)
  names(result) <- c("Residual Normality Test", "Levene's Test", "Runs Test")
  print(result)
  
  # Run t-test for mean = 0 ---------------------------------------------------
  stDev <- sd(resDf$resi)
  mean <- mean(resDf$resi)
  degF <- summary(nls)$df[2]
  t.value  <- abs(mean/stDev)
  p.value <- dt(t.value, df=degF)
  tResult <- cbind("Residual mean"=mean, "t-value"=t.value, "p-value"=p.value)
  rownames(tResult) <- c("")
  cat(paste("$ `t-Test for residual mean zero`", "\n"))
  print(tResult)
  cat(paste("Alternative hypothesis: true mean is not equal to 0", "\n"))
}