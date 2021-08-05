# =============================================================================
# mCount
# earlycapistran@comunidad.unam.mx - August 2020
# =============================================================================

#' @title Calculate datasets to impute
#' 
#' @description 
#' Calculate the percentage of missing data and the number of 
#' datasets to impute as a number equivalent to the percentage
#' of missing data. 

#' Bodner, T. E. (2008). What Improves with Increased Missing 
#' Data Imputations? Structural Equation Modeling: A 
#' Multidisciplinary Journal, 15(4), 651–675. 
#' https://doi.org/10.1080/10705510802339072

#' @param data A data frame
#' @param yName A character string with the column name
#' of the response variable that will be imputed
#' @return A list with two items
#' @export
#' @usage mCount(data, yName)
#' 
#' @importFrom VIM countNA

mCount <- function(data, yName) {
  if (!is.character(yName)) 
    stop("'yName' must be a character string")
  var <- data[, yName]
  naCount <- VIM::countNA(var)
  yCount <- length(var)
  mPer <- (naCount/yCount)
  mCount <- mPer * 100
  mCount <- round(mCount, 0)
  result <- list(mPer, mCount)
  result <- cbind("Percentage of missing data" = mPer, 
                    "Number of datasets to impute" = mCount)
  rownames(result) <- c("")
  print(result)
  return(result)
}
