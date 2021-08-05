# ---
# title: Analysis of multiply imputed data for LEK
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: July 2020
# Script and data info:
#   - This script compares multiple imputation through chained equations (MICE)
#     from several imputation models.
#   - Data consists of LEK-derived green turtle (Chelonia mydas) CPUE data. 
#   - Data were obtained from local ecological knowledge of years 1952-1982, 
#     documented in Bahía de los Ángeles, Baja California, Mexico.
#     LEK data was documented by M.M. Early Capistrán and G. Garibay Melo. Data
#     are available in Early-Capistrán et al. (PeerJ, 2020).
# - - -

# Install and load libraries --------------------------------------------------

# Check if required libraries are installed and install
# if necessary

# packages <- c("beepr", " car","DescTools", "ggthemes", "mice", "tidyverse",
#                 "VIM")
# 
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }

library("beepr") # 'Beep' when analysis is ready
library("car")
library("DescTools")
library("mice")
library("tidyverse")
library("VIM")
library("devtools")
library("here")
load_all("consLettersUtils")

# Source functions ------------------------------------------------------------
# source("R/functions/getExpStartVal.R")
# source("R/functions/mCount.R")
# source("R/functions/analyseMiraResi.R")
# source("R/functions/analyseNlsResiduals.R")
# source("R/functions/getPooledParams.R")
# source("R/functions/poolResiduals.R")
# source("R/functions/getMiceR2.R")
# source("R/functions/getPooledR2.R")
# source("R/functions/obsImpResiPlot.R")
# source("R/functions/theme_cmydas.R")

#  Load data and prepare data -------------------------------------------------
# Load data 
data <- read.csv("data/cpue_data.csv", header=TRUE)

# Prepare data for "Commercial Fishing" phase 
cf_data = data %>%  
  filter(type=="LEK") %>% 
  select(yearSerial,cpue) 

# Get an overview of missing data patterns ------------------------------------
# Scatterplot
plot(x=cf_data$yearSerial, y=cf_data$cpue)

# Margin plot
marginplot(cf_data)

# Calculate the percentage of missing data and number of datasets to impute
m_count <- mCount(cf_data, "cpue")

# Impute missing data with MICE -----------------------------------------------
# Create an imputation with "m" datasets 
mice_data <- mice(cf_data,
                  m = m_count[[2]],
                  maxit = 1500,
                  meth = 'midastouch',
                  seed = 500); beep("coin")

# Plot convergence
plot(mice_data)

# Check imputed data ----------------------------------------------------------

# Check plausibility of imputed values
summary(mice_data$imp$cpue)

# XY Plot shows observed (blue) and imputed (magenta) data
xyplot(mice_data, cpue ~ yearSerial | .imp, pch = 20, cex = 0.75)

# Save multiply imputed datasets
saveRDS(mice_data, "results/cf_mice_data.rds")

# You can also load mice values from here: 
# mice_data <- readRDS("results/cf_mice_data.rds")

# Fit imputed datasets to analysis model for observed values ------------------
# 'y ~ a * exp(b * x)' 

# We'll use a quasi-Newton method to optimize values by starting with values
# for the observed model and updating with parameter estimates generated
# by pooling 'mice' models.

# Get approximate starting values using observed data by linearizing the 
# nonlinear model to 'ln(y) ~ ln(a) + bx' and transforming parameter 'a'
exp_start <- getExpStartVal(x = cf_data$yearSerial, y = cf_data$cpue)

# Fit nonlinear model to observed data for comparison
cf_obs_mod <- nls(cpue ~ a * exp(b * yearSerial), 
                   data = cf_data, 
                   start = exp_start)
summary(cf_obs_mod)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# a 24.11233    3.12376   7.719 2.07e-06 ***
#   b -0.08294    0.01300  -6.382 1.71e-05 ***

# Check residuals for observed data model:
analyseNlsResiduals(cf_obs_mod)

# Fit nonlinear model to the 'm' complete datasets
mice_cf_exp <- with(mice_data, 
                 nls(cpue ~ a * exp(b * yearSerial), 
                     start = exp_start))
# Pool results of the 'm' repeated analyses and get summary
cf_exp_pooled <- pool(mice_cf_exp)
summary(cf_exp_pooled, conf.int = TRUE)

# Run again using parameter estimates as starting values to verify result
cf_exp_pooled_params <- getPooledParams(cf_exp_pooled)  # Get parameter estimates
cf_qn1 <- with(mice_data, 
                  nls(cpue ~ a * exp(b * yearSerial), 
                      start = cf_exp_pooled_params))
cf_qn1_pooled <- pool(cf_qn1)
summary(cf_qn1_pooled, conf.int = TRUE)

# Get a quasi-R2 value by pooling quasi-R2 from 'm' repeated complete-data models
getPooledR2(cf_qn1)

# Analyse residuals and fitted values averaged across all 'm' complete-data 
# models
poolResiduals(cf_qn1)

# You can also evaluate residuals for each of the 'm' complete-data models
# separately by uncommenting the next line

# analyseMiraResi(cf_qn1); beep()

# You can also check residuals by plotting residual vs. fitted values for
# all multiply imputed models (grey) and for the observed data model (black)
dev.off()
obsImpResiPlot(mice_cf_exp, cf_obs_mod)

# Save pooled models for plotting
saveRDS(cf_qn1_pooled, "results/cf_pooled_final.rds")

# Save 'mira' object to calculate confidence intervals
saveRDS(cf_qn1, "results/cf_mira_final.rds")
