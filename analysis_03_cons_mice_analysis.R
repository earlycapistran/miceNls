# ---
# title: Analysis of multiply imputed data for C. mydas monitoring data
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: July 2020
# Script and data info:
#   - This script compares multiple imputation through chained equations (MICE)
#     from several imputation models.
#   - Data consists of green turtle (Chelonia mydas) CPUE data from ecological
#     monitoring. 
#   - Data were obtained from ecological monitoring (1995-2018) in 
#     Bahía de los Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo 
#     Tortuguero de Bahía de los Ángeles, and Comisión Nacional de Áreas 
#     Naturales Protegidas.
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
library("here")
library("devtools")
load_all("consLettersUtils")

# Source functions ------------------------------------------------------------
# source("R/functions/getExpStartVal.R")
# source("R/functions/mCount.R")
# source("R/functions/analyseMiraResi.R")
# source("R/functions/analyseNlsResiduals.R")
# source("R/functions/getPooledParams.R")
# source("R/functions/obsImpResiPlot.R")
# source("R/functions/poolResiduals.R")
# source("R/functions/getMiceR2.R")
# source("R/functions/getPooledR2.R")

#  Load data and prepare data -------------------------------------------------
# Load data 
cmydas_data <- read.csv("data/cpue_data.csv", header=TRUE)

# Prepare data for "Conservation" phase 
cons_data = cmydas_data %>%  
  # Include LEK data from commercial fishery "collapse" stage to interpolate 
  #values from 1983-1994
  filter(stage>=4) %>%  
  # We only need observed values from "Collapse" phase to interpolate the gap
  # between LEK and monitoring data. Remove na's (these are imputed separately 
  # using LEK data in: "R/cf_mice_analysis.R")
  filter(!(stage == 4 & is.na(cpue))) %>% 
  select(yearSerial, cpue)

# Impute missing data with MICE -----------------------------------------------
# Calculate the percentage of missing data and number of datasets to impute
m_count <- mCount(cons_data, "cpue")

# Create an imputation with "m" datasets 
mice_data <- mice(data=cons_data,
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
saveRDS(mice_data, "results/cons_mice_data.rds")

# You can also load the imputed datasets from here
# mice_data <- readRDS("results/cons_mice_data.rds")

# Fit imputed datasets to analysis model for observed values ------------------
# 'y ~ a * exp(b * x)' 

# We'll use a quasi-Newton method to optimize values by starting with values
# for the observed model and updating with parameter estimates generated
# by pooling 'mice' models.

# Get approximate starting values using observed data by linearising the 
# nonlinear model to 'ln(y) ~ ln(a) + bx' and transforming parameter 'a'
exp_start <- getExpStartVal(cons_data$yearSerial, cons_data$cpue)

# Fit nonlinear model to observed values for comparison
obs_mod <- nls(cpue ~ a * exp(b * yearSerial), 
                   data = cons_data, 
                   start = exp_start)
summary(obs_mod)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# a 0.0005400  0.0008218   0.657    0.518    
# b 0.1481040  0.0238417   6.212 3.67e-06 ***

# Check residuals for observed data model:
analyseNlsResiduals(obs_mod)

# Fit nonlinear model to the 'm' complete datasets
mice_exp <- with(mice_data, 
                 nls(cpue ~ a * exp(b * yearSerial), 
                     start = exp_start))
# Pool results of the 'm' repeated analyses and get summary
mice_exp_pooled <- pool(mice_exp)
summary(mice_exp_pooled, conf.int = TRUE)

# Get a quasi-R2 value by pooling quasi-R2 from the 'm' repeated 
# complete-data models
getPooledR2(mice_exp)

# Analyze residuals and fitted values averaged across all 'm' complete-data 
# models
poolResiduals(mice_exp)

# You can also evaluate residuals for each of the 'm' complete-data models
# separately by uncommenting the next line
# analyseMiraResi(pmm_exp_update); beep()

# Run again using parameter estimates as starting values to verify result
exp_pooled_params <- getPooledParams(mice_exp_pooled)
cons_qn1 <- with(mice_data, 
                 nls(cpue ~ a * exp(b * yearSerial),
                     start = exp_pooled_params))
cons_qn1_pooled <- pool(cons_qn1)
summary(cons_qn1_pooled, conf.int = TRUE)
# Note that parameter estimates vary slightly

# Get quasi-R2 and analyse residuals
getPooledR2(cons_qn1)
poolResiduals(cons_qn1)

# Run again using parameter estimates as starting values to verify result
cons_qn1_params <- getPooledParams(cons_qn1_pooled)
cons_qn2 <- with(mice_data, 
                 nls(cpue ~ a * exp(b * yearSerial), 
                     start = cons_qn1_params))
cons_qn2_pooled <- pool(cons_qn1)
summary(cons_qn2_pooled, conf.int = TRUE)

# Get quasi-R2 and analyze residuals
getPooledR2(cons_qn2)
poolResiduals(cons_qn2)

# You can also check residuals by plotting residual vs. fitted values for
# all multiply imputed models (grey) and for the observed data model (black)
dev.off()
obsImpResiPlot(cons_qn2, obs_mod)

# Note that parameter estimates, R2, and residual tests values are unchanged,
# suggesting the model is robust.

# Save pooled models
saveRDS(cons_qn2_pooled, "results/cons_pooled_final.rds")
# Save multiply imputed repeated analyses
saveRDS(cons_qn2, "results/cons_mira_final.rds")
