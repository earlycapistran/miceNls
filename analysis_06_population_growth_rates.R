# ---
# title: Population Growth Rates
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: March 2021
# Script and data info:
#   - This script calculates population growth rates for different periods
#   - Data consists of green turtle (Chelonia mydas) CPUE data from LEK and 
#     ecological monitoring. 
#   - Data were obtained from ecological monitoring (1995-2018) in 
#     Bahía de los Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo 
#     Tortuguero de Bahía de los Ángeles, and Comisión Nacional de Áreas 
#     Naturales Protegidas and LEK-derived data is from Early-Capistrán et al. 
#     (PeerJ, 2020)
# ---

# Load libraries and packages
library("tidyverse")
library("here")
library("devtools")
load_all("consLettersUtils")

# Load and prepare data
cmydas_data <- read.csv("data/cpue_data.csv")

cpue <- cmydas_data %>% 
  select(year, cpue) 

# Get annual growth rates for different periods
fishery_rate <- getGrowthRate(data = cpue, start_year = 1952, end_year = 1982)

# As with mice analyses and NLS, we'll calculate from the last stage of the 
# fishery, as initial conservation efforts began in the late 1970s
conservation_rate <- getGrowthRate(data = cpue, start_year = 1978, 
                                   end_year = 2018)

# Evaluate the growth rates by plugging in to the growth model and comparing to 
# observed values
fishery_mod <- getRateComp(data = cpue, start_year = 1952, end_year = 1982, 
                           rate = fishery_rate)

conservation_mod <-  getRateComp(data = cpue, start_year = 1978, 
                                 end_year = 2018, rate = conservation_rate)

# We can compare the proportionality between the growth rate during fishery
# exploitation and during conservation 

abs(fishery_rate/conservation_rate)

