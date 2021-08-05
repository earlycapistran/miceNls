# ---
# title: CPUE descriptive statistics
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: March 2021
# Script and data info:
#   - This script calculates descriptive statistics and Mann-Whitney U test
#     for CPUE from the LEK and monitoring datasets.
#   - Data was obtained from ecological monitoring (1995-2018) in Bahía de los 
#     Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo Tortuguero
#     de Bahía de los Ángeles, and Comisión Nacional de Áreas Naturales
#     Protegidas and LEK-derived data is from Early-Capistrán et al. 
#     (PeerJ, 2020)
# - - -

# devtools::install('consLettersUtils')

# Load libraries and source functions
library("tidyverse")
library("devtools")
library("here")

# Install and load consLettersUtils package
# devtools::install('consLettersUtils')
devtools::load_all('consLettersUtils')

#  Load data and prepare data -------------------------------------------------
# Load mean annual CPUE values used for analyses
cpue_analysis <- read.csv("data/cpue_data.csv", header=TRUE) %>% 
  na.omit()

# Prepare LEK datasets
lek_data_analysis <- cpue_analysis %>%  
  filter(type=="LEK")

# Prepare Monitoring dataset
mon_data_analysis <- cpue_analysis %>%  
  filter(type=="Monitoring")

# Get descriptive statistics
getDescStats(lek_data_analysis, cpue)
getDescStats(mon_data_analysis, cpue)

# Apply Mann-Whitney test to mean annual values from LEK and monitoring
wilcox.test(x = lek_data_analysis$cpue, y = mon_data_analysis$cpue, paired = FALSE, 
            conf.int = 0.95, exact = FALSE)

# Plot grouped data used for Mann-Whitney test ----------
# Calculate confidence intervals
type_ci <- cpue_analysis %>%
  group_by(type) %>%
  summarise(n=n(), 
            mean=mean(cpue),
            sd=sd(cpue)) %>%
  mutate(se = sd/sqrt(n), # Calculate standard error
         tScore = qt(p = 0.05/2, df = n -1, lower.tail=F),
         ci = tScore * se) %>% 
  suppressWarnings()

# Add labels to the factor "stage" for plotting
type_ci$type<- factor(type_ci$type, levels = c("LEK", "Monitoring"),
                        labels = c("LEK (1952-1983)", 
                                   "Monitoring (1995-2018)"))

# Make bar plot with error bars
ggplot(type_ci, aes(x = type, y = mean, fill = type)) +  
  geom_bar(position = position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2) +
  theme_cmydas() +
  labs(y = "CPUE (turtles/12 hr)", 
       color = "Source", 
       shape = "Data type",
       text=element_text(size=5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x=element_blank())
