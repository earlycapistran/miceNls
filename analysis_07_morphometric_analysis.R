# ---
# title: Morphometric analyses
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: March 2021
# Script and data info:
#   - This script calculates descriptive statistics and Mann-Whitney U test
#     for size distribution in the monitoring dataset.
#   - Data was obtained from ecological monitoring (1995-2018) in Bahía de los 
#     Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo Tortuguero
#     de Bahía de los Ángeles, and Comisión Nacional de Áreas Naturales
#     Protegidas.
# - - -

# Load libraries and source functions
library("tidyverse")
library("car")
library("devtools")
load_all("~/SpiderOak Hive/articulos/arbitrados/conservation_letters/consLettersUtils")


source("R/functions/getDescStats.R")
source("R/functions/theme_cmydas.R")

# Load and prepare data
size_data = read.csv("data/morphometric_final.csv")

size_data <- size_data %>% 
  mutate(period = as.factor(ifelse(year %in% c(1995:2005), 
                                  "period_1", "period_2"))) 

# Split data into two periods (1995-2005 and 2009-2018)
period_1 <- size_data %>% 
  filter(period == "period_1") %>% 
  select(year, ccl)

period_2 <- size_data %>% 
  filter(period == "period_2") %>% 
  select(year, ccl)

# Check distribution
hist(period_1$ccl)
hist(period_2$ccl)

# Descriptive statistics for periods and for all SCL values
desc_periods <- size_data %>% 
  group_by(period) %>% 
  summarize(mean = mean(ccl), 
            median = median(ccl),
            sd = sd(ccl), 
            min = min(ccl), 
            max = max(ccl),
            n = length(ccl))

desc_all <- getDescStats(size_data, ccl)
desc_all

# Calculate percentages of adults and juveniles in each period
class_per_year <- size_data %>%
  drop_na %>% 
  group_by(size_class, period) %>% 
  tally(name = "n_turtles") %>% 
  ungroup()

percent_classes <- class_per_year %>% 
  group_by(period, size_class) %>% 
  summarize(n_turtles = sum(n_turtles)) %>% 
  mutate(percent = n_turtles/sum(n_turtles) * 100)

# Normality test
shapiro.test(period_1$ccl)
qqPlot(period_1$ccl)

shapiro.test(period_2$ccl)
qqPlot(period_2$ccl)

# Apply Mann-Whitney-Wilcoxon test
wilcox.test(x = period_1$ccl, y = period_2$ccl, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)

# Plot grouped data used for Mann-Whitney test ----------
# Calculate confidence intervals
size_ci <- size_data %>%
  group_by(period) %>%
  summarise(n=n(), 
            mean=mean(ccl),
            sd=sd(ccl)) %>%
  mutate(se = sd/sqrt(n), # Calculate standard error
         tScore = qt(p = 0.05/2, df = n -1, lower.tail=F),
         ci = tScore * se) %>% 
  suppressWarnings()

# Add labels to the factor "period" for plotting
size_ci$period <- factor(size_ci$period, levels = c("period_1", "period_2"),
                             labels = c("Period 1 (1995-2005)", 
                                        "Period 2 (2009-2018)"))

# Make bar plot with error bars
ggplot(size_ci, aes(x = period, y = mean, fill = period)) +  
  geom_bar(position = position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2) +
  theme_cmydas() +
  labs(y = "Curved Carapace Length (cm)", 
       color = "Source", 
       shape = "Data type",
       text=element_text(size=5)) +
  theme_cmydas() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.title = element_blank(), 
        legend.position = "none",
        axis.title.x=element_blank())

