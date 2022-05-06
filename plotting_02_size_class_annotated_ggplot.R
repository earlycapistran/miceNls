# ---
# title: Size Class Plots
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: March 2021
# Script and data info:
#   - This script plots size distribution along with key events in green 
#     turtle conservation and management. 
#   - Data consists of green turtle (Chelonia mydas) CPUE data from ecological
#     monitoring.
#   - Data were obtained from ecological monitoring (1995-2018) in Bahía de los 
#     Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo Tortuguero
#     de Bahía de los Ángeles, and Comisión Nacional de Áreas Naturales
#     Protegidas.
# - - -

# Check if required libraries are installed and install
# if necessary

# packages <- c("ggthemes", "gginnards", "patchwork", "tidyverse")
# 
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }
#
# devtools::install('consLettersUtils')

# Load libraries and packages
library("ggthemes")
library("tidyverse")
library("gginnards")
library("here")
library("devtools")
library("scico")
load_all("consLettersUtils")

# Load and prepare data
size_data = read.csv("data/morphometric_data.csv")

# Count turtles per  size class per year in BLA
class_per_year <- size_data %>%
  drop_na %>% 
  group_by(year, size_class) %>% 
  tally(name = "n_turtles") %>% 
  ungroup() 

# Generate a placeholder for missing values in size data
size_class_na <- size_data %>% 
  filter(is.na(size_class)) %>% 
  add_row(year = c(1952:1994, 2006:2008, 2016, 2017)) %>%
  mutate(placeholder = -0.5) %>% 
  select(year, placeholder) %>% 
  arrange(year)

# Plot size class data --------------------------------------------------------
# Bar chart of adults and juveniles per year
size_plot <- ggplot(
  class_per_year, 
  aes(x = year, y = n_turtles, fill = size_class)) +
  geom_col(position = "dodge")  +
  xlim(1950, 2020) +
  labs(x = "Year", y = "Turtles / year", fill = "Life stage") +
  theme_cmydas()
size_plot

# Test with scientific colour maps
scico_palette_show()

size_plot <- ggplot(
  class_per_year, 
  aes(x = year, y = n_turtles, fill = size_class)) +
  scale_fill_scico_d(palette = "cork", begin = 0.2, end = 0.8) +
  geom_col(position = "dodge")  +
  xlim(1995, 2020) +
  labs(x = "Year", y = "Turtles / year", fill = "Life stage") +
  theme_cmydas()
size_plot

# Add placeholders for 'na' values
size_plot_na <- size_plot +
  geom_col(
    size_class_na, 
    mapping = aes(x = year, y = placeholder, fill = "placeholder")
    ) +
  scale_fill_manual(
    values = c("#FBA63B", "#2EBEB4", "#595959"),
    labels = c("Adult", "Juvenile", "NA")
    ) +
  theme(
    legend.box = "vertical",
    legend.position = "right",
    legend.spacing.y = unit(3, "mm")
    ) 
size_plot_na

# Annotate dates for key events -----------------------------------------------
# Label 1: Beginning of permanent sea turtle conservation efforts in BLA
# Label 2: Nesting beach protection
# Label 3: Permit closure for C. mydas
# Label 4: Permanent ban on sea turtle captures in Mexico
# Label 5: Start of in-water monitoring at BLA
size_plot_annotated <- size_plot_na + 
  annotate("label", 
           x = c(1979, 1979, 1983, 1990, 1995), 
           y = c(20,25, 25, 25, 25), 
           label = c("1", "2", "3", "4", "5") , 
           colour = "#363636", fill = "white", 
           family = "lato", fontface = "bold") +
  annotate("segment",
           x = c(1979, 1983, 1990, 1995),
           xend = c(1979, 1983, 1990, 1995),
           y = c(17, 22, 22, 22),
           yend = c( 0.5, 0.5, 0.5,18),
           color = "#363636",
           arrow = arrow(length = unit(3, "mm")))
size_plot_annotated

# Save plot to file
saveRDS(size_plot_annotated, file = "results/size_plot_annotated.rds")

