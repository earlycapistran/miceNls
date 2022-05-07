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
  add_row(year = c(2006:2008, 2016, 2017)) %>%
  mutate(placeholder = -1) %>% 
  select(year, placeholder) %>% 
  arrange(year)

# Plot size class data --------------------------------------------------------
# Bar chart of adults and juveniles per year
size_plot <- ggplot(
  class_per_year, 
  aes(x = year, y = n_turtles, fill = size_class)) +
  geom_col(position = "dodge")  +
  xlim(1994, 2019) +
  labs(x = "Year", y = "Turtles / year", fill = " ") +
  theme_cmydas()
size_plot

# Add placeholders for 'na' values
size_plot_na <- size_plot +
  geom_col(
    size_class_na, 
    mapping = aes(x = year, y = placeholder, fill = "placeholder")
    ) +
  scale_fill_manual(
    values = c("#4E79A7", "#F28E2b", "#595959"),
    labels = c("Adult", "Juvenile", "Missing Data")
    ) +
  theme(
    legend.box = "vertical",
    legend.position = "right",
    legend.spacing.y = unit(3, "mm")
    ) 
size_plot_na

# Annotate dates for key events -----------------------------------------------
size_plot_annotated <- size_plot_na + 
  annotate("text", 
           x = c(2010, 2010), 
           y = c(75, 70), 
           label = c(">30 years of nesting beach protection", 
                     ">20 years of full ban on captures"),
           hjust = 0,
           colour = "#363636", fill = "white", 
           family = "lato", fontface = "bold")
size_plot_annotated

# Generate a shaded area to distinguish LEK-derived values 
gradient_shade <- size_plot_annotated +
  annotate(
    "rect", 
    xmin = 2008, xmax = 2019, ymin = 0, ymax = 80,
    alpha = 0.75, color="lightgrey", fill="lightgrey"
  ) 
gradient_shade

# Move shading layer to background
gradient_shade <- move_layers(gradient_shade, "GeomRect", position = "bottom")
gradient_shade

# Save plot to file
saveRDS(size_plot_annotated, file = "results/size_plot_annotated.rds")

