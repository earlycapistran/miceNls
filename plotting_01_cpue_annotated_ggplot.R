# ---
# title: Annotated ggplot
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: January 2021
# Script and data info:
#   - This script plots observed values for CPUE along
#     with key events in green turtle conservation and management. 
#   - Data consists of green turtle (Chelonia mydas) CPUE data from monitoring
#     and derived from LEK.
#   - Data were obtained from ecological monitoring (1995-2018) in 
#     Bahía de los Ángeles, Baja California, Mexico by Dr. J. Seminoff, Grupo 
#     Tortuguero de Bahía de los Ángeles, and Comisión Nacional de Áreas 
#     Naturales Protegidas and LEK-derived data is from Early-Capistrán et al. 
#     (PeerJ, 2020)
# - - -

# Install scientific colour maps
# devtools::install_github("thomasp85/scico")

# Load libraries and packages
library("ggthemes")
library("tidyverse")
library("gginnards")
library("here")
library("devtools")
library("scico")
library("grid")
load_all("consLettersUtils")

# Load and prepare data
cmydas_data = read.csv("data/cpue_data.csv", header=TRUE)

# Replace placeholders with negative values in dataframe
cmydas_na <- cmydas_data %>% 
  mutate(cpue = replace(cpue, is.na(cpue), -0.25))

# Base plot for CPUE -----------------------------------------------------

# Experiment: make everything from the single data frame
gradient_base <- ggplot() +
  # Filter positive values to plot CPUE
  geom_col(
    cmydas_na %>% 
      filter(cpue > 0), 
    mapping = aes(x = year, y= cpue, fill = cpue)
           ) +
  # Add scientific colour guide pallette to CPUE values
  scale_fill_scico(palette = 'oslo', begin = 0.2, end = 0.6, direction = -1) +
  # Plot placeholders for missing data
  geom_col(
    cmydas_na %>% 
      filter(cpue < 0), 
    mapping = aes(x = year, y= cpue, alpha = "Missing Data")
    ) +
  # Alpha scale is a placeholder to generate legend key. Alpha set to 1.
  scale_alpha_manual(name = NULL, values = c(1)) +
  # Adjust legend settings
  labs(x = "Year", y = "CPUE (turtles/12 hr)", fill = "CPUE") +
  theme_cmydas() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(3, "mm")
  ) +
  # Fix guide order
  guides(
    fill = guide_colourbar(order = 1),
    alpha = guide_legend(order = 2)
    )
gradient_base

# Generate a shaded area to distinguish LEK-derived values 
gradient_shade <- gradient_base +
  annotate(
    "rect", 
    xmin = 1950, xmax = 1983, ymin = 0, ymax = 20,
    alpha = 0.75, color="lightgrey", fill="lightgrey"
  ) 
gradient_shade

# Move shading layer to background
gradient_shade <- move_layers(gradient_shade, "GeomRect", position = "bottom")
gradient_shade

# Annotate dates for key events -----------------------------------------------
# Label 1: Beginning of permanent sea turtle conservation efforts in BLA
# Label 2: Nesting beach protection
# Label 3: Permit closure for C. mydas
# Label 4: Permanent ban on sea turtle captures in Mexico
# Label 5: Start of in-water monitoring at BLA
cpue_plot_annotated <- gradient_shade + 
  annotate("label", 
           x = c(1979, 1979, 1983, 1990, 1995), 
           y = c(8.5, 7, 7, 7, 7), 
           label = c("1", "2", "3", "4", "5") , 
           colour = "#363636", fill = "white", 
           family = "lato", fontface = "bold") +
  annotate("segment",
           x = c(1979, 1983, 1990, 1995),
           xend = c(1979, 1983, 1990, 1995),
           y = c(6, 6, 6, 6),
           yend = c(2, 0.5, 0.5,0.65),
           color = "#363636",
           arrow = arrow(length = unit(3, "mm"))) 
cpue_plot_annotated

# Save plot to file as rds for layouts
saveRDS(cpue_plot_annotated, file = "results/annotated_cpue_plot.rds")
