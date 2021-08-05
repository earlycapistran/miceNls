# ---
# title: Annotated plot patchwork
# author: Michelle María Early Capistrán 
# email:  earlycapistran@comunidad.unam.mx
# date: January 2021
# Script and data info:
#   - This script joins two annotated plots into a single panel. 
# - - -

# Load libraries and packages
library("tidyverse")
library("patchwork")
library("here")
library("devtools")
load_all("consLettersUtils")

annot_cpue_plot <- readRDS("results/annotated_cpue_plot.rds")
annot_size_plot <- readRDS("results/size_plot_annotated.rds")

# Put plots together
annot_cpue_plot / annot_size_plot + 
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 16),
        text = element_text('lato'))

# Save plot as svg
ggsave("figures/annotated_stacked_plot.svg", device = svg)
