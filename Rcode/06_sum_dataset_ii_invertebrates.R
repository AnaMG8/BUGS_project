###############################################################################
# Script name: 06_sum_dataset_ii_invertebrates.R
# Purpose: Summary metrics of Dataset ii: Invertebrate biomass for pitfall traps
# Author: Morales-González et al.
# Date: 27 May 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset ii.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(skimr)
library(ggplot2)
library(lubridate)
library(patchwork)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################

biom_clean <- read_excel(paste0(pathRepo, "clean_datasets/Dataset_ii_clean.xlsx"))

# Inspect clean dataset structure and summary statistics
skim(biom_clean)

########################
# LOAD CREATED FUNCTIONS
########################

# Function to get summaries
summaryF <- function(biom_clean){
  
  # ================================ #
  # 1. General dataset information   #
  # ================================ #
  
  dataset_summary <- biom_clean %>%
    mutate(
      trap_id_base = str_remove(trap_id, "D$")
    ) %>%
    summarise(
      n_sampling_occasions = n(),
      n_pitfall_traps = n_distinct(trap_id_base),
      timestamp_start = min(timestamp, na.rm = TRUE),
      timestamp_end = max(timestamp, na.rm = TRUE)
    )
  
  # ================================ #
  # 2. Total abundance and biomass   #
  # ================================ #
  
  total_summary <- biom_clean %>%
    summarise(
      total_individuals = sum(number, na.rm = TRUE),
      total_biomass_g = sum(weight, na.rm = TRUE)
    )
  
  # ================================ #
  # 3. Empty traps                   #
  # ================================ #
  
  empty_traps_summary <- biom_clean %>%
    mutate(
      empty_trap = number == 0 & weight == 0
    ) %>%
    summarise(
      n_empty_traps = sum(empty_trap, na.rm = TRUE),
      perc_empty_traps = 100 * n_empty_traps / n()
    )
  
  # ============================================= #
  # 4. Abundance per trap and sampling occasion   #
  # ============================================= #
  
  abundance_summary <- biom_clean %>%
    summarise(
      mean_number = mean(number, na.rm = TRUE),
      median_number = median(number, na.rm = TRUE),
      perc_2.5_number = quantile(number, 0.025, na.rm = TRUE),
      perc_97.5_number = quantile(number, 0.975, na.rm = TRUE),
      min_number = min(number, na.rm = TRUE),
      max_number = max(number, na.rm = TRUE)
    )
  
  # ========================================== #
  # 5. Biomass per trap and sampling occasion  #
  # ========================================== #
  
  biomass_summary <- biom_clean %>%
    summarise(
      mean_weight_g = mean(weight, na.rm = TRUE),
      median_weight_g = median(weight, na.rm = TRUE),
      perc_2.5_weight_g = quantile(weight, 0.025, na.rm = TRUE),
      perc_97.5_weight_g = quantile(weight, 0.975, na.rm = TRUE),
      min_weight_g = min(weight, na.rm = TRUE),
      max_weight_g = max(weight, na.rm = TRUE),
      n_missing_weight = sum(is.na(weight))
    )
  
  # ================================ #
  # 6. Sampling occasions per trap   #
  # ================================ #
  
  sampling_per_trap <- biom_clean %>%
    mutate(
      trap_id_base = str_remove(trap_id, "D$")
    ) %>%
    group_by(trap_id_base) %>%
    summarise(
      n_sampling_occasions = n(),
      timestamp_start = min(timestamp, na.rm = TRUE),
      timestamp_end = max(timestamp, na.rm = TRUE),
      .groups = "drop"
    )
  
  sampling_per_trap_summary <- sampling_per_trap %>%
    summarise(
      mean_sampling_occasions = mean(n_sampling_occasions),
      median_sampling_occasions = median(n_sampling_occasions),
      min_sampling_occasions = min(n_sampling_occasions),
      max_sampling_occasions = max(n_sampling_occasions),
      perc_2.5_sampling_occasions = quantile(n_sampling_occasions, 0.025),
      perc_97.5_sampling_occasions = quantile(n_sampling_occasions, 0.975)
    )
  
  # ================================ #
  # 7. Monthly temporal summaries    #
  # ================================ #
  
  monthly_summary <- biom_clean %>%
    mutate(month = floor_date(timestamp, unit = "month")) %>%
    group_by(month) %>%
    summarise(
      n_sampling_occasions = n(),
      total_individuals = sum(number, na.rm = TRUE),
      total_biomass_g = sum(weight, na.rm = TRUE),
      mean_number = mean(number, na.rm = TRUE),
      median_number = median(number, na.rm = TRUE),
      mean_weight_g = mean(weight, na.rm = TRUE),
      median_weight_g = median(weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ================================ #
  # 8. Plots                         #
  # ================================ #
  
  Sys.setlocale("LC_TIME", "C")
  
  biom_plot <- biom_clean %>%
    mutate(month = as.Date(floor_date(timestamp, unit = "month")))
  
  # Panel A: abundance
  plot_number_month <- biom_plot %>%
    ggplot(aes(x = month, y = number, group = month)) +
    geom_boxplot(
      outlier.shape = NA,
      width = 18,
      fill = "#2A9D8F",
      colour = "#2A9D8F",
      alpha = 0.85,
      linewidth = 0.35
    ) +
    coord_cartesian(ylim = c(0, 18)) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = "Total abundance
(no. individuals)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = element_blank(),
      axis.title.y = element_text(size = 13),
      axis.text.y = element_text(size = 11, colour = "grey30"),
      plot.margin = margin(5, 10, 5, 10)
    )
  
  # Panel B: biomass
  plot_weight_month <- biom_plot %>%
    ggplot(aes(x = month, y = weight, group = month)) +
    geom_boxplot(
      outlier.shape = NA,
      width = 18,
      fill = "#A44A3F",
      colour = "#A44A3F",
      alpha = 0.85,
      linewidth = 0.35
    ) +
    coord_cartesian(ylim = c(0, 0.27)) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = "Total biomass (g)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11, colour = "grey30"),
      axis.title.y = element_text(size = 13),
      axis.text.y = element_text(size = 11, colour = "grey30"),
      plot.margin = margin(5, 10, 5, 10)
    )
  
  # Combine panels
  plot_dataset_ii <- plot_number_month / plot_weight_month +
    plot_annotation(tag_levels = "A") &
    theme(
      plot.tag = element_text(size = 18, face = "bold"),
      plot.tag.position = c(0.01, 0.98)
    )
  
  
  return(list(
    dataset_summary,
    total_summary,
    empty_traps_summary,
    abundance_summary,
    biomass_summary,
    sampling_per_trap,
    sampling_per_trap_summary,
    monthly_summary,
    plot_dataset_ii
  ))
}

############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(biom_clean)
dataset_summary <- resSum[[1]]
total_summary <- resSum[[2]]
empty_traps_summary <- resSum[[3]]
abundance_summary <- resSum[[4]]
biomass_summary <- resSum[[5]]
sampling_per_trap <- resSum[[6]]
sampling_per_trap_summary <- resSum[[7]]
monthly_summary <- resSum[[8]]
plot_dataset_ii <- resSum[[9]]

############################
# INSPECT OUTPUTS MANUALLY
############################

# General dataset information
dataset_summary

# Total number of individuals and total biomass
total_summary

# Number and percentage of empty traps
empty_traps_summary

# Number of individuals per trap and sampling occasion
abundance_summary

# Biomass per trap and sampling occasion
biomass_summary

# Sampling occasions per pitfall trap
sampling_per_trap

# Summary of sampling occasions per pitfall trap
sampling_per_trap_summary

# Monthly temporal summaries
monthly_summary

# Range of variables for data dictionary
summary(biom_clean)

############
# SAVE PLOTS
############

fig_path <- paste0(pathRepo, "figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

plot_dataset_ii

ggsave(
  filename = "dataset_ii.png",
  plot = plot_dataset_ii,
  width = 9,
  height = 7,
  dpi = 400
)

