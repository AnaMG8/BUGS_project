###############################################################################
# Script name: 08_sum_dataset_iii_invertebrates.R
# Purpose: Summary metrics of Dataset iii: Invertebrate biomass for subterranean traps
# Author: Morales-González et al.
# Date: 29 May 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset iii.
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
library(cowplot)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################

biom_clean <- read_excel(paste0(pathRepo, "clean_datasets/Dataset_iii_clean.xlsx"))

# Inspect clean dataset structure and summary statistics
skim(biom_clean)

########################
# LOAD CREATED FUNCTIONS
########################

# Function for plotting
make_panel <- function(data, yvar, ylab, ymax,show_x = TRUE,show_y_title = TRUE,title = NULL) {
  
  ggplot(data, aes(x = month, y = .data[[yvar]], fill = trap_type)) +
    geom_boxplot(
      aes(group = interaction(month, trap_type)),
      outlier.shape = NA,
      width = 20,
      position = position_dodge2(
        width = 30,
        preserve = "single",
        padding = 0.35
      ),
      alpha = 0.85,
      linewidth = 0.35
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_fill_manual(
      values = c(
        "Double-stratified" = "#2A9D8F",
        "Triple-stratified" = "#E9C46A"
      ),
      name = NULL
    ) +
    scale_x_date(
      breaks = seq(
        from = min(data$month, na.rm = TRUE),
        to = max(data$month, na.rm = TRUE),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = ylab,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, size = 11, colour = "grey30")
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = 11, colour = "grey30"),
      axis.title.y = if (show_y_title) {
        element_text(size = 13)
      } else {
        element_blank()
      },
      legend.position = "top",
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.margin = margin(5, 10, 5, 10)
    )
}

# Function to get summaries
summaryF <- function(biom_clean) {
  
  # ================================ #
  # 1. General dataset information   #
  # ================================ #
  
  dataset_summary <- biom_clean %>%
    summarise(
      n_sampling_occasions = n(),
      n_trap_strata = n_distinct(trap_id),
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
    mutate(empty_trap = number == 0 & weight == 0) %>%
    summarise(
      n_empty_traps = sum(empty_trap, na.rm = TRUE),
      perc_empty_traps = 100 * n_empty_traps / n()
    )
  
  # ============================================= #
  # 4. Abundance per stratum and sampling occasion #
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
  # 5. Biomass per stratum and sampling occasion #
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
  # 6. Sampling occasions per stratum #
  # ================================ #
  
  sampling_per_trap <- biom_clean %>%
    group_by(trap_id) %>%
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
    mutate(
      month = as.Date(floor_date(timestamp, unit = "month")),
      trap_type = case_when(
        str_detect(trap_id, "L01L") ~ "Double-stratified",
        str_detect(trap_id, "L05L") ~ "Triple-stratified",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(trap_type))
  
  # Split periods
  biom_plot_1 <- biom_plot %>%
    filter(timestamp < as.POSIXct("2024-11-01", tz = "UTC"))
  
  biom_plot_2 <- biom_plot %>%
    filter(timestamp >= as.POSIXct("2024-11-01", tz = "UTC"))
  
  # Create panels
  pA1 <- make_panel(
    data = biom_plot_1,
    yvar = "number",
    ylab = "Total abundance\n(no. individuals)",
    ymax = 130,
    show_x = FALSE,
    title = "A"
  )
  
  pA2 <- make_panel(
    data = biom_plot_2,
    yvar = "number",
    ylab = NULL,
    ymax = 7,
    show_x = FALSE,
    title = "B"
  )
  
  pB1 <- make_panel(
    data = biom_plot_1,
    yvar = "weight",
    ylab = "Total biomass (g)",
    ymax = 0.65,
    show_x = TRUE,
    title = "C"
  )
  
  pB2 <- make_panel(
    data = biom_plot_2,
    yvar = "weight",
    ylab = NULL,
    ymax = 0.06,
    show_x = TRUE,
    title = "D"
  )
  
  # Combine into one figure
  top_row <- pA1 + pA2 +
    plot_layout(widths = c(0.7, 1.4))
  
  bottom_row <- pB1 + pB2 +
    plot_layout(widths = c(0.7, 1.4))
  
  plot_no_legend <- top_row / bottom_row
  
  plot_dataset_iii <- plot_no_legend +
    plot_layout(
      guides = "collect"
    ) &
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
      ) &
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
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
    plot_dataset_iii
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
plot_dataset_iii <- resSum[[9]]

############################
# INSPECT OUTPUTS MANUALLY
############################

# General dataset information
dataset_summary

# Total number of individuals and total biomass
total_summary

# Number and percentage of empty stratum
empty_traps_summary

# Number of individuals per stratum and sampling occasion
abundance_summary

# Biomass per stratum and sampling occasion
biomass_summary

# Sampling occasions per stratum
sampling_per_trap

# Summary of sampling occasions per stratum
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

plot_dataset_iii

ggsave(
  filename = "dataset_iii.png",
  plot = plot_dataset_iii,
  width = 9,
  height = 7,
  dpi = 400
)

###
# BORRADOR

# Function for plotting. no distingue entre double and triple stratified y solo muestra boxplots
make_panel <- function(data, yvar, ylab, ymax, show_x = TRUE,show_y_title = TRUE,title = NULL) { 
  
  ggplot(data, aes(x = month, y = .data[[yvar]], group = month)) +
    geom_boxplot(
      outlier.shape = NA,
      width = 18,
      fill = ifelse(yvar == "number", "#2A9D8F", "#A44A3F"),
      colour = ifelse(yvar == "number", "#2A9D8F", "#A44A3F"),
      alpha = 0.85,
      linewidth = 0.35
    ) +
    coord_cartesian(ylim = c(0, ymax)) + 
    scale_x_date(
      breaks = seq(
        from = min(data$month, na.rm = TRUE),
        to = max(data$month, na.rm = TRUE),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = ylab,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, size = 11, colour = "grey30")
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = 11, colour = "grey30"),
      axis.title.y = if (show_y_title) {
        element_text(size = 13)
      } else {
        element_blank()
      },
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.margin = margin(5, 10, 5, 10)
    )
}
# Function to get summaries. no distingue entre double and triple stratified y solo muestra boxplots
summaryF <- function(biom_clean){
  
  # ================================ #
  # 1. General dataset information   #
  # ================================ #
  
  dataset_summary <- biom_clean %>%
    summarise(
      n_sampling_occasions = n(),
      n_pitall_traps = n_distinct(trap_id),
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
  # 4. Abundance per stratum and sampling occasion   #
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
  # 5. Biomass per stratum and sampling occasion  #
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
    group_by(trap_id) %>%
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
  
  # ---------- Split periods ----------
  
  biom_plot_1 <- biom_plot %>%
    filter(timestamp < as.POSIXct("2024-11-01", tz = "UTC"))
  
  biom_plot_2 <- biom_plot %>%
    filter(timestamp >= as.POSIXct("2024-11-01", tz = "UTC"))
  
  # ---------- Create panels ----------
  
  pA1 <- make_panel(
    data = biom_plot_1,
    yvar = "number",
    ylab = "Total abundance\n(no. individuals)",
    ymax = 130,
    show_x = FALSE,
    title = "A"
  )
  
  pA2 <- make_panel(
    data = biom_plot_2,
    yvar = "number",
    ylab = "Total abundance\n(no. individuals)",
    ymax = 7, 
    show_x = FALSE,
    title = "B"
    #show_y_title = FALSE
  )
  
  pB1 <- make_panel(
    data = biom_plot_1,
    yvar = "weight",
    ylab = "Total biomass (g)",
    ymax = 0.65, 
    show_x = TRUE,
    title = "C"
  )
  
  pB2 <- make_panel(
    data = biom_plot_2,
    yvar = "weight",
    ylab = "Total biomass (g)",
    ymax = 0.06,
    show_x = TRUE,
    title = "D"
    #show_y_title = FALSE
  )
  
  # ---------- Combine into one figure ----------
  
  plot_dataset_iii <- (pA1 | pA2) / (pB1 | pB2)
  
  
  return(list(
    dataset_summary,
    total_summary,
    empty_traps_summary,
    abundance_summary,
    biomass_summary,
    sampling_per_trap,
    sampling_per_trap_summary,
    monthly_summary,
    plot_dataset_iii
  ))
}

# Function for plotting. no distingue entre double and triple stratified y muestra individual data
make_panel <- function(data, yvar, ylab, show_x = TRUE,show_y_title = TRUE,title = NULL) { 
  
  ggplot(data, aes(x = month, y = .data[[yvar]], group = month)) +
    geom_point(
      data = data,
      aes(x = month, y = .data[[yvar]]),
      inherit.aes = FALSE,
      position = position_jitter(width = 5, height = 0),
      alpha = 0.8,
      size = 0.7,
      colour = "grey40"
    ) + 
    geom_boxplot(
      outlier.shape = NA,
      width = 18,
      fill = ifelse(yvar == "number", "#2A9D8F", "#A44A3F"),
      colour = ifelse(yvar == "number", "#2A9D8F", "#A44A3F"),
      alpha = 0.85,
      linewidth = 0.35
    ) +
    scale_x_date(
      breaks = seq(
        from = min(data$month, na.rm = TRUE),
        to = max(data$month, na.rm = TRUE),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = ylab,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, size = 11, colour = "grey30")
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = 11, colour = "grey30"),
      axis.title.y = if (show_y_title) {
        element_text(size = 13)
      } else {
        element_blank()
      },
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.margin = margin(5, 10, 5, 10)
    )
}
# Function to get summaries. no distingue entre double and triple stratified y muestra individual data
summaryF <- function(biom_clean){
  
  # ================================ #
  # 1. General dataset information   #
  # ================================ #
  
  dataset_summary <- biom_clean %>%
    summarise(
      n_sampling_occasions = n(),
      n_pitall_traps = n_distinct(trap_id),
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
  # 4. Abundance per stratum and sampling occasion   #
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
  # 5. Biomass per stratum and sampling occasion  #
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
    group_by(trap_id) %>%
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
  
  # ---------- Split periods ----------
  
  biom_plot_1 <- biom_plot %>%
    filter(timestamp < as.POSIXct("2024-11-01", tz = "UTC"))
  
  biom_plot_2 <- biom_plot %>%
    filter(timestamp >= as.POSIXct("2024-11-01", tz = "UTC"))
  
  # ---------- Create panels ----------
  
  pA1 <- make_panel(
    data = biom_plot_1,
    yvar = "number",
    ylab = "Total abundance\n(no. individuals)",
    show_x = FALSE,
    title = "A"
  )
  
  pA2 <- make_panel(
    data = biom_plot_2,
    yvar = "number",
    ylab = NULL,
    show_x = FALSE,
    title = "B"
    #show_y_title = FALSE
  )
  
  pB1 <- make_panel(
    data = biom_plot_1,
    yvar = "weight",
    ylab = "Total biomass (g)",
    show_x = TRUE,
    title = "C"
  )
  
  pB2 <- make_panel(
    data = biom_plot_2,
    yvar = "weight",
    ylab = NULL,
    show_x = TRUE,
    title = "D"
    #show_y_title = FALSE
  )
  
  # ---------- Combine into one figure ----------
  
  plot_dataset_iii <- (pA1 | pA2) / (pB1 | pB2)
  
  
  return(list(
    dataset_summary,
    total_summary,
    empty_traps_summary,
    abundance_summary,
    biomass_summary,
    sampling_per_trap,
    sampling_per_trap_summary,
    monthly_summary,
    plot_dataset_iii
  ))
}

# Function for plotting. distingue entre double and triple stratified y muestra individual data
make_panel <- function(data, yvar, ylab,show_x = TRUE, show_y_title = TRUE,title = NULL) {
  
  ggplot(data, aes(x = month, y = .data[[yvar]], fill = trap_type)) +
    geom_point(
      aes(group = trap_type),
      position = position_jitterdodge(
        jitter.width = 5,
        jitter.height = 0,
        dodge.width = 24
      ),
      alpha = 0.8,
      size = 0.7,
      colour = "grey40"
    ) +
    geom_boxplot(
      aes(group = interaction(month, trap_type)),
      outlier.shape = NA,
      width = 28,
      position = position_dodge(width = 24),
      alpha = 0.85,
      linewidth = 0.35
    ) +
    scale_fill_manual(
      values = c(
        "Double-stratified" = "#2A9D8F",
        "Triple-stratified" = "#E9C46A"
      ),
      name = NULL
    ) +
    scale_x_date(
      breaks = seq(
        from = min(data$month, na.rm = TRUE),
        to = max(data$month, na.rm = TRUE),
        by = "2 months"
      ),
      date_labels = "%b %Y"
    ) +
    labs(
      x = NULL,
      y = ylab,
      title = title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, size = 11, colour = "grey30")
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = 11, colour = "grey30"),
      axis.title.y = if (show_y_title) {
        element_text(size = 13)
      } else {
        element_blank()
      },
      legend.position = "top",
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.margin = margin(5, 10, 5, 10)
    )
}
# Function to get summaries. distingue entre double and triple stratified y muestra individual data
summaryF <- function(biom_clean) {
  
  dataset_summary <- biom_clean %>%
    summarise(
      n_records = n(),
      n_trap_strata = n_distinct(trap_id),
      timestamp_start = min(timestamp, na.rm = TRUE),
      timestamp_end = max(timestamp, na.rm = TRUE)
    )
  
  total_summary <- biom_clean %>%
    summarise(
      total_individuals = sum(number, na.rm = TRUE),
      total_biomass_g = sum(weight, na.rm = TRUE)
    )
  
  empty_traps_summary <- biom_clean %>%
    mutate(empty_trap = number == 0 & weight == 0) %>%
    summarise(
      n_empty_traps = sum(empty_trap, na.rm = TRUE),
      perc_empty_traps = 100 * n_empty_traps / n()
    )
  
  abundance_summary <- biom_clean %>%
    summarise(
      mean_number = mean(number, na.rm = TRUE),
      median_number = median(number, na.rm = TRUE),
      perc_2.5_number = quantile(number, 0.025, na.rm = TRUE),
      perc_97.5_number = quantile(number, 0.975, na.rm = TRUE),
      min_number = min(number, na.rm = TRUE),
      max_number = max(number, na.rm = TRUE)
    )
  
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
  
  sampling_per_trap <- biom_clean %>%
    group_by(trap_id) %>%
    summarise(
      n_records = n(),
      timestamp_start = min(timestamp, na.rm = TRUE),
      timestamp_end = max(timestamp, na.rm = TRUE),
      .groups = "drop"
    )
  
  sampling_per_trap_summary <- sampling_per_trap %>%
    summarise(
      mean_records = mean(n_records),
      median_records = median(n_records),
      min_records = min(n_records),
      max_records = max(n_records),
      perc_2.5_records = quantile(n_records, 0.025),
      perc_97.5_records = quantile(n_records, 0.975)
    )
  
  monthly_summary <- biom_clean %>%
    mutate(month = floor_date(timestamp, unit = "month")) %>%
    group_by(month) %>%
    summarise(
      n_records = n(),
      total_individuals = sum(number, na.rm = TRUE),
      total_biomass_g = sum(weight, na.rm = TRUE),
      mean_number = mean(number, na.rm = TRUE),
      median_number = median(number, na.rm = TRUE),
      mean_weight_g = mean(weight, na.rm = TRUE),
      median_weight_g = median(weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  Sys.setlocale("LC_TIME", "C")
  
  biom_plot <- biom_clean %>%
    mutate(
      month = as.Date(floor_date(timestamp, unit = "month")),
      trap_type = case_when(
        str_detect(trap_id, "L01L") ~ "Double-stratified",
        str_detect(trap_id, "L05L") ~ "Triple-stratified",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(trap_type))
  
  biom_plot_1 <- biom_plot %>%
    filter(timestamp < as.POSIXct("2024-11-01", tz = "UTC"))
  
  biom_plot_2 <- biom_plot %>%
    filter(timestamp >= as.POSIXct("2024-11-01", tz = "UTC"))
  
  pA1 <- make_panel(
    data = biom_plot_1,
    yvar = "number",
    ylab = "Total abundance\n(no. individuals)",
    show_x = FALSE,
    title = "A"
  )
  
  pA2 <- make_panel(
    data = biom_plot_2,
    yvar = "number",
    ylab = NULL,
    show_x = FALSE,
    title = "B"
  )
  
  pB1 <- make_panel(
    data = biom_plot_1,
    yvar = "weight",
    ylab = "Total biomass (g)",
    show_x = TRUE,
    title = "C"
  )
  
  pB2 <- make_panel(
    data = biom_plot_2,
    yvar = "weight",
    ylab = NULL,
    show_x = TRUE,
    title = "D"
  )
  
  plot_dataset_iii <- (pA1 | pA2) / (pB1 | pB2) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "top"
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
    plot_dataset_iii
  ))
}