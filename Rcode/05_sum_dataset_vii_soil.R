###############################################################################
# Script name: 05_sum_dataset_vii_soil.R
# Purpose: Summary metrics of Dataset vii: Soil temperature and moisture
# Author: Morales-González et al.
# Date: 26 May 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset vii.
###############################################################################

###############
# LOAD PACKAGES
###############
library(skimr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################
clean_soil <- read.csv(gzfile(paste0(pathRepo, "clean_datasets/Dataset_vii_clean.csv.gz")))

# Inspect clean dataset structure and summary statistics
skim(clean_soil)

########################
# LOAD CREATED FUNCTIONS
########################

summaryF <- function(clean_soil){
  
  # ============ #
  # 1. Summaries #
  # ============ #
  
  # Total records
  total_records <- nrow(clean_soil)
  
  # Range for temperature values
  temp_range <- clean_soil %>%
    filter(sensor_name %in% c("TMS_T1","TMS_T2","TMS_T3")) %>%
    summarise(
      min_temp = min(value, na.rm = TRUE),
      max_temp = max(value, na.rm = TRUE)
    )
  
  # Range for moisture values
  moist_range <- clean_soil %>%
    filter(sensor_name %in% c("VWC_moist")) %>%
    summarise(
      min_temp = min(value, na.rm = TRUE),
      max_temp = max(value, na.rm = TRUE)
    )
  
  # =========== #
  # 2. Plots    #
  # =========== #  
  
  # Prepare dataset for plotting
  
  clean_soil_sub <- clean_soil %>%
    filter( # keep only a subset of the data for plotting
      serial_number == "95145246" |
        (serial_number == "95139712" & sensor_name == "TMS_T1")
    ) %>%
    mutate(
      hour = hour(timestamp), # column with hour
      day_night = ifelse(hour >= 7 & hour < 19, "day", "night"), # column with day/night (day is defined as 07:00–19:00, night otherwise)
      day = yday(timestamp)  # day of year for cyclic smoother
    )

  
  clean_soil_sub$timestamp <- as.POSIXct(clean_soil_sub$timestamp, tz = "UTC")
  clean_soil_sub <- clean_soil_sub[order(clean_soil_sub$height, clean_soil_sub$timestamp), ]
  
  clean_soil_sub %>%
    distinct(serial_number, sensor_name, height) %>%
    arrange(height)
  
  clean_soil_sub$height <- factor(
    clean_soil_sub$height,
    levels = c("-50cm", "-6cm", "+2cm", "+15cm", "0 to -15cm")
  )
  
  soil_temp <- clean_soil_sub %>%
    filter(sensor_name != "VWC_moist")
  
  soil_moist <- clean_soil_sub %>%
    filter(sensor_name == "VWC_moist")
  
  
  # Plot
  
  Sys.setlocale("LC_TIME", "C")
  
  plot_a <- ggplot(
    soil_temp,
    aes(
      x = timestamp,
      y = value,
      color = day_night
    )
  ) +
    geom_line(linewidth = 0.4, alpha = 0.9) +
    facet_wrap(~ height, scales = "free_y", ncol = 1) +
    scale_color_manual(
      values = c("day" = "#E69F00", "night" = "#0072B2"),
      name = NULL
    ) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "2 months"
    ) +
    labs(
      x = NULL,
      y = "Soil temperature (ºC)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.spacing = unit(0.8, "lines")
    )
  
  plot_a <- plot_a +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  plot_b <- ggplot(
    soil_moist,
    aes(
      x = timestamp,
      y = value,
      color = day_night
    )
  ) +
    geom_line(linewidth = 0.4, alpha = 0.9) +
    facet_wrap(~ height, scales = "free_y", ncol = 1) +
    scale_color_manual(
      values = c("day" = "#E69F00", "night" = "#0072B2"),
      name = NULL
    ) +
    scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "2 months"
    ) +
    labs(
      x = NULL,
      y = "Soil moisture (VWC)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.spacing = unit(0.8, "lines")
    )
  
  plot_b <- plot_b + theme(legend.position = "none")
  
  final_plot <- plot_a / plot_b +
    plot_layout(heights = c(4, 1)) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))
  
  final_plot

  
  return(list(total_records,temp_range,moist_range, final_plot))
}

############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(clean_soil)
total_records <- resSum[[1]]
temp_range <- resSum[[2]]
moist_range <- resSum[[3]]
plot <- resSum[[4]]

#################
# INSPECT OUTPUTS (manually)
#################

# Total number of records
total_records

# Range values of temperature
temp_range

# Range values of moisture
moist_range

##############
# SAVE PLOTS
##############

fig_path <- paste0(pathRepo, "figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

graphics.off()
quartz(width = 8, height = 9)
plot

dev.copy(png, "dataset_vii.png", width = 8, height = 9, units = "in", res = 400)
dev.off()

