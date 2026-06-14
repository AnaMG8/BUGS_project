###############################################################################
# Script name: 10_sum_dataset_iv_vegetation.R
# Purpose: Summary metrics of Dataset iv: Vegetation species composition
# Author: Morales-González et al.
# Date: 31 May 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset iv.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(readxl)
library(skimr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(cowplot)
library(grid)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################

clean_veg <- read_excel(paste0(pathRepo, "clean_datasets/Dataset_iv_clean.xlsx"))

# Inspect clean dataset structure and summary statistics
skim(clean_veg)

########################
# LOAD CREATED FUNCTIONS
########################

summaryF <- function(clean_veg){
  
  # ============================= #
  # 1. Define vegetation groups   #
  # ============================= #
  
  non_vegetation_categories <- c(
    "bare soil",
    "dead grass"
  )
  
  unidentified_categories <- c(
    "unknown vegetation",
    "unidentifiable stubs"
  )
  
  identified_taxa <- clean_veg %>%
    filter(
      !veg_taxon %in% c(
        non_vegetation_categories,
        unidentified_categories
      )
    ) %>%
    distinct(
      veg_taxon
    ) %>%
    pull(
      veg_taxon
    )
  
  # ============================= #
  # 2. Cover summaries            #
  # ============================= #
  
  # Define all pitfall trap x sampling occasion combinations
  sampling_events <- clean_veg %>%
    distinct(
      trap_id,
      month,
      year
    )
  
  # Sum cover by broad cover group within each sampling event
  cover_by_event <- clean_veg %>%
    mutate(
      cover_group = case_when(
        veg_taxon == "bare soil" ~ "bare_soil",
        veg_taxon == "dead grass" ~ "dead_grass",
        !veg_taxon %in% non_vegetation_categories ~ "vegetation"
      )
    ) %>%
    group_by(
      trap_id,
      month,
      year,
      cover_group
    ) %>%
    summarise(
      cover = sum(
        cover_percentage,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # Add zeros for cover groups absent from a sampling event
  cover_by_event <- sampling_events %>%
    crossing(
      cover_group = c(
        "bare_soil",
        "dead_grass",
        "vegetation"
      )
    ) %>%
    left_join(
      cover_by_event,
      by = c(
        "trap_id",
        "month",
        "year",
        "cover_group"
      )
    ) %>%
    mutate(
      cover = replace_na(
        cover,
        0
      )
    )
  
  # Calculate mean cover and percentiles including zeros
  cover_summary <- cover_by_event %>%
    group_by(
      cover_group
    ) %>%
    summarise(
      mean_cover = mean(
        cover,
        na.rm = TRUE
      ),
      perc_2.5 = quantile(
        cover,
        0.025,
        na.rm = TRUE
      ),
      perc_97.5 = quantile(
        cover,
        0.975,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # ============================= #
  # 3. Vegetation height summary  #
  # ============================= #
  
  height_summary <- clean_veg %>%
    distinct(
      trap_id,
      month,
      year,
      height
    ) %>%
    summarise(
      mean_height = mean(
        height,
        na.rm = TRUE
      ),
      perc_2.5 = quantile(
        height,
        0.025,
        na.rm = TRUE
      ),
      perc_97.5 = quantile(
        height,
        0.975,
        na.rm = TRUE
      )
    )
  
  # ============================= #
  # 4. Number of vegetation taxa  #
  # ============================= #
  
  n_taxa <- clean_veg %>%
    summarise(
      n_identified_taxa = n_distinct(
        veg_taxon[
          veg_taxon %in% identified_taxa
        ]
      ),
      n_species_level = n_distinct(
        veg_taxon[
          veg_taxon %in% identified_taxa &
            !grepl(" sp$", veg_taxon)
        ]
      ),
      n_genus_level = n_distinct(
        veg_taxon[
          veg_taxon %in% identified_taxa &
            grepl(" sp$", veg_taxon)
        ]
      ),
      n_unidentified_vegetation_categories = n_distinct(
        veg_taxon[
          veg_taxon == "unknown vegetation"
        ]
      ),
      n_unidentifiable_plant_remain_categories = n_distinct(
        veg_taxon[
          veg_taxon == "unidentifiable stubs"
        ]
      )
    )
  
  # ===================================================== #
  # 5. Mean number of taxa per pitfall and sampling event #
  # ===================================================== #
  
  taxa_per_event <- clean_veg %>%
    filter(
      veg_taxon %in% identified_taxa
    ) %>%
    group_by(
      trap_id,
      month,
      year
    ) %>%
    summarise(
      n_taxa = n_distinct(
        veg_taxon
      ),
      .groups = "drop"
    )
  
  taxa_per_event_summary <- taxa_per_event %>%
    summarise(
      mean_n_taxa = mean(
        n_taxa,
        na.rm = TRUE
      ),
      perc_2.5 = quantile(
        n_taxa,
        0.025,
        na.rm = TRUE
      ),
      perc_97.5 = quantile(
        n_taxa,
        0.975,
        na.rm = TRUE
      )
    )
  
  # ============================= #
  # 6. Most common taxa           #
  # ============================= #
  
  most_common_taxa <- clean_veg %>%
    filter(
      veg_taxon %in% identified_taxa
    ) %>%
    group_by(
      veg_taxon
    ) %>%
    summarise(
      n_pitfall_traps = n_distinct(
        trap_id
      ),
      .groups = "drop"
    ) %>%
    arrange(
      desc(
        n_pitfall_traps
      ),
      veg_taxon
    )
  
  top_common_taxa <- most_common_taxa %>%
    slice_head(
      n = 3
    )
  
  # ============================================= #
  # 7. Taxa with highest mean cover               #
  # ============================================= #
  
  mean_cover_taxa <- clean_veg %>%
    filter(
      veg_taxon %in% identified_taxa
    ) %>%
    group_by(
      veg_taxon
    ) %>%
    summarise(
      mean_cover = mean(
        cover_percentage,
        na.rm = TRUE
      ),
      perc_2.5 = quantile(
        cover_percentage,
        0.025,
        na.rm = TRUE
      ),
      perc_97.5 = quantile(
        cover_percentage,
        0.975,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    arrange(
      desc(mean_cover),
      veg_taxon
    )
  
  top_cover_taxa <- mean_cover_taxa %>%
    slice_head(
      n = 3
    )
  
  # ============================================= #
  # 8. Vegetation cover boxplots                  #
  # ============================================= #
  
  sampling_events <- clean_veg %>%
    distinct(
      trap_id,
      month,
      year
    ) %>%
    mutate(
      sampling = case_when(
        month == 6 & year == 2023 ~ "June 2023",
        month == 2 & year == 2024 ~ "February 2024"
      )
    )
  
  # ---------- Panel A: broad cover categories ---------- #
  
  broad_cover_plot_data <- clean_veg %>%
    mutate(
      cover_type = case_when(
        veg_taxon == "bare soil" ~ "bare soil",
        veg_taxon == "dead grass" ~ "dead grass",
        !veg_taxon %in% non_vegetation_categories ~ "vegetation"
      )
    ) %>%
    group_by(
      trap_id,
      month,
      year,
      cover_type
    ) %>%
    summarise(
      cover = sum(cover_percentage, na.rm = TRUE),
      .groups = "drop"
    )
  
  broad_cover_plot_data <- sampling_events %>%
    crossing(
      cover_type = c(
        "vegetation",
        "bare soil",
        "dead grass"
      )
    ) %>%
    left_join(
      broad_cover_plot_data,
      by = c(
        "trap_id",
        "month",
        "year",
        "cover_type"
      )
    ) %>%
    mutate(
      cover = replace_na(cover, 0),
      cover_type = factor(
        cover_type,
        levels = c(
          "vegetation",
          "bare soil",
          "dead grass"
        )
      )
    )
  
  # ---------- Panel B: top 8 identified taxa ---------- #
  
  top5_taxa_names <- clean_veg %>%
    filter(
      veg_taxon %in% identified_taxa,
      cover_percentage > 0
    ) %>%
    group_by(
      veg_taxon
    ) %>%
    summarise(
      n_pitfalls = n_distinct(
        trap_id
      ),
      .groups = "drop"
    ) %>%
    arrange(
      desc(n_pitfalls),
      veg_taxon
    ) %>%
    slice_head(
      n = 5
    ) %>%
    pull(
      veg_taxon
    )
  
  taxa_cover_plot_data <- clean_veg %>%
    filter(
      veg_taxon %in% top5_taxa_names,
      cover_percentage > 0
    ) %>%
    mutate(
      sampling = case_when(
        month == 6 & year == 2023 ~ "June 2023",
        month == 2 & year == 2024 ~ "February 2024"
      ),
      veg_taxon = factor(
        veg_taxon,
        levels = top5_taxa_names
      )
    )
  
  # Labels with species names in two lines
  taxa_cover_plot_data <- taxa_cover_plot_data %>%
    mutate(
      veg_taxon_label = stringr::str_replace(
        stringr::str_to_sentence(
          as.character(veg_taxon)
        ),
        " ",
        "\n"
      ),
      veg_taxon_label = factor(
        veg_taxon_label,
        levels = stringr::str_replace(
          stringr::str_to_sentence(
            top5_taxa_names
          ),
          " ",
          "\n"
        )
      )
    )
  
  # n labels: number of distinct pitfalls per taxon and sampling
  taxa_n_labels <- taxa_cover_plot_data %>%
    group_by(
      veg_taxon_label,
      sampling
    ) %>%
    summarise(
      n_pitfalls = n_distinct(trap_id),
      .groups = "drop"
    ) %>%
    mutate(
      y_position = max(
        taxa_cover_plot_data$cover_percentage,
        na.rm = TRUE
      ) * 1.08
    )
  
  broad_cover_plot_data <- broad_cover_plot_data %>%
    mutate(
      sampling = factor(
        sampling,
        levels = c(
          "June 2023",
          "February 2024"
        )
      )
    )
  
  taxa_cover_plot_data <- taxa_cover_plot_data %>%
    mutate(
      sampling = factor(
        sampling,
        levels = c(
          "June 2023",
          "February 2024"
        )
      )
    )
  
  plot_cover_A <- ggplot(
    broad_cover_plot_data,
    aes(
      x = cover_type,
      y = cover,
      fill = sampling
    )
  ) +
    geom_boxplot(
      position = position_dodge2(
        width = 0.75,
        preserve = "single",
        padding = 0.25
      ),
      width = 0.6,
      outlier.shape = NA,
      alpha = 0.85,
      linewidth = 0.35
    ) +
    scale_fill_manual(
      values = c(
        "June 2023" = "#8190A5",
        "February 2024" = "#A67C52"
      ),
      name = NULL
    ) +
    labs(
      x = NULL,
      y = "Cover (%)",
      title = "A"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        colour = "grey90",
        linewidth = 0.4
      ),
      axis.text.x = element_text(
        size = 11,
        colour = "grey30"
      ),
      axis.text.y = element_text(
        size = 11,
        colour = "grey30"
      ),
      axis.title.y = element_text(
        size = 13
      ),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0
      ),
      plot.margin = margin(5, 10, 5, 10)
    )
  
  plot_cover_B <- ggplot(
    taxa_cover_plot_data,
    aes(
      x = veg_taxon_label,
      y = cover_percentage,
      fill = sampling
    )
  ) +
    geom_boxplot(
      position = position_dodge2(
        width = 0.75,
        preserve = "single",
        padding = 0.25
      ),
      width = 0.6,
      outlier.shape = NA,
      alpha = 0.85,
      linewidth = 0.35
    ) +
    geom_text(
      data = taxa_n_labels,
      aes(
        x = veg_taxon_label,
        y = y_position,
        label = n_pitfalls,
        group = sampling
      ),
      position = position_dodge2(
        width = 0.75,
        preserve = "single",
        padding = 0.25
      ),
      size = 3.2,
      colour = "grey20",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(
        "June 2023" = "#8190A5",
        "February 2024" = "#A67C52"
      ),
      name = NULL
    ) +
    coord_cartesian(
      ylim = c(
        0,
        max(taxa_cover_plot_data$cover_percentage, na.rm = TRUE) * 1.15
      )
    ) +
    labs(
      x = NULL,
      y = "Cover when present (%)",
      title = "B"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        colour = "grey90",
        linewidth = 0.4
      ),
      axis.text.x = element_text(
        size = 10,
        colour = "grey30",
        lineheight = 0.9
      ),
      axis.text.y = element_text(
        size = 11,
        colour = "grey30"
      ),
      axis.title.y = element_text(
        size = 13
      ),
      legend.position = "none",
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0
      ),
      plot.margin = margin(5, 10, 5, 10)
    )
  
  plot_vegetation_cover <- plot_cover_A / plot_cover_B
  
  # ============================= #
  # 9. Ranges for data dictionary #
  # ============================= #
  
  variable_ranges <- list(
    height_range = clean_veg %>%
      summarise(
        min_height = min(
          height,
          na.rm = TRUE
        ),
        max_height = max(
          height,
          na.rm = TRUE
        )
      ),
    
    cover_percentage_range = clean_veg %>%
      summarise(
        min_cover_percentage = min(
          cover_percentage,
          na.rm = TRUE
        ),
        max_cover_percentage = max(
          cover_percentage,
          na.rm = TRUE
        )
      ),
    
    month_levels = sort(
      unique(
        clean_veg$month
      )
    ),
    
    year_levels = sort(
      unique(
        clean_veg$year
      )
    ),
    
    veg_taxon_levels = sort(
      unique(
        clean_veg$veg_taxon
      )
    )
  )
  
  return(
    list(
      cover_summary,
      height_summary,
      n_taxa,
      taxa_per_event_summary,
      most_common_taxa,
      top_common_taxa,
      mean_cover_taxa,
      top_cover_taxa,
      plot_vegetation_cover,
      variable_ranges
    )
  )
}

############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(clean_veg)
cover_summary <- resSum[[1]]
height_summary <- resSum[[2]]
n_taxa <- resSum[[3]]
taxa_per_event_summary <- resSum[[4]]
most_common_taxa <- resSum[[5]]
top_common_taxa <- resSum[[6]]
mean_cover_taxa <- resSum[[7]]
top_cover_taxa <- resSum[[8]]
plot_vegetation_cover <- resSum[[9]]
variable_ranges <- resSum[[10]]

#################
# INSPECT OUTPUTS (manually)
#################

# Mean bare soil, dead grass and vegetation cover
cover_summary

# Mean vegetation height
height_summary

# Number of identified taxa and unidentified categories
n_taxa

# Mean number of vegetation taxa per pitfall trap and sampling occasion
taxa_per_event_summary

# Most common identified taxa around pitfall traps
most_common_taxa
top_common_taxa

# Taxa with highest mean cover
mean_cover_taxa
top_cover_taxa

# Ranges and levels for the data dictionary
variable_ranges

############
# SAVE PLOTS
############

fig_path <- paste0(pathRepo, "figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

plot_vegetation_cover

ggsave(
  filename = "dataset_iv.png",
  plot = plot_vegetation_cover,
  width = 9,
  height = 7,
  dpi = 400
)
