###############################################################################
# Script name: 11_sum_dataset_v_vertebrates.R
# Purpose: Summary metrics of Dataset v: Vertebrate species composition
# Author: Morales-González et al.
# Date: 11 July 2026
# Description:
#   This script calculates summary metrics provided in "Data Records"
#   for the clean Dataset v.
###############################################################################

###############
# LOAD PACKAGES
###############

library(tidyverse)
library(skimr)
library(plotly)
library(htmlwidgets)
library(dplyr)

##########################
# DEFINE WORKING DIRECTORY
##########################

pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################

clean_vert <- read_csv(
  paste0(pathRepo, "clean_datasets/Dataset_v_clean.csv.zip"),
  show_col_types = FALSE
)

skim(clean_vert)

############################
# RENAME SPECIES LABELS
############################

# Display all unique species and identification categories
# recorded in image_1, image_2, and image_3.

clean_vert %>%
  select(image_1, image_2, image_3) %>%
  unlist() %>%
  unique() %>%
  sort()

# Define the replacement labels for species and
# identification categories.

relabel <- c(
  "aardvark" = "Orycteropus afer",
  "aardwolf" = "Proteles cristatus",
  "African wildcat" = "Felis lybica",
  "bat" = "bat",
  "bat-eared fox" = "Otocyon megalotis",
  "bird" = "bird",
  "black-footed cat" = "Felis nigripes",
  "blesbok" = "Damaliscus pygargus phillipsi",
  "camel" = "camel",
  "cape fox" = "Vulpes chama",
  "Cape ground squirrel" = "Xerus inauris",
  "Cape porcupine" = "Hystrix africaeaustralis",
  "caracal" = "Caracal caracal",
  "cattle" = "cattle",
  "duiker" = "Sylvicapra grimmia",
  "eland" = "Tragelaphus oryx",
  "ground pangolin" = "Smutsia temminckii",
  "hare" = "Lepus spp.",
  "hartebeest" = "Alcelaphus buselaphus caama",
  "honey badger" = "Mellivora capensis",
  "horse" = "horse",
  "kudu" = "Tragelaphus strepsiceros",
  "lechwe" = "Kobus leche",
  "meerkat" = "Suricata suricatta",
  "none" = "none",
  "oryx" = "Oryx gazella",
  "reptile" = "reptile",
  "roan" = "Hippotragus equinus",
  "rodent" = "rodent",
  "sheep" = "sheep",
  "slender mongoose" = "Galerella sanguinea",
  "Small cat-like" = "small cat-like",
  "small-spotted genet" = "Genetta genetta",
  "springbok" = "Antidorcas marsupialis",
  "springhare" = "Pedetes capensis",
  "steenbok" = "Raphicerus campestris",
  "striped polecat" = "Ictonyx striatus",
  "unknown" = "unknown",
  "vehicle/human/livestock" = "vehicle/human/livestock",
  "warthog" = "Phacochoerus africanus",
  "wildebeest" = "Connochaetes taurinus taurinus",
  "yellow mongoose" = "Cynictis penicillata"
)

# Apply the replacement labels to all image columns.

clean_vert <- clean_vert %>%
  mutate(
    across(
      c(image_1, image_2, image_3),
      ~ recode(.x, !!!relabel)
    )
  )


########################
# FUNCTION TO CREATE PLOT
########################

plotF <- function(df, label_col = taxon){
  
  label_col <- enquo(label_col)
  
  df <- df %>%
    mutate(
      label = as.character(!!label_col),
      label = str_replace_all(label, "_", " "),
      label = str_to_sentence(label),
      label = str_replace(label, " ", "<br>")
    )
  
  plot_ly(
    data = df,
    r = ~n,
    theta = ~label,
    type = "barpolar",
    marker = list(
      color = ~n,
      colorscale = list(c(0, 1), c("#c6dbef", "#08306b"))
    )
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          range = c(0, max(df$n) * 1.1),
          gridcolor = "grey80",
          tickfont = list(size = 15, color = "grey20")
        ),
        angularaxis = list(
          direction = "clockwise",
          tickfont = list(size = 15)
        ),
        domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85))
      ),
      margin = list(l = 120, r = 120, t = 120, b = 120),
      showlegend = FALSE
    )
}

########################
# SUMMARY FUNCTION
########################

summaryF <- function(clean_vert){
  
  ############################
  # 1. Convert to long format
  ############################
  
  vert_long <- clean_vert %>%
    pivot_longer(
      cols = c(image_1, image_2, image_3),
      names_to = "image_label",
      values_to = "taxon"
    ) %>%
    filter(taxon != "none") %>%
    mutate(
      date = as.Date(timestamp)
    )
  
  ############################
  # 2. Define groups
  ############################
  
  game_species <- c(
    "Oryx gazella",
    "Connochaetes taurinus taurinus",
    "Tragelaphus oryx",
    "Alcelaphus buselaphus caama",
    "Tragelaphus strepsiceros",
    "Damaliscus pygargus phillipsi",
    "Antidorcas marsupialis",
    "Raphicerus campestris",
    "Sylvicapra grimmia",
    "Hippotragus equinus",
    "Kobus leche"
  )
  
  domestic_mammals <- c(
    "cattle",
    "sheep",
    "horse",
    "camel"
  )
  
  other_categories <- c(
    "rodent",
    "bat",
    "bird",
    "reptile",
    "unknown",
    "vehicle/human/livestock"
  )
  
  non_game_wild_mammals <- vert_long %>%
    distinct(taxon) %>%
    filter(
      !taxon %in% game_species,
      !taxon %in% domestic_mammals,
      !taxon %in% other_categories
    ) %>%
    pull(taxon)
  
  ############################
  # 3. Basic dataset summary
  ############################
  
  sampling_summary <- clean_vert %>%
    summarise(
      n_image_records = n(),
      n_sampling_stations = n_distinct(station_id),
      n_camera_traps = n_distinct(camera_id),
      first_date = min(as.Date(timestamp), na.rm = TRUE),
      last_date = max(as.Date(timestamp), na.rm = TRUE)
    )
  
  ############################
  # 4. Special categories
  ############################
  
  unknown_summary <- clean_vert %>%
    mutate(
      only_unknown =
        image_1 == "unknown" &
        image_2 == "none" &
        image_3 == "none"
    ) %>%
    summarise(
      n_only_unknown = sum(only_unknown),
      perc_only_unknown = 100 * mean(only_unknown)
    )
  
  vehicle_human_livestock_summary <- clean_vert %>%
    mutate(
      only_vehicle_human_livestock =
        image_1 == "vehicle/human/livestock" &
        image_2 == "none" &
        image_3 == "none"
    ) %>%
    summarise(
      n_only_vehicle_human_livestock = sum(only_vehicle_human_livestock),
      perc_only_vehicle_human_livestock =
        100 * mean(only_vehicle_human_livestock)
    )
  
  ############################
  # 5. Number of species/categories
  ############################
  
  n_species_categories <- tibble(
    n_game_species = length(game_species),
    n_non_game_wild_mammals = length(non_game_wild_mammals),
    n_domestic_mammal_categories = length(domestic_mammals)
  )
  
  ############################
  # 6. Species/categories per sampling station
  ############################
  
  species_categories_per_station <- vert_long %>%
    filter(!taxon %in% c("unknown", "vehicle/human/livestock")) %>%
    group_by(station_id) %>%
    summarise(
      n_species_categories = n_distinct(taxon),
      .groups = "drop"
    )
  
  species_categories_per_station_summary <- species_categories_per_station %>%
    summarise(
      mean_species_categories = mean(n_species_categories),
      perc_2.5 = quantile(n_species_categories, 0.025),
      perc_97.5 = quantile(n_species_categories, 0.975),
      n_sampling_stations = n()
    )
  
  ############################
  # 7. Summary by taxon
  ############################
  
  taxon_summary <- vert_long %>%
    group_by(taxon) %>%
    summarise(
      n_images = n(),
      perc_images = 100 * n() / nrow(clean_vert),
      n_sampling_stations = n_distinct(station_id),
      .groups = "drop"
    ) %>%
    mutate(
      group = case_when(
        taxon %in% game_species ~ "game",
        taxon %in% non_game_wild_mammals ~ "non_game",
        taxon %in% domestic_mammals ~ "domestic",
        taxon %in% c("rodent", "bat", "bird", "reptile") ~ "broad_category",
        TRUE ~ "excluded"
      )
    )
  
  ############################
  # 8. Frequency and occurrence summaries
  ############################
  
  game_frequency <- taxon_summary %>%
    filter(group == "game") %>%
    arrange(desc(n_images)) %>%
    transmute(taxon, n = n_images, perc_images)
  
  game_occurrence <- taxon_summary %>%
    filter(group == "game") %>%
    arrange(desc(n_sampling_stations), desc(n_images)) %>%
    transmute(taxon, n = n_sampling_stations)
  
  non_game_frequency <- taxon_summary %>%
    filter(group == "non_game") %>%
    arrange(desc(n_images)) %>%
    transmute(taxon, n = n_images, perc_images)
  
  non_game_occurrence <- taxon_summary %>%
    filter(group == "non_game") %>%
    arrange(desc(n_sampling_stations), desc(n_images)) %>%
    transmute(taxon, n = n_sampling_stations)
  
  domestic_frequency <- taxon_summary %>%
    filter(group == "domestic") %>%
    arrange(desc(n_images)) %>%
    transmute(taxon, n = n_images, perc_images)
  
  domestic_occurrence <- taxon_summary %>%
    filter(group == "domestic") %>%
    arrange(desc(n_sampling_stations), desc(n_images)) %>%
    transmute(taxon, n = n_sampling_stations)
  
  broad_category_summary <- taxon_summary %>%
    filter(group == "broad_category") %>%
    arrange(taxon)
  
  ############################
  # 9. Plots
  ############################
  
  plot_game_frequency <- game_frequency %>%
    slice_max(n, n = 20) %>%
    arrange(desc(n)) %>%
    plotF(taxon)
  
  plot_game_occurrence <- game_occurrence %>%
    slice_max(n, n = 20) %>%
    arrange(desc(n)) %>%
    plotF(taxon)
  
  plot_non_game_frequency <- non_game_frequency %>%
    slice_max(n, n = 20) %>%
    arrange(desc(n)) %>%
    plotF(taxon)
  
  plot_non_game_occurrence <- non_game_occurrence %>%
    slice_max(n, n = 20) %>%
    arrange(desc(n)) %>%
    plotF(taxon)
  
  return(list(
    sampling_summary,
    unknown_summary,
    vehicle_human_livestock_summary,
    n_species_categories,
    species_categories_per_station_summary,
    taxon_summary,
    game_frequency,
    game_occurrence,
    non_game_frequency,
    non_game_occurrence,
    domestic_frequency,
    domestic_occurrence,
    broad_category_summary,
    plot_game_frequency,
    plot_game_occurrence,
    plot_non_game_frequency,
    plot_non_game_occurrence
  ))
}

############################################
# RUN SUMMARY STATISTICS
############################################

resSum <- summaryF(clean_vert)

sampling_summary <- resSum[[1]]
unknown_summary <- resSum[[2]]
vehicle_human_livestock_summary <- resSum[[3]]
n_species_categories <- resSum[[4]]
species_categories_per_station_summary <- resSum[[5]]
taxon_summary <- resSum[[6]]

game_frequency <- resSum[[7]]
game_occurrence <- resSum[[8]]
non_game_frequency <- resSum[[9]]
non_game_occurrence <- resSum[[10]]
domestic_frequency <- resSum[[11]]
domestic_occurrence <- resSum[[12]]
broad_category_summary <- resSum[[13]]

plot_game_frequency <- resSum[[14]]
plot_game_occurrence <- resSum[[15]]
plot_non_game_frequency <- resSum[[16]]
plot_non_game_occurrence <- resSum[[17]]

#################
# INSPECT OUTPUTS
#################

sampling_summary
unknown_summary
vehicle_human_livestock_summary
n_species_categories
species_categories_per_station_summary
taxon_summary

game_frequency
game_occurrence

non_game_frequency
non_game_occurrence

domestic_frequency
domestic_occurrence

broad_category_summary

############
# SAVE PLOTS
############

fig_path <- paste0(pathRepo, "working_files/figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

plot_game_occurrence
plot_non_game_occurrence
plot_game_frequency
plot_non_game_frequency
# Export plots at 820 x 818 px for later assembly in a graphics editing environment.

