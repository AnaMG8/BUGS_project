###############################################################################
# Script name: 12_sum_dataset_viii_macrohabitat.R
# Purpose: Summary metrics of Dataset viii: Macrohabitat polygons
# Author: Morales-González et al.
# Date: 11 September 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset viii.
###############################################################################

###############
# LOAD PACKAGES
###############

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(skimr)
library(ragg)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Set the path to the project directory
pathRepo <- "path/to/BUGS_project-main/"

#####################
# LOAD CLEAN DATASETS
#####################

# Dataset viii: Macrohabitat polygons
clean_macrohabitat <- st_read(
  paste0(pathRepo, "clean_datasets/Dataset_viii_clean.gpkg")
)

# Dataset ix: Pitfall trap locations
clean_pitfall_loc <- read_csv(
  paste0(pathRepo, "clean_datasets/Dataset_ix_clean.csv"),
  show_col_types = FALSE
)

# Dataset xi: Camera trap locations
clean_camera_loc <- read_csv(
  paste0(pathRepo, "clean_datasets/Dataset_xi_clean.csv"),
  show_col_types = FALSE
)

# Dataset xii: Land-use type polygons
clean_landuse <- st_read(
  paste0(pathRepo, "clean_datasets/Dataset_xii_clean.gpkg")
)

# Inspect clean dataset structure and summary statistics
skim(st_drop_geometry(clean_macrohabitat))

########################
# LOAD CREATED FUNCTIONS
########################

summaryF <- function(clean_macrohabitat, clean_landuse){
  
  # ============================= #
  # 1. Prepare spatial datasets   #
  # ============================= #
  
  # Clean geometries, remove Z/M dimensions, ensure valid polygons,
  # align both datasets to the same CRS, and retain only land-use
  # polygons belonging to the study area.
  
  macrohabitat <- clean_macrohabitat %>%
    st_zm(
      drop = TRUE,
      what = "ZM"
    ) %>%
    st_make_valid()
  
  landuse <- clean_landuse %>%
    st_transform(
      st_crs(macrohabitat)
    ) %>%
    st_zm(
      drop = TRUE,
      what = "ZM"
    ) %>%
    st_make_valid() %>%
    filter(
      !is.na(land_use)
    )
  
  
  # ==================================================== #
  # 2. Transform to projected CRS for area calculations #
  # ==================================================== #
  
  # UTM zone 34S (EPSG:32734) is appropriate for the study area.
  # Areas are calculated in square metres and converted to hectares.
  
  macrohabitat_utm <- macrohabitat %>%
    st_transform(
      32734
    )
  
  landuse_utm <- landuse %>%
    st_transform(
      32734
    )
  
  
  # ============================= #
  # 3. Total study area           #
  # ============================= #
  
  # Merge all land-use polygons and calculate the total study area in hectares
  
  study_area_union <- landuse_utm %>%
    summarise() %>%
    st_union()
  
  total_study_area <- tibble(
    area_ha = as.numeric(
      st_area(study_area_union)
    ) / 10000
  )
  
  
  # =================================== #
  # 4. Area of each land-use type       #
  # =================================== #
  
  land_use_area <- landuse_utm %>%
    group_by(
      land_use
    ) %>%
    summarise(
      .groups = "drop"
    ) %>%
    mutate(
      area_ha = as.numeric(
        st_area(geom)
      ) / 10000,
      percentage = 100 * area_ha / total_study_area$area_ha
    ) %>%
    st_drop_geometry() %>%
    arrange(
      desc(area_ha)
    )
  
  
  # ===================================================== #
  # 5. Crop macrohabitat polygons to the study area       #
  # ===================================================== #
  
  macrohabitat_study_area <- st_intersection(
    macrohabitat_utm,
    study_area_union
  )
  
  
  # ========================================= #
  # 6. Macrohabitat area across study area    #
  # ========================================= #
  
  macrohabitat_summary <- macrohabitat_study_area %>%
    group_by(
      macrohabitat
    ) %>%
    summarise(
      .groups = "drop"
    ) %>%
    mutate(
      area_ha = as.numeric(
        st_area(geom)
      ) / 10000,
      percentage = 100 * area_ha / total_study_area$area_ha
    ) %>%
    st_drop_geometry() %>%
    arrange(
      desc(area_ha)
    )
  
  
  # ================================================= #
  # 7. Macrohabitat area within each land-use type    #
  # ================================================= #
  
  # Merge polygons belonging to the same land-use type
  land_use_polygons <- landuse_utm %>%
    group_by(
      land_use
    ) %>%
    summarise(
      .groups = "drop"
    )
  
  # Intersect macrohabitat polygons with each land-use polygon
  macrohabitat_by_land_use <- st_intersection(
    macrohabitat_utm,
    land_use_polygons
  )
  
  # Calculate macrohabitat area within each land-use type
  macrohabitat_land_use_summary <- macrohabitat_by_land_use %>%
    group_by(
      land_use,
      macrohabitat
    ) %>%
    summarise(
      .groups = "drop"
    ) %>%
    mutate(
      area_ha = as.numeric(
        st_area(geom)
      ) / 10000
    ) %>%
    st_drop_geometry() %>%
    left_join(
      land_use_area %>%
        select(
          land_use,
          land_use_area_ha = area_ha
        ),
      by = "land_use"
    ) %>%
    mutate(
      percentage = 100 * area_ha / land_use_area_ha
    ) %>%
    arrange(
      land_use,
      desc(area_ha)
    )
  
  
  # ============================================ #
  # 8. Number of macrohabitat categories         #
  # ============================================ #
  
  n_macrohabitats <- macrohabitat %>%
    st_drop_geometry() %>%
    summarise(
      n_macrohabitats = n_distinct(
        macrohabitat
      )
    )
  
  
  # ============================================ #
  # 9. Macrohabitat levels                       #
  # ============================================ #
  
  macrohabitat_levels <- macrohabitat %>%
    st_drop_geometry() %>%
    distinct(
      macrohabitat
    ) %>%
    arrange(
      macrohabitat
    )
  
  
  return(
    list(
      total_study_area,
      land_use_area,
      macrohabitat_summary,
      macrohabitat_land_use_summary,
      n_macrohabitats,
      macrohabitat_levels
    )
  )
}


############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(clean_macrohabitat,clean_landuse)
total_study_area <- resSum[[1]]
land_use_area <- resSum[[2]]
macrohabitat_summary <- resSum[[3]]
macrohabitat_land_use_summary <- resSum[[4]]
n_macrohabitats <- resSum[[5]]
macrohabitat_levels <- resSum[[6]]


#################
# INSPECT OUTPUTS
#################

# Total study area (ha)
total_study_area

# Area and percentage of each land-use type
land_use_area

# Area and percentage of each macrohabitat across the entire study area
macrohabitat_summary

# Area and percentage of each macrohabitat within each land-use type
macrohabitat_land_use_summary

# Number of macrohabitat categories
n_macrohabitats

# Macrohabitat categories
macrohabitat_levels


# Rounded values

macrohabitat_summary %>%
  mutate(
    area_ha_round = round(area_ha),
    percentage_round = round(percentage)
  )

macrohabitat_land_use_summary %>%
  mutate(
    area_ha_round = round(area_ha),
    percentage_round = round(percentage)
  )

land_use_area %>%
  mutate(
    area_ha_round = round(area_ha),
    percentage_round = round(percentage)
  )


############################
# PREPARE DATA FOR FIGURE 9
############################

# Clean geometries
macrohabitat_plot <- clean_macrohabitat %>%
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) %>%
  st_make_valid()

landuse_plot <- clean_landuse %>%
  st_transform(
    st_crs(macrohabitat_plot)
  ) %>%
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) %>%
  st_make_valid()

# Keep only polygons with land-use information
landuse_plot <- landuse_plot %>%
  filter(
    !is.na(land_use)
  )

# Crop macrohabitat layer to study area
landuse_union <- landuse_plot %>%
  summarise() %>%
  st_union()

macrohabitat_plot_crop <- st_intersection(
  macrohabitat_plot,
  landuse_union
)


###############################
# PREPARE PITFALL TRAP LOCATIONS
###############################

pitfall_loc_sf <- st_as_sf(
  clean_pitfall_loc,
  coords = c(
    "longitude",
    "latitude"
  ),
  crs = 4326
) %>%
  st_transform(
    st_crs(macrohabitat_plot)
  )

# Retain one point per sampling transect for display
pitfall_loc_sf <- pitfall_loc_sf %>%
  filter(
    str_ends(
      trap_id,
      "L05"
    )
  ) %>%
  mutate(
    label = substr(
      trap_id,
      1,
      3
    )
  )


################################
# PREPARE CAMERA TRAP LOCATIONS
################################

camera_loc_sf <- st_as_sf(
  clean_camera_loc,
  coords = c(
    "longitude",
    "latitude"
  ),
  crs = 4326
) %>%
  st_transform(
    st_crs(macrohabitat_plot)
  )


##################
# LAND-USE LABELS
##################

land_use_labels_manual <- tibble::tribble(
  ~land_use, ~longitude, ~latitude,
  "natural",                 21.850, -26.940,
  "rotational\ngrazing",     21.835, -27.010,
  "mixed-species\ngrazing",  21.848, -27.045
) %>%
  st_as_sf(
    coords = c(
      "longitude",
      "latitude"
    ),
    crs = 4326
  ) %>%
  st_transform(
    st_crs(macrohabitat_plot)
  )

label_df <- cbind(
  st_coordinates(
    land_use_labels_manual
  ),
  st_drop_geometry(
    land_use_labels_manual
  )
) %>%
  mutate(
    hjust_label = if_else(
      land_use == "natural",
      0,
      1
    )
  )


###################
# CREATE FIGURE 9
###################

plot_macrohabitat <- ggplot() +
  
  geom_sf(
    data = macrohabitat_plot_crop,
    aes(
      fill = macrohabitat
    ),
    colour = NA
  ) +
  
  geom_sf(
    data = landuse_plot,
    fill = NA,
    colour = "grey25",
    linewidth = 0.5
  ) +
  
  geom_sf(
    data = pitfall_loc_sf,
    colour = "transparent",
    fill = "black",
    shape = 21,
    size = 2,
    stroke = 0
  ) +
  
  geom_sf(
    data = camera_loc_sf,
    colour = "grey20",
    fill = "red",
    shape = 21,
    size = 1.2,
    stroke = 0.25,
    alpha = 1
  ) +
  
  geom_text_repel(
    data = cbind(
      st_coordinates(
        pitfall_loc_sf
      ),
      st_drop_geometry(
        pitfall_loc_sf
      )
    ),
    aes(
      X,
      Y,
      label = label
    ),
    size = 3,
    segment.color = NA,
    seed = 1
  ) +
  
  geom_text(
    data = label_df,
    aes(
      X,
      Y,
      label = land_use,
      hjust = hjust_label
    ),
    size = 4,
    lineheight = 0.9,
    fontface = "plain",
    colour = "grey25"
  ) +
  
  scale_fill_manual(
    values = c(
      "Calcareous pan flats" = "#d46ac6",
      "Calcareous Rhigozum shrubland" = "#2ee61a",
      "High duneveld" = "#fff36a",
      "Low duneveld" = "#f7a52b",
      "River terrace" = "#4DA6FF"
    )
  ) +
  
  theme_bw() +
  
  labs(
    fill = "Macrohabitat",
    x = NULL,
    y = NULL
  ) +
  
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 10
    ),
    legend.key.size = unit(
      0.6,
      "cm"
    )
  ) +
  
  guides(
    fill = guide_legend(
      ncol = 1
    )
  ) +
  
  coord_sf(
    expand = TRUE
  )


#################
# INSPECT FIGURE
#################

plot_macrohabitat


############
# SAVE PLOT
############

fig_path <- paste0(pathRepo, "figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

ggsave(
  filename = paste0(
    fig_path,
    "/Figure_viii.png"
  ),
  plot = plot_macrohabitat,
  width = 180,
  height = 150,
  units = "mm",
  dpi = 600,
  device = ragg::agg_png
)
