###############################################################################
# Script name: 02_sum_dataset_i_invertebrates.R
# Purpose: Summary metrics of Dataset i: Invertebrate species composition
# Author: Morales-González et al.
# Date: 26 May 2026
# Description:
#   This script calculates the summary metrics provided in "Data Records"
#   for the clean Dataset i.
###############################################################################

###############
# LOAD PACKAGES
###############
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(skimr)
library(plotly)
library(tidyverse)
library(magick)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

####################
# LOAD CLEAN DATASET
####################
clean_div <- read_excel(paste0(pathRepo, "clean_datasets/Dataset_i_clean.xlsx"))

# Inspect clean dataset structure and summary statistics
skim(clean_div)

########################
# LOAD CREATED FUNCTIONS
########################

# Function to create polar bar plot
plotF <- function(df, label_col = genus_species){
  
  label_col <- enquo(label_col)
  
  df <- df %>%
    mutate(
      label = as.character(!!label_col),
      label = str_replace(
        label,
        "^([A-Za-z])[A-Za-z]*\\s+",
        "\\1. "
      ),
      label = case_when(
        str_detect(label, "\\([^)]*\\)\\s+[A-Za-z]+$") ~
          str_replace(label, "(\\))\\s+([A-Za-z]+)$", "\\1<br>\\2"),
        TRUE ~ label
      )
    )
  
  plot_ly(
    data = df,
    r = ~n,
    theta = ~label,
    type = "barpolar",
    marker = list(
      color = ~n,
      colorscale = list(c(0, 1), c("#a1d99b", "#005a32"))
    )
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          range = c(0, max(df$n) * 1.1),
          gridcolor = "grey80",
          tickfont = list(size = 15,color="grey20")
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

# Function to get summaries
summaryF <- function(clean_div){
  
  # =========================================================== #
  # 1. List of unique highest taxonomic resolution available    #
  # =========================================================== #
  
  # List with unique "lowest_taxon" available
  
  unique_div <- clean_div %>%
    mutate(
      lowest_taxon = case_when(
        !is.na(species) & !is.na(genera) ~ 
          paste(genera, species),        # use species if available
        !is.na(genera) ~ genera,         # otherwise use genus
        !is.na(tribe) ~ tribe,           # otherwise use tribe
        !is.na(subfamily) ~ subfamily,   # otherwise use subfamily
        !is.na(family) ~ family,         # otherwise use family
        !is.na(order) ~ order,           # otherwise use order
        !is.na(class) ~ class,           # otherwise use class
        TRUE ~ "Unidentified"                                # no taxonomic information available
      )
    )
  
  taxa_list <- unique_div %>%
    distinct(lowest_taxon, .keep_all = TRUE) %>%
    select(
      class,
      order,
      family,
      subfamily,
      tribe,
      genera,
      species,
      lowest_taxon
    ) %>%
    arrange(
      class,
      order,
      family,
      subfamily,
      tribe,
      genera,
      species,
      lowest_taxon
      
    )
  
  # ============================================= #
  # 2. Clean taxon names                          #
  # ============================================= #
  
  # Remove "cf. " at the beginning of any taxonomic field
  taxon_cols <- c("species", "genera", "tribe",
                  "subfamily", "family", "order",
                  "class")
  
  clean_div <- clean_div %>%
    mutate(across(all_of(taxon_cols),
                  ~ str_replace(.x, "^cf\\.\\s+", "")))   # remove "cf. " at start
  rm(taxon_cols)
  
  
  # ===================================== #
  # 3. Count the total number of taxa     #
  # ===================================== #
  
  # Create the "lowest_taxon" variable by selecting the most specific available taxonomic level
  clean_div <- clean_div %>%
    mutate(
      lowest_taxon = case_when(
        !is.na(species) & !is.na(genera) ~ 
          paste(genera, species),        # use species if available
        !is.na(genera) ~ genera,         # otherwise use genus
        !is.na(tribe) ~ tribe,           # otherwise use tribe
        !is.na(subfamily) ~ subfamily,   # otherwise use subfamily
        !is.na(family) ~ family,         # otherwise use family
        !is.na(order) ~ order,           # otherwise use order
        !is.na(class) ~ class,           # otherwise use class
        TRUE ~ "Unidentified"                                # no taxonomic information available
      )
    )
  
  # Calculate the total number of distinct taxa based on the "lowest_taxon" field
  n_taxa <- clean_div %>%
    summarise(n_taxa = n_distinct(lowest_taxon))
  
  # ========================================= #
  # 4. Calculate abundance by taxonomic level #
  # ========================================= #
  
  # Calculate total abundance and percentage of individuals identified at each taxonomic level
  total_abundance <- sum(clean_div$number_caught, na.rm = TRUE)  # total number of individuals
  
  abundance_by_level <- clean_div %>%
    mutate(
      tax_level = case_when(
        !is.na(species) ~ "Species",       # use species if available
        !is.na(genera) ~ "Genus",         # otherwise use genus
        !is.na(tribe) ~ "Tribe",          # otherwise tribe
        !is.na(subfamily) ~ "Subfamily",  # otherwise subfamily
        !is.na(family) ~ "Family",        # otherwise family
        !is.na(order) ~ "Order",          # otherwise order
        !is.na(class) ~ "Class",          # otherwise class
        TRUE ~ "Unidentified"                        # no taxonomic information available
      )
    ) %>%
    group_by(tax_level) %>%                        # group by taxonomic level
    summarise(
      abundance = sum(number_caught, na.rm = TRUE) # sum individuals per level
    ) %>%
    mutate(
      percentage = 100 * abundance / total_abundance # percentage of total abundance
    )
  
  
  # ============================================= #
  # 5. Count the total number of sp, genera, ...  #
  # ============================================= #
  
  # Calculate the total number of unique taxa at each taxonomic level
  unique_taxa_counts <- clean_div %>%
    summarise(
      n_species = n_distinct(paste(genera, species)
                             [!is.na(species) & !is.na(genera)]), # unique species
      n_genus   = n_distinct(genera[!is.na(genera)]),     # unique genera
      n_tribe   = n_distinct(tribe[!is.na(tribe)]),       # unique tribes
      n_subfamily = n_distinct(subfamily[!is.na(subfamily)]), # unique subfamilies
      n_family  = n_distinct(family[!is.na(family)]),      # unique families
      n_order   = n_distinct(order[!is.na(order)]),       # unique orders
      n_class   = n_distinct(class[!is.na(class)])        # unique classes
    )
  
  
  # ============================================= #
  # 6. Calculate species richness per pitfall     #
  # ============================================= #
  
  # Create a normalized pitfall ID
  clean_div <- clean_div %>%
    mutate(
      trap_id = case_when(
        # For methods other than "pitfall", remove the last 3 characters
        # (these represent strata within the same trap and should be ignored)
        method != "pitfall" ~ str_sub(trap_id, 1, -4),
        # For "pitfall" method, remove the 7th character if the string is long enough
        # (this character represents an internal subdivision that we want to ignore)
        method == "pitfall" & str_length(trap_id) == 7 ~ 
          str_sub(trap_id, 1, 6),
        # Otherwise, keep the original trap_id unchanged
        TRUE ~ trap_id                                 
      )
    )
  
  # Calculate the number of species per pitfall
  species_per_pitfall <- clean_div %>%
    filter(!is.na(species)) %>%            # remove records without species ID
    group_by(trap_id) %>%                         # group by normalized pitfall
    summarise(n_species = n_distinct(paste(genera, species))) # count unique species per pitfall
  
  # Add a trap_type column based on trap_id length
  species_per_pitfall <- species_per_pitfall %>%
    mutate(
      trap_type = if_else(str_length(trap_id) == 7, "Subterranean", "Standard")
    )
  
  # Calculate mean and 2.5th / 97.5th percentiles per trap type
  species_summary_by_type <- species_per_pitfall %>%
    group_by(trap_type) %>%
    summarise(
      mean_species  = mean(n_species),                                # mean number of species per trap
      perc_2.5      = quantile(n_species, 0.025),                    # 2.5th percentile
      perc_97.5     = quantile(n_species, 0.975),                    # 97.5th percentile
      n_traps       = n()                                             # number of traps per type
    )
  rm(species_per_pitfall)
  
  
  # ================================================ #
  # 7. Identify the most common and abundant species #
  #    with genus                                    #
  # ================================================ #
  
  # Most common species: species appearing in most pitfall traps
  most_common_species <- clean_div %>%
    filter(!is.na(genera) & !is.na(species)) %>%  # remove any rows with missing genus or species
    mutate(genus_species = paste(genera, species, sep = " "),
           trap_type = if_else(str_length(trap_id) == 7, "Subterranean", "Standard")) %>%
    group_by(trap_type, genus_species) %>%
    summarise(
      n = n_distinct(trap_id),
      .groups = "drop"
    ) %>%
    group_by(trap_type) %>%
    ungroup()
  
  # Most abundant: sum of number_caught
  most_abundant_species <- clean_div %>%
    filter(!is.na(genera) & !is.na(species)) %>%  # remove rows with missing genus or species
    mutate(genus_species = paste(genera, species, sep = " "),
           trap_type = if_else(str_length(trap_id) == 7, "Subterranean", "Standard")) %>%
    group_by(trap_type, genus_species) %>%
    summarise(
      n = sum(number_caught, na.rm = TRUE),           # sum individuals per genus+species
      .groups = "drop"
    ) %>%
    group_by(trap_type) %>%
    arrange(desc(n))  
  
  # ================================================ #
  # 8. Identify the most common and abundant genus   #
  # ================================================ #
  
  # Most common genus: genus appearing in most pitfall traps
  most_common_genus <- clean_div %>%
    filter(!is.na(genera)) %>%  # remove any rows with missing genus
    mutate(trap_type = if_else(str_length(trap_id) == 7, "Subterranean", "Standard")) %>%
    group_by(trap_type, genera) %>%
    summarise(
      n = n_distinct(trap_id),
      .groups = "drop"
    ) %>%
    group_by(trap_type)
  
  # Most abundant: sum of number_caught
  most_abundant_genus <- clean_div %>%
    filter(!is.na(genera)) %>%  # remove rows with missing genus
    mutate(trap_type = if_else(str_length(trap_id) == 7, "Subterranean", "Standard")) %>%
    group_by(trap_type, genera) %>%
    summarise(
      n = sum(number_caught, na.rm = TRUE),           # sum individuals per genus
      .groups = "drop"
    ) %>%
    group_by(trap_type)
  
  # =================== #
  # 9. Plots species    #                           
  # =================== #
  
  # Most common species in standard pitfalls
  most_common_species_sd <- most_common_species %>%
    filter(trap_type == "Standard") %>%
    slice_max(n, n = 18) %>%
    arrange(desc(n))
  plot_pres_sd_species <- plotF(most_common_species_sd, genus_species)
  
  # Most common species in subterranean pitfalls
  most_common_species_sub <- most_common_species %>%
    filter(trap_type == "Subterranean") %>%
    slice_max(n, n = 18) %>%
    arrange(desc(n))
  plot_pres_sub_species <- plotF(most_common_species_sub, genus_species)
  
  # Most abundant species in standard pitfalls
  most_abundant_species_sd <- most_abundant_species %>%
    filter(trap_type == "Standard") %>%
    slice_max(n, n = 18) %>%
    arrange(desc(n))
  plot_ab_sd_species <- plotF(most_abundant_species_sd, genus_species)
  
  # Most abundant species in subterranean pitfalls
  most_abundant_species_sub <- most_abundant_species %>%
    filter(trap_type == "Subterranean") %>%
    slice_max(n, n = 18) %>%
    arrange(desc(n))
  plot_ab_sub_species <- plotF(most_abundant_species_sub, genus_species)
  
  # =================== #
  # 10. Plots genus     #                           
  # =================== #
  
  # Most common genus in standard pitfalls
  most_common_genus_sd <- most_common_genus %>%
    filter(trap_type == "Standard") %>%
    slice_max(n, n = 15) %>%
    arrange(desc(n))
  plot_pres_sd_genus <- plotF(most_common_genus_sd, genera)
  
  # Most common genus in subterranean pitfalls
  most_common_genus_sub <- most_common_genus %>%
    filter(trap_type == "Subterranean") %>%
    slice_max(n, n = 15) %>%
    arrange(desc(n))
  plot_pres_sub_genus <- plotF(most_common_genus_sub, genera)
  
  # Most abundant genus in standard pitfalls
  most_abundant_genus_sd <- most_abundant_genus %>%
    filter(trap_type == "Standard") %>%
    slice_max(n, n = 15) %>%
    arrange(desc(n))
  plot_ab_sd_genus <- plotF(most_abundant_genus_sd, genera)
  
  # Most abundant genus in subterranean pitfalls
  most_abundant_genus_sub <- most_abundant_genus %>%
    filter(trap_type == "Subterranean") %>%
    slice_max(n, n = 15) %>%
    arrange(desc(n))
  plot_ab_sub_genus <- plotF(most_abundant_genus_sub, genera)
  
  return(list(taxa_list,n_taxa,total_abundance,abundance_by_level,unique_taxa_counts,species_summary_by_type,
              most_common_species,most_abundant_species,plot_pres_sd_genus,plot_pres_sub_genus,plot_ab_sd_genus,plot_ab_sub_genus))
}

############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(clean_div)
taxa_list <- resSum[[1]]
n_taxa <- resSum[[2]]
total_abundance <- resSum[[3]]
abundance_by_level <- resSum[[4]]
unique_taxa_counts <- resSum[[5]]
species_summary_by_type <- resSum[[6]]
most_common_species <- resSum[[7]]
most_abundant_species <- resSum[[8]]
plot_common_sd <- resSum[[9]] 
plot_common_sub <- resSum[[10]]
plot_ab_sd <- resSum[[11]]
plot_ab_sub <- resSum[[12]]

#################
# INSPECT OUTPUTS (manually)
#################

# List of taxa
taxa_list

# Number of potential taxa
n_taxa

# Total abundance
total_abundance

# Percentage of individuals identified to order,family,genera and species
(1-sum(clean_div[is.na(clean_div$order),"number_caught"])/total_abundance)*100
(1-sum(clean_div[is.na(clean_div$family),"number_caught"])/total_abundance)*100
(1-sum(clean_div[is.na(clean_div$genera),"number_caught"])/total_abundance)*100
(1-sum(clean_div[is.na(clean_div$species),"number_caught"])/total_abundance)*100

# Abundance per taxon
abundance_by_level

# Number of identified class, order, family, ...
unique_taxa_counts

# Mean number of species per pitfall, for standard and subterranean pitfalls
species_summary_by_type

# Most common species in pitfalls, for standard and subterranean pitfalls
most_common_species

# Most abundant species overall
most_abundant_species

############
# SAVE PLOTS
############

fig_path <- paste0(pathRepo, "figures")
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
setwd(fig_path)

plot_common_sd
plot_common_sub
plot_ab_sd
plot_ab_sub
# Export plots at 820 x 818 px for later assembly in a graphics editing environment.

