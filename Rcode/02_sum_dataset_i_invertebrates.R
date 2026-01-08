###############################################################################
# Script name: 02_sum_dataset_i_invertebrates.R
# Purpose: Summary metrics of Dataset i: Invertebrate species composition
# Author: Morales-González et al.
# Date: 10 December 2025
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

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/Scientific_Data_Inv/"

####################
# LOAD CLEAN DATASET
####################
clean_div <- read_excel(paste0(pathRepo, "clean_datasets/Dataset_i_clean.xlsx"))

# Inspect clean dataset structure and summary statistics
skim(clean_div)

########################
# LOAD CREATED FUNCTIONS
########################

summaryF <- function(clean_div){
  # ============================================= #
  # 1. Clean taxon names                           #
  # ============================================= #
  
  # Remove "cf. " at the beginning of any taxonomic field
  taxon_cols <- c("prey_item_species", "prey_item_genera", "prey_item_tribe",
                  "prey_item_subfamily", "prey_item_family", "prey_item_order",
                  "prey_item_class")
  
  clean_div <- clean_div %>%
    mutate(across(all_of(taxon_cols),
                  ~ str_replace(.x, "^cf\\.\\s+", "")))   # remove "cf. " at start
  rm(taxon_cols)
  
  
  # ===================================== #
  # 2. Count the total number of taxa     #
  # ===================================== #
  
  # Create the "lowest_taxon" variable by selecting the most specific available taxonomic level
  clean_div <- clean_div %>%
    mutate(
      lowest_taxon = case_when(
        !is.na(prey_item_species) ~ prey_item_species,       # use species if available
        !is.na(prey_item_genera) ~ prey_item_genera,         # otherwise use genus
        !is.na(prey_item_tribe) ~ prey_item_tribe,           # otherwise use tribe
        !is.na(prey_item_subfamily) ~ prey_item_subfamily,   # otherwise use subfamily
        !is.na(prey_item_family) ~ prey_item_family,         # otherwise use family
        !is.na(prey_item_order) ~ prey_item_order,           # otherwise use order
        !is.na(prey_item_class) ~ prey_item_class,           # otherwise use class
        TRUE ~ "Unidentified"                                # no taxonomic information available
      )
    )
  
  # Calculate the total number of distinct taxa based on the "lowest_taxon" field
  n_taxa <- clean_div %>%
    summarise(n_taxa = n_distinct(lowest_taxon))
  
  # ========================================= #
  # 3. Calculate abundance by taxonomic level #
  # ========================================= #
  
  # Calculate total abundance and percentage of individuals identified at each taxonomic level
  total_abundance <- sum(clean_div$number_caught, na.rm = TRUE)  # total number of individuals
  
  abundance_by_level <- clean_div %>%
    mutate(
      tax_level = case_when(
        !is.na(prey_item_species) ~ "Species",       # use species if available
        !is.na(prey_item_genera) ~ "Genus",         # otherwise use genus
        !is.na(prey_item_tribe) ~ "Tribe",          # otherwise tribe
        !is.na(prey_item_subfamily) ~ "Subfamily",  # otherwise subfamily
        !is.na(prey_item_family) ~ "Family",        # otherwise family
        !is.na(prey_item_order) ~ "Order",          # otherwise order
        !is.na(prey_item_class) ~ "Class",          # otherwise class
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
  # 4. Count the total number of sp, genera, ...  #
  # ============================================= #
  
  # Calculate the total number of unique taxa at each taxonomic level
  unique_taxa_counts <- clean_div %>%
    summarise(
      n_species = n_distinct(prey_item_species[!is.na(prey_item_species)]),   # unique species
      n_genus   = n_distinct(prey_item_genera[!is.na(prey_item_genera)]),     # unique genera
      n_tribe   = n_distinct(prey_item_tribe[!is.na(prey_item_tribe)]),       # unique tribes
      n_subfamily = n_distinct(prey_item_subfamily[!is.na(prey_item_subfamily)]), # unique subfamilies
      n_family  = n_distinct(prey_item_family[!is.na(prey_item_family)]),      # unique families
      n_order   = n_distinct(prey_item_order[!is.na(prey_item_order)]),       # unique orders
      n_class   = n_distinct(prey_item_class[!is.na(prey_item_class)])        # unique classes
    )
  
  
  # ============================================= #
  # 5. Calculate species richness per pitfall     #
  # ============================================= #
  
  # Create a normalized pitfall ID
  # For methods other than "Pitfall", remove the last 3 characters of ref_2
  # because they indicate strata within the same trap, which we want to ignore
  clean_div <- clean_div %>%
    mutate(
      pitfall_id = case_when(
        method != "pitfall" ~ str_sub(ref_2, 1, -4),  # remove last 3 characters
        TRUE ~ ref_2                                   # keep original ref_2 for Pitfall
      )
    )
  
  # Calculate the number of species per pitfall
  species_per_pitfall <- clean_div %>%
    filter(!is.na(prey_item_species)) %>%            # remove records without species ID
    group_by(pitfall_id) %>%                         # group by normalized pitfall
    summarise(n_species = n_distinct(prey_item_species)) # count unique species per pitfall
  
  # Add a trap_type column based on pitfall_id length
  species_per_pitfall <- species_per_pitfall %>%
    mutate(
      trap_type = if_else(str_length(pitfall_id) == 7, "Subterranean", "Standard")
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
  # 6. Identify the most common and abundant species #
  #    with genus                                    #
  # ================================================ #
  
  # Create a combined genus + species column
  clean_div <- clean_div %>%
    mutate(genus_species = paste(prey_item_genera, prey_item_species, sep = " "))  # combine genus + species
  
  # Most common species: species appearing in the most pitfall traps
  most_common_species <- clean_div %>%
    filter(!is.na(prey_item_genera) & !is.na(prey_item_species)) %>%  # remove any rows with missing genus or species
    mutate(genus_species = paste(prey_item_genera, prey_item_species, sep = " "),
           trap_type = if_else(str_length(pitfall_id) == 7, "Subterranean", "Standard")) %>%
    group_by(trap_type, genus_species) %>%
    summarise(
      n_traps = n_distinct(pitfall_id),
      .groups = "drop"
    ) %>%
    group_by(trap_type) %>%
    slice_max(order_by = n_traps, n = 3) %>%  # top 3 per trap type
    ungroup()
  
  
  # Most abundant species overall: sum of number_caught
  most_abundant_species <- clean_div %>%
    filter(!is.na(prey_item_genera) & !is.na(prey_item_species)) %>%  # remove rows with missing genus or species
    mutate(genus_species = paste(prey_item_genera, prey_item_species, sep = " ")) %>%
    group_by(genus_species) %>%
    summarise(
      total_individuals = sum(number_caught, na.rm = TRUE),           # sum individuals per genus+species
      .groups = "drop"
    ) %>%
    arrange(desc(total_individuals)) %>%
    slice_head(n = 3)                                                  # top 3 most abundant
  
  
  return(list(n_taxa,total_abundance,abundance_by_level,unique_taxa_counts,species_summary_by_type,
              most_common_species,most_abundant_species))
}

############################################
# RUN SUMMARY STATISTICS AND EXTRACT OUTPUTS
############################################

resSum <- summaryF(clean_div)
n_taxa <- resSum[[1]]
total_abundance <- resSum[[2]]
abundance_by_level <- resSum[[3]]
unique_taxa_counts <- resSum[[4]]
species_summary_by_type <- resSum[[5]]
most_common_species <- resSum[[6]]
most_abundant_species <- resSum[[7]]

#################
# INSPECT OUTPUTS (manually)
#################

# Number of potential taxa
n_taxa

# Total abundance
total_abundance

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


