###############################################################################
# Script name: 09_clean_dataset_iv_vegetation.R
# Purpose: Validation and cleaning of Dataset iv: Vegetation species composition
# Author: Morales-González et al.
# Date: 29 May 2026
# Description:
#   This script processes the raw vegetation species composition dataset collected
#   in 4 x 4 m quadrats centred on pitfall traps to obtain a clean dataset.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readxl)
library(skimr)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

##################
# LOAD RAW DATASET
##################

veg <- read_excel(paste0(pathRepo, "raw_datasets/vegetation_data.xlsx"))

# Inspect raw dataset structure and summary statistics
skim(veg)

########################
# LOAD CREATED FUNCTIONS
########################

# Function to detect invalid pitfall IDs
invalid_refsF <- function(div){
  # Valid transect IDs
  valid_sites <- c("O18","D15","H15","J13","J15","K18","L15","L17","P15","Q17")
  
  # Build patterns
  site_pattern <- paste0("(", paste(valid_sites, collapse = "|"), ")")
  # standard pitfalls: SITE L01..L10
  pattern_standard <- paste0("^", site_pattern, "L(0[1-9]|10)D?$")
  # double-stratified subterranean pitfalls: S SITE L01 L01|L02  (first L01 indicates double, second indicates the stratum)
  pattern_double <- paste0("^S", site_pattern, "L01L(0[1-2])$")
  # three-stratified subterranean pitfalls: S SITE L05 L01|L02|L03  (first L05 indicates three, second indicates the stratum)
  pattern_three  <- paste0("^S", site_pattern, "L05L(0[1-3])$")
  
  # Check each row against the patterns
  div_checked <- div %>%
    mutate(
      is_standard = str_detect(trap_id, pattern_standard),
      is_double   = str_detect(trap_id, pattern_double),
      is_three    = str_detect(trap_id, pattern_three),
      is_valid    = is_standard | is_double | is_three
    )
  
  # Extract invalid IDs into a separate dataframe for review
  invalid_refs <- div_checked %>%
    filter(!is_valid) %>%
    # keep original columns plus the helper columns for debugging
    dplyr::select(trap_id, everything(), is_standard, is_double, is_three, is_valid)
  
  invalid_refs <-unique(invalid_refs$trap_id)
  
  return(invalid_refs)
}

# Main validation function
validationF <- function(veg){
  
  # ====================================================== #
  # 1. Standardize column names                           #
  # ====================================================== #
  
  # Convert column names to snake_case and lowercase
  veg <- veg %>%
    janitor::clean_names()
  
  # ====================================================== #
  # 2. Format variables                                   #
  # ====================================================== #
  
  # Standardize variable formats
  veg <- veg %>%
    rename(
      trap_id = pitfall_id
    ) %>%
    mutate(
      trap_id = toupper(as.character(trap_id)),
      month = as.integer(month),
      year = as.integer(year),
      across(
        -c(trap_id, month, year),
        as.numeric
      )
    )
  
  # ====================================================== #
  # 3. Validate pitfall IDs                               #
  # ====================================================== #
  
  invalid_refs <- invalid_refsF(
    veg
  )
  
  # ====================================================== #
  # 4. Check columns containing missing values            #
  # ====================================================== #
  
  na_summary <- veg %>%
    summarise(
      across(
        everything(),
        ~ sum(is.na(.))
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "n_missing"
    ) %>%
    filter(
      n_missing > 0
    ) %>%
    arrange(
      desc(n_missing)
    )
  
  # ====================================================== #
  # 5. Replace missing values in cover variables           #
  # ====================================================== #
  
  # Define metadata variables
  metadata_cols <- c(
    "trap_id",
    "month",
    "year"
  )
  
  # Define non-cover variables (metadata and vegetation height)
  non_cover_cols <- c(
    metadata_cols,
    "height"
  )
  
  # Identify cover variables
  cover_cols <- setdiff(
    names(veg),
    non_cover_cols
  )
  
  # Replace missing values with 0 in cover variables, assuming
  # that missing records indicate absence
  veg <- veg %>%
    mutate(
      across(
        all_of(cover_cols),
        ~ tidyr::replace_na(
          .,
          0
        )
      )
    )
  
  # ====================================================== #
  # 6. Calculate total cover                   #
  # ====================================================== #
  
  # Calculate total cover as the sum of all cover values
  veg <- veg %>%
    mutate(
      total_cover = rowSums(
        select(
          .,
          all_of(cover_cols)
        ),
        na.rm = TRUE
      )
    )
  
  # ========================================== #
  # 7. Rescale cover to sum 100%               #
  # ========================================== #
  
  # In some sampling records, total cover may be below 100%, reflecting
  # minor field recording inconsistencies. Here, we proportionally rescale these records
  # so that total cover sum exactly 100%.
  
  # Store records with total cover below 100% for inspection and quality control
  cover_rescaled_records <- veg %>%
    filter(
      total_cover < 100
    )
  
  # Loop through sampling records
  for(i in 1:nrow(veg)){
    
    # Only modify records where total cover is below 100%
    if(
      veg$total_cover[i] < 100
    ){
      
      # Increase cover proportionally
      veg[i, cover_cols] <- 
        veg[i, cover_cols] *
        (
          100 /
            veg$total_cover[i]
        )
      
      # Recalculate total cover
      veg$total_cover[i] <- sum(
        unlist(
          veg[i, cover_cols]
        ),
        na.rm = TRUE
      )
      
      # Calculate remaining difference to reach exactly 100%
      diff <- 
        100 -
        veg$total_cover[i]
      
      # Add remaining difference to dominant category
      max_col <- cover_cols[
        which.max(
          unlist(
            veg[i, cover_cols]
          )
        )
      ]
      
      veg[i, max_col] <- 
        veg[i, max_col] +
        diff
      
      # Update total cover
      veg$total_cover[i] <- sum(
        unlist(
          veg[i, cover_cols]
        ),
        na.rm = TRUE
      )
    }
  }
  
  # ====================================================== #
  # 8. Detect outliers                                    #
  # ====================================================== #
  
  # Identify records with total cover exceeding 100%.
  # Values above 100% may arise from overlapping vegetation
  # layers recorded in the field (e.g. understory vegetation
  # beneath woody species)
  outlier_cover <- veg %>%
    filter(
      total_cover > 100
    )
  
  # Identify records with vegetation height < 0 or > 100
  outlier_height <- veg %>%
    filter(
      !is.na(height),
      height < 0 |
        height > 100
    )
  
  # ====================================================== #
  # 9. Detect duplicated records                          #
  # ====================================================== #
  
  duplicates <- veg %>%
    group_by(
      trap_id,
      month,
      year
    ) %>%
    filter(
      n() > 1
    ) %>%
    ungroup()
  
  # ====================================================== #
  # 10. Create long-format vegetation dataset             #
  # ====================================================== #
  
  # Reclassify unidentified or ambiguously identified
  # vegetation records (unknown species, unknown
  # herbaceous vegetation and descriptive field names) as
  # "unknown vegetation". The category "unidentifiable stubs"
  # is retained separately.
  
  veg <- veg %>%
    pivot_longer(
      cols = all_of(cover_cols),
      names_to = "veg_taxon",
      values_to = "cover_percentage"
    ) %>%
    filter(
      !is.na(cover_percentage),
      cover_percentage > 0
    ) %>%
    mutate(
      veg_taxon = str_replace_all(veg_taxon, "_", " "),
      veg_taxon = str_squish(veg_taxon),
      
      veg_taxon = case_when(
        veg_taxon %in% c(
          "unknown herbaceous",
          "unknown sp",
          "cucumber sp",
          "red bush",
          "thorny bush yellow flower sp"
        ) ~ "unknown vegetation",
        TRUE ~ veg_taxon
      )
    ) %>%
    group_by(
      trap_id,
      month,
      year,
      height,
      veg_taxon
    ) %>%
    summarise(
      cover_percentage = sum(cover_percentage, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(
      year,
      month,
      trap_id,
      height,
      veg_taxon,
      cover_percentage
    ) %>%
    arrange(
      year,
      month,
      trap_id,
      veg_taxon
    )
  
  
  return(list(veg,invalid_refs,cover_rescaled_records,outlier_cover,outlier_height,duplicates))
}

####################################
# RUN VALIDATION AND EXTRACT OUTPUTS
####################################
resVal <- validationF(veg)
veg_clean <- resVal[[1]]
invalid_refs <- resVal[[2]]
cover_rescaled_record <- resVal[[3]]
outlier_cover <- resVal[[4]]
outlier_height <- resVal[[5]]
duplicates <- resVal[[6]]

#################
# INSPECT OUTPUTS (manual quality control)
#################

# Invalid pitfall IDs
invalid_refs # No invalid pitfall identifiers detected.

# Records with total cover < 100%
cover_rescaled_record # 25 records had total cover below 100%. 
# In all cases, total cover exceeded 99%, suggesting minor
# discrepancies likely due to visual estimation or field recording.
# Proportional rescaling was therefore considered appropriate.

# Records with total vegetation cover > 100%
outlier_cover # 70 records exceeded 100% total cover (range: 100.1–181%).
# This likely reflects overlap among vegetation strata (e.g. herbaceous
# vegetation beneath shrubs or trees) rather than data entry errors.

# Records with vegetation height < 0 or > 100 cm
outlier_height # 8 records had vegetation height values between 104–170 cm. These values
# were considered biologically plausible and therefore retained.

# Duplicate records
duplicates # No duplicated sampling records detected.


######################
# EXPORT CLEAN DATASET
######################

write_xlsx(veg_clean,paste0(pathRepo, "clean_datasets/Dataset_iv_clean.xlsx"))
