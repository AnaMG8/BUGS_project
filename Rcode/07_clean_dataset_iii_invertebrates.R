###############################################################################
# Script name: 07_clean_dataset_iii_invertebrates.R
# Purpose: Validation and cleaning of Dataset iii: Invertebrate biomass for subterranean traps
# Author: Morales-González et al.
# Date: 29 May 2026
# Description:
#   This script processes the raw invertebrate biomass data recorded
#   in subterranean traps to obtain a clean dataset.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(janitor)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

# Define study period for subterranean trap sampling
startDate <- "2024-03-28"
endDate <- "2026-01-08"

############################
# LOAD NAMES OF RAW DATASETS
############################

# List all Excel files in the folder
nfiles <- list.files(
  path = paste0(pathRepo, "raw_datasets/sub_biomass/"),
  pattern = "\\.xlsx$|\\.xls$",
  full.names = FALSE
)

########################
# LOAD CREATED FUNCTIONS
########################

# Function to detect invalid subterranean trap IDs (function previously used for dataset i)
invalid_refs_TF <- function(biom_T){
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
  biom_T_checked <- biom_T %>%
    mutate(
      is_standard = str_detect(trap_id, pattern_standard),
      is_double   = str_detect(trap_id, pattern_double),
      is_three    = str_detect(trap_id, pattern_three),
      is_valid    = is_standard | is_double | is_three
    )
  
  # Extract invalid IDs into a separate dataframe for review
  invalid_refs_T <- biom_T_checked %>%
    filter(!is_valid) %>%
    # keep original columns plus the helper columns for debugging
    dplyr::select(trap_id, everything(), is_standard, is_double, is_three, is_valid)
  
  invalid_refs_T <-unique(invalid_refs_T$trap_id)
  
  return(invalid_refs_T)
}


# Main validation function
validationF <- function(nfiles){  # Apply validation steps to each biomass dataset
  
  biom <- NULL
  invalid_refs <- NULL
  outlier_abundances <- NULL
  outlier_weights <- NULL
  duplicates <- NULL
  
  for(i in 1:length(nfiles)){ # Apply validation steps to each biomass dataset
    
    file <- paste0(pathRepo, "raw_datasets/sub_biomass/", nfiles[i])
    biom_T <- read_excel(file, na = c("", "NA"))
    
    # =============================== #
    # 1. Standardize column names     #
    # =============================== #
    
    # convert column names to snake_case and lowercase
    biom_T <- biom_T %>% 
      janitor::clean_names()
    
    # column names should be timestamp, trap_id, weight and number
    colnames(biom_T)[1:3] = c("timestamp", "trap_id","weight")
    if(colnames(biom_T)[4] == "biomass"){colnames(biom_T)[4] <- "number"}
    
    if (ncol(biom_T) == 4) {
      
      biom_T$comments <- NA_character_
      
    } else if (ncol(biom_T) == 5) {
      
      colnames(biom_T)[5] <- "comments"
      biom_T$comments <- as.character(biom_T$comments)
      
    } else if (ncol(biom_T) > 5) {
      
      biom_T$comments <- apply(
        biom_T[, 5:ncol(biom_T), drop = FALSE],
        1,
        function(x) {
          x <- as.character(x)
          x <- trimws(x)
          x <- x[!is.na(x) & x != "" & x != "NA"]
          
          if (length(x) == 0) NA_character_ else paste(x, collapse = " / ")
        }
      )
      
      biom_T <- biom_T[, c("timestamp","trap_id","weight","number","comments")]
    }
    
    # =============================== #
    # 2. Format and validate types    #
    # =============================== #
    
    # convert timestamp to POSIXct
    if (is.character(biom_T$timestamp)) {
      
      if (grepl("\\.", biom_T$timestamp[1])) {
        biom_T$timestamp <- as.POSIXct(
          biom_T$timestamp,
          format = "%d.%m.%Y %H:%M",
          tz = "UTC"
        )
      } else {
        biom_T$timestamp <- as.POSIXct(
          biom_T$timestamp,
          format = "%d/%m/%y %H:%M",
          tz = "UTC"
        )
      }
      
    } else {
      
      biom_T$timestamp <- as.POSIXct(
        biom_T$timestamp,
        tz = "UTC"
      )
    }
    
    
    # convert weight and number to numeric, and add characters to comments
    biom_T <- biom_T %>%
      mutate(
        weight_old = as.character(weight),
        number_old = as.character(number),
        
        weight = suppressWarnings(as.numeric(weight)),
        number = suppressWarnings(as.numeric(number)),
        
        bad_values = paste(
          ifelse(is.na(weight) & !is.na(weight_old) & weight_old != "", weight_old, NA),
          ifelse(is.na(number) & !is.na(number_old) & number_old != "", number_old, NA)
        ),
        
        bad_values = gsub("NA", "", bad_values),
        bad_values = gsub("^ / | / $", "", bad_values),
        bad_values = gsub("  +", " ", bad_values),
        bad_values = trimws(bad_values),
        bad_values = ifelse(bad_values == "", NA, bad_values),
        
        comments = case_when(
          is.na(comments) ~ bad_values,
          is.na(bad_values) ~ comments,
          TRUE ~ paste(comments, bad_values, sep = " / ")
        )
      ) %>%
      select(-weight_old, -number_old, -bad_values)
    
    biom_T$comments <- as.character(biom_T$comments)
    
    
    # =============================== #
    # 3. Validate pitfall IDs         #
    # =============================== #
    
    # Ensure pitfall IDs are uppercase
    biom_T$trap_id <- toupper(biom_T$trap_id)
    
    # Ensure there are no empty spaces
    biom_T <- biom_T%>%
      mutate(
        trap_id = str_remove_all(trap_id, "\\s+")
      )
    
    # Detect potential invalid subterranean trap IDs (to inspect manually)
    invalid_refs_T <- invalid_refs_TF(biom_T)
    
    # ======================================= #
    # 4. Handle missing values and errors     #
    # ======================================= #
    
    # Remove rows where the date is outside the study period
    biom_T <- biom_T %>%
      filter(timestamp >= as.POSIXct(paste0(startDate)) &
               timestamp <= as.POSIXct(paste0(endDate, " 23:59:59")))
    
    # remove rows with invalid timestamp and NA weights
    biom_T <- biom_T %>%
      filter(!is.na(timestamp), !is.na(trap_id), !is.na(weight))
    
    # we do not remove rows with NA in number because this information 
    # may also be retrieved from the invertebrate dataset.
    
    # ==================== #
    # 5. Detect outliers   #
    # ==================== #
    
    # Outliers in invertebrate abundances (to inspect manually)
    outlier_abundances_T <- biom_T %>%
      filter(number < 0 | number > 40) %>%
      arrange(desc(number)) %>%
      mutate(file = i)
    
    # Outliers in invertebrate weights (to inspect manually)
    outlier_weights_T <- biom_T %>%
      filter(weight < 0 | weight > 0.2) %>%
      arrange(desc(weight)) %>%
      mutate(file = i)
    
    # =============================== #
    # 6. Detect duplicated records    #
    # =============================== #
    
    # Detect potential duplicates (to inspect manually)
    duplicates_T <- biom_T %>%
      group_by(across(everything())) %>%  
      filter(n() > 1) %>%             
      ungroup() 
    
    # ============================ #
    # 7. Join individual datasets  #
    # ============================ #
    
    if(nrow(biom_T) > 0){
      biom <- bind_rows(biom, biom_T)
      invalid_refs <- c(invalid_refs, invalid_refs_T)
      outlier_abundances <- bind_rows(outlier_abundances, outlier_abundances_T)
      outlier_weights <- bind_rows(outlier_weights, outlier_weights_T)
      duplicates <- bind_rows(duplicates, duplicates_T)
    }
  }
  
  return(list(biom, invalid_refs, outlier_abundances, outlier_weights, duplicates))
}

####################################
# RUN VALIDATION AND EXTRACT OUTPUTS
####################################
resVal <- validationF(nfiles)
biom_clean <- resVal[[1]]
invalid_refs <- resVal[[2]]
outlier_abundances <- resVal[[3]]
outlier_weights <- resVal[[4]]
duplicates <- resVal[[5]]
rm(resVal)

############################
# INSPECT OUTPUTS MANUALLY
############################

# OUTLIERS. ABUNDANCES
outlier_abundances # no typos

# OUTLIERS. WEIGHTS
outlier_weights # no typos

# SUBTERRANEAN TRAP IDs

invalid_refs # 7 typos found
biom_clean$trap_id[biom_clean$trap_id %in% "SJ15L03L01"] <- "SJ15L05L01"  # replace L03 by L05
biom_clean$trap_id[biom_clean$trap_id %in% "SJ15L03L02"] <- "SJ15L05L02"  # replace L03 by L05
biom_clean$trap_id[biom_clean$trap_id %in% "SJ15L03L03"] <- "SJ15L05L03"  # replace L03 by L05

biom_clean$trap_id[biom_clean$trap_id %in% "SH15L02L03"] <- "SH15L05L03"  # replace L02 by L05
biom_clean$trap_id[biom_clean$trap_id %in% "SH15L02L02"] <- "SH15L05L02"  # replace L02 by L05
biom_clean$trap_id[biom_clean$trap_id %in% "SH15L02L01"] <- "SH15L05L01"  # replace L02 by L05

# extract pitfalls checked before and after to identify typos manually
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "SJ13L01L03"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean <- biom_clean[!(biom_clean$trap_id %in% "SJ13L01L03"),] # remove wrong entry

invalid_refs <- invalid_refs_TF(biom_clean)
invalid_refs # no typos left

# DUPLICATES
duplicates # 1 duplicate found. We remove it.
biom_clean <- biom_clean %>% distinct()

######################
# EXPORT CLEAN DATASET
######################

write_xlsx(biom_clean, paste0(pathRepo, "clean_datasets/Dataset_iii_clean.xlsx"))

