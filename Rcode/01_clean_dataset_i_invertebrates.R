###############################################################################
# Script name: 01_clean_dataset_i_invertebrates.R
# Purpose: Validation and cleaning of Dataset i: Invertebrate species composition
# Author: Morales-González et al.
# Date: 10 December 2025
# Description:
#   This script processes the raw invertebrate species composition dataset collected
#   from standard and subterranean pitfall traps to obtain a clean dataset.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(skimr)
library(readxl)
library(writexl)
library(stringdist)

###########################################
# DEFINE WORKING DIRECTORY AND STUDY PERIOD 
###########################################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/Scientific_Data_Inv/"

# Define study period (accounting for standard and subterranean sampling)
startDate <- "2023-08-02"
endDate <- "2025-09-30"

##################
# LOAD RAW DATASET
##################
div <- read_excel(paste0(pathRepo, "raw_datasets/invertebrate_diversity_AUG25.xlsx"))

# Inspect raw dataset structure and summary statistics
skim(div)

########################
# LOAD CREATED FUNCTIONS
########################

# Function to correctly capitalize taxonomic names
capitalizeF <- function(taxL) {
  # --------------------------------------------------------
  # Step 1: Handle missing values
  # If the input is NA, return a real NA
  # --------------------------------------------------------
  if (is.na(taxL)) {
    return(NA)  # Return actual NA
  }
  
  # --------------------------------------------------------
  # Step 2: Handle names starting with "cf."
  # "cf." means "confer" in taxonomy (uncertain identification)
  # The function normalizes the prefix to "cf. " (lowercase + period + space)
  # --------------------------------------------------------
  if (grepl("^(?i)cf\\.\\s*", taxL, perl = TRUE)) {
    # Normalize the cf. prefix, ignoring case
    taxL <- sub("^(?i)cf\\.\\s*", "cf. ", taxL, perl = TRUE)
    
    # Split the string into words
    parts <- strsplit(taxL, " ", fixed = TRUE)[[1]]
    
    if (length(parts) > 1) {
      # Capitalize the first letter of the second word (the taxon name)
      parts[2] <- paste0(
        toupper(substr(parts[2], 1, 1)),      # uppercase first letter
        substr(parts[2], 2, nchar(parts[2]))  # keep rest of word as-is
      )
      # Combine the words back into a single string
      taxL <- paste(parts, collapse = " ")
    }
    # Return the normalized name with cf. prefix
    return(taxL)
    
    # --------------------------------------------------------
    # Step 3: Handle normal names (without "cf.")
    # Capitalize the first letter of the string
    # --------------------------------------------------------
  } else {
    return(paste0(
      toupper(substr(taxL, 1, 1)),        # uppercase first letter
      substr(taxL, 2, nchar(taxL))        # append rest of the string as-is
    ))
  }
}

# Function to detect potential typographical errors within taxonomic levels using Levenshtein distance
similar_namesF <- function(div, targetT){
    if(targetT %in% "species"){
      taxon_all <- unique(div$prey_item_species) # get unique species
    }else if(targetT %in% "genera"){
      taxon_all <- unique(div$prey_item_genera) # get unique genera
    }else if(targetT %in% "tribe"){
      taxon_all <- unique(div$prey_item_tribe) # get unique tribe
    }else if(targetT %in% "subfamily"){
      taxon_all <- unique(div$prey_item_subfamily) # get unique subfamily
    }else if(targetT %in% "family"){
      taxon_all <- unique(div$prey_item_family) # get unique family
    }else if(targetT %in% "order"){
      taxon_all <- unique(div$prey_item_order) # get unique order
    }else if(targetT %in% "class"){
      taxon_all <- unique(div$prey_item_class) # get unique class
    }
    dist_matrix <- stringdistmatrix(taxon_all, taxon_all, method = "lv") # calculate pairwise Levenshtein distances
    threshold <- 2 # Establish a threshold for potential similar names
    similar_pairs <- which(dist_matrix <= threshold & dist_matrix > 0, arr.ind = TRUE) # Find pairs of names within threshold
    if(!nrow(similar_pairs)%in%0){
      similar_taxon_list <- list() # Build a list of similar names
      for(i in 1:nrow(similar_pairs)) {
        sp1 <- taxon_all[similar_pairs[i, 1]]
        sp2 <- taxon_all[similar_pairs[i, 2]]
        distance <- dist_matrix[similar_pairs[i, 1], similar_pairs[i, 2]]
        similar_taxon_list[[sp1]] <- c(similar_taxon_list[[sp1]],
                                       paste(sp2, "(Distance:", distance, ")"))
      }
      similar_taxon_df <- data.frame( # Convert to data.frame for easy inspection
        taxon = names(similar_taxon_list),
        Similar_taxon = sapply(similar_taxon_list, function(x) paste(x, collapse = ", "))
      )
      rownames(similar_taxon_df) <- 1:nrow(similar_taxon_df)
      
      # Get names that are similar
      taxon_list_1 <- similar_taxon_df$taxon
      taxon_list_2 <- similar_taxon_df$Similar_taxon %>%
        str_split(",") %>%         
        unlist() %>%               
        str_trim() %>%           
        str_remove("\\s*\\(Distance:.*\\)") 
      taxon_D <- unique(c(taxon_list_1, taxon_list_2))
      if(targetT %in% "species"){
        similar_taxon <- unique(div[div$prey_item_species %in% taxon_D, c("prey_item_genera", "prey_item_species")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_species, similar_taxon$prey_item_genera), ]
      }else if(targetT %in% "genera"){
        similar_taxon <- unique(div[div$prey_item_genera %in% taxon_D, c("prey_item_genera", "prey_item_species")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_genera, similar_taxon$prey_item_species), ]
      }else if(targetT %in% "tribe"){
        similar_taxon <- unique(div[div$prey_item_tribe %in% taxon_D, c("prey_item_tribe", "prey_item_genera")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_tribe), ]
      }else if(targetT %in% "subfamily"){
        similar_taxon <- unique(div[div$prey_item_subfamily %in% taxon_D, c("prey_item_subfamily","prey_item_tribe")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_subfamily), ]
      }else if(targetT %in% "family"){
        similar_taxon <- unique(div[div$prey_item_family %in% taxon_D, c("prey_item_family","prey_item_subfamily")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_family), ]
      }else if(targetT %in% "order"){
        similar_taxon <- unique(div[div$prey_item_order %in% taxon_D, c("prey_item_order","prey_item_family")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_order), ]
      }else if(targetT %in% "class"){
        similar_taxon <- unique(div[div$prey_item_class %in% taxon_D, c("prey_item_class","prey_item_order")])
        similar_taxon <- similar_taxon[order(similar_taxon$prey_item_class), ]
      }
    }else{
      similar_taxon <- NULL
    }
  return(similar_taxon)
}

# Function to detect invalid pitfall IDs
invalid_refsF <- function(div){
  # Valid transect IDs
  valid_sites <- c("O18","D15","H15","J13","J15","K18","L15","L17","P15","Q17")
  
  # Build patterns
  site_pattern <- paste0("(", paste(valid_sites, collapse = "|"), ")")
  # standard pitfalls: SITE L01..L10
  pattern_standard <- paste0("^", site_pattern, "L(0[1-9]|10)$")
  # double-stratified subterranean pitfalls: S SITE L01 L01|L02  (first L01 indicates double, second indicates the stratum)
  pattern_double <- paste0("^S", site_pattern, "L01L(0[1-2])$")
  # three-stratified subterranean pitfalls: S SITE L05 L01|L02|L03  (first L05 indicates three, second indicates the stratum)
  pattern_three  <- paste0("^S", site_pattern, "L05L(0[1-3])$")
  
  # Check each row against the patterns
  div_checked <- div %>%
    mutate(
      is_standard = str_detect(ref_2, pattern_standard),
      is_double   = str_detect(ref_2, pattern_double),
      is_three    = str_detect(ref_2, pattern_three),
      is_valid    = is_standard | is_double | is_three
    )
  
  # Extract invalid IDs into a separate dataframe for review
  invalid_refs <- div_checked %>%
    filter(!is_valid) %>%
    # keep original columns plus the helper columns for debugging
    dplyr::select(ref, ref_2, everything(), is_standard, is_double, is_three, is_valid)
  
  invalid_refs <-unique(invalid_refs$ref_2)
  
  return(invalid_refs)
}

# Function to detect inconsistencies between stated method and pitfall ID
invalid_methF <- function(div){
  # Valid transect IDs
  valid_sites <- c("O18","D15","H15","J13","J15","K18","L15","L17","P15","Q17")
  
  # Build patterns
  site_pattern <- paste0("(", paste(valid_sites, collapse = "|"), ")")
  # standard pitfalls: SITE L01..L10
  pattern_standard <- paste0("^", site_pattern, "L(0[1-9]|10)$")
  # double-stratified subterranean pitfalls: S SITE L01 L01|L02  (first L01 indicates double, second indicates the stratum)
  pattern_double <- paste0("^S", site_pattern, "L01L(0[1-2])$")
  # three-stratified subterranean pitfalls: S SITE L05 L01|L02|L03  (first L05 indicates three, second indicates the stratum)
  pattern_three  <- paste0("^S", site_pattern, "L05L(0[1-3])$")
  
  # Create a column inferring method from pitfall ID based on the patterns above
  div_checked <- div %>%
    mutate(
      trap_type_ref = case_when(
        str_detect(ref_2, pattern_standard) ~ "pitfall",
        str_detect(ref_2, pattern_double)   ~ "double-stratified subterranean",
        str_detect(ref_2, pattern_three)    ~ "three-stratified subterranean",
        TRUE ~ NA_character_
      ),
      # Flag records where the stated method does not match the inferred method
      method_mismatch = !is.na(trap_type_ref) & tolower(method) != trap_type_ref
    )
  
  # Keep only rows where a mismatch is detected
  invalid_method <- div_checked %>%
    filter(method_mismatch)
  
  # Return unique combinations of pitfall ID and method
  invalid_method <- unique(invalid_method[,c("ref_2","method"),])
  
  return(invalid_method)
}

# Main validation function
validationF <- function(div){
  
  # =============================== #
  # 1. Standardize column names     #
  # =============================== #
  
  # Convert column names to snake_case and lowercase
  div <- div %>% 
    janitor::clean_names()
  
  # Replace "tribe" by "prey_item_tribe" to match the other taxonomic columns
  names(div)[names(div) == "tribe"] <- "prey_item_tribe"
  
  # Replace "prey_item_group" by "prey_item_class" to match the other taxonomic columns
  names(div)[names(div) == "prey_item_group"] <- "prey_item_class"
  
  # =============================== #
  # 2. Format and validate types    #
  # =============================== #
  
  # Convert timestamp to POSIXct and convert number_caught to numeric
  div <- div %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      number_caught = as.numeric(number_caught)
    )
  
  # Keep only rows with complete timestamp format
  div <- div %>%
    filter(str_detect(as.character(timestamp), "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"))
  
  # Normalize inconsistent NAs in taxonomic columns.
  # This includes entries that are empty or marked as unknown, unidentified and NA.
  # All such values are converted to real NA.
  div <- div %>%
    mutate(
      across(
        -c(timestamp,number_caught,comments),
        ~ {
          x <- as.character(.)
          x[x %in% c("", "NA", "na", "unknown", "Unknown", "UNKNOWN",
                     "unidentified", "Unidentified", "UNIDENTIFIED")] <- NA
          x
        }
      )
    )
  
  # =============================== #
  # 3. Handle missing values        #
  # =============================== #
  
  # Remove rows missing critical info
  div <- div %>%
    tidyr::drop_na(prey_item_class, prey_item_order,timestamp, ref_2, number_caught)
  
  # Fill missing 'method' based on 'ref_2'
  div <- div %>%
    mutate(
      method = ifelse(
        is.na(method),
        case_when(
          nchar(ref_2) == 6 ~ "pitfall",
          substr(ref_2, 7, 7) == "1" ~ "double-stratified subterranean",
          substr(ref_2, 7, 7) == "5" ~ "three-stratified subterranean",
          TRUE ~ NA_character_
        ),
        method
      )
    )
  
  # =============================== #
  # 4. Taxonomic validation         #
  # =============================== #
  
  # Standardize taxonomic names (species in lowercase; the first letter of higher-level taxa capitalized)
  div <- div %>%
    mutate(prey_item_species = tolower(prey_item_species))
  div <- div %>%
    mutate(across(all_of(c("prey_item_class","prey_item_order","prey_item_family",
                           "prey_item_subfamily","prey_item_tribe","prey_item_genera")), ~ sapply(., capitalizeF)))
  
  # Handle species labeled as sp.X (replace with NA)
  species_all <- unique(div$prey_item_species)
  sp_to_unid <- species_all[grepl("^sp\\.?\\s*\\d+$", species_all, ignore.case = TRUE)]
  div$prey_item_species[div$prey_item_species %in% sp_to_unid] <- NA
  rm(sp_to_unid,species_all)
  
  # Detect similar names within taxonomic levels (to inspect manually)
  similar_sp <- similar_namesF(div,"species")
  similar_gen <- similar_namesF(div,"genera")
  similar_tribe <- similar_namesF(div,"tribe")
  similar_subfam <- similar_namesF(div,"subfamily")
  similar_fam <- similar_namesF(div,"family")
  similar_order <- similar_namesF(div,"order")
  similar_class <- similar_namesF(div,"class")
  
  # =============================== #
  # 5. Detect outliers and errors   #
  # =============================== #
  
  # Outliers in invertebrate abundances (to inspect manually)
  outlier_abundances <- div %>%
    filter(number_caught <= 0 | number_caught > 200) %>%
    arrange(desc(number_caught))
  
  # Remove rows where the date is outside the study period
  div <- div %>%
    filter(timestamp >= as.POSIXct(paste0(startDate)) &
             timestamp <= as.POSIXct(paste0(endDate)))
  
  # =============================== #
  # 6. Validate pitfall IDs         #
  # =============================== #
  
  # Ensure pitfall IDs are uppercase
  div$ref_2 <- toupper(div$ref_2)
  
  # Detect potential invalid pitfall IDs (to inspect manually)
  invalid_refs <- invalid_refsF(div)
  
  # =============================== #
  # 7. Validate stated methods      #
  # =============================== #
  
  # Ensure stated methods are lowercase
  div$method <- tolower(div$method)
  
  # Detect potential invalid stated methods (to inspect manually)
  invalid_method <- invalid_methF(div)
  
  # ===================================== #
  # 8. Standardize life-stage categories  #
  # ===================================== #
  
  # Convert all life-stage labels to lowercase to ensure consistent capitalization
  div$life_stage <- tolower(div$life_stage)
  
  # Remove punctuation (e.g., periods) that can cause issues
  div$life_stage <- gsub("\\.", "", div$life_stage)
  
  # Remove any leading or trailing whitespace from the labels
  div$life_stage <- trimws(div$life_stage)
  
  # =============================== #
  # 9. Detect duplicated records    #
  # =============================== #
  
  # Detect potential duplicates (to inspect manually)
  duplicates <- div %>%
    group_by(across(everything())) %>%  
    filter(n() > 1) %>%             
    ungroup()     
  
  # =============================== #
  # 10. Repeated refs               #
  # =============================== #
  
  # Detect 'ref' codes that appear more than once
  repeated_refs <- div %>%
    group_by(ref) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  
  return(list(div,similar_class,similar_order,similar_fam,similar_subfam,similar_tribe,similar_gen,similar_sp,
              outlier_abundances,invalid_refs,invalid_method,duplicates,repeated_refs))
}

####################################
# RUN VALIDATION AND EXTRACT OUTPUTS
####################################
resVal <- validationF(div)
div_clean <- resVal[[1]]
similar_class <- resVal[[2]]
similar_order <- resVal[[3]]
similar_fam <- resVal[[4]]
similar_subfam <- resVal[[5]]
similar_tribe <- resVal[[6]]
similar_gen <- resVal[[7]]
similar_sp <- resVal[[8]]
outlier_abundances <- resVal[[9]]
invalid_refs <- resVal[[10]]
invalid_method <- resVal[[11]]
duplicates <- resVal[[12]]
repeated_refs <- resVal[[13]]
rm(resVal)

#################
# INSPECT OUTPUTS (manually)
#################

# TAXONOMIC NAMES
similar_class # no similar class names found
similar_order # no similar order names found
similar_fam # 1 typo found
div_clean$prey_item_family[div_clean$prey_item_family %in% "Myrmeleonitdae"] <- "Myrmeleontidae" # replace Myrmeleonitdae by Myrmeleontidae
similar_subfam # all correct
similar_tribe # 1 typo found
div_clean$prey_item_tribe[div_clean$prey_item_tribe %in% "Anthini"] <- "Anthiini" # replace Anthini by Anthiini
similar_gen # 1 typo found
div_clean$prey_item_genera[div_clean$prey_item_genera %in% "Megaponea"] <- "Megaponera" # replace Megaponea by Megaponera
similar_sp # 2 typo found
div_clean$prey_item_species[div_clean$prey_item_species %in% "andersoni" & div_clean$prey_item_genera %in% "Metacatharsius"] <- "anderseni" # replace Metacatharsius andersoni by Metacatharsius anderseni
div_clean$prey_item_species[div_clean$prey_item_species %in% "coccineus" & div_clean$prey_item_genera %in% "Opistophthalmus"] <- "concinnus" # replace Opistophthalmus coccineus by Opistophthalmus concinnus

# OUTLIERS
outlier_abundances # realistic abundances because they are ants and termites

# PITFALL IDs
invalid_refs # 9 typos found
div_clean$ref_2[div_clean$ref_2 %in% "K18LO6"] <- "K18L06" # replace O by 0
div_clean$ref_2[div_clean$ref_2 %in% "SK15L01L01"] <- NA # unknown site, cannot fix zzz
div_clean$ref_2[div_clean$ref_2 %in% "SP15S05L03"] <- "SP15L05L03" # replace S by L zzz
div_clean$ref_2[div_clean$ref_2 %in% "SITE03"] <- NA # unknown site, cannot fix zzz
div_clean$ref_2[div_clean$ref_2 %in% "SITE05"] <- NA # unknown site, cannot fix zzz
div_clean$ref_2[div_clean$ref_2 %in% "S018L05L01"] <- "SO18L05L01" # replace O by 0 zzz
div_clean$ref_2[div_clean$ref_2 %in% "J51L10"] <- NA # unknown site, cannot fix
div_clean$ref_2[div_clean$ref_2 %in% "K18K10"] <- "K18L10" # replace K by L zzz
div_clean$ref_2[div_clean$ref_2 %in% "K18K08"] <- "K18L08" # replace K by L
div_clean <- div_clean[!is.na(div_clean$ref_2),] # remove rows where ref_2 is NA

# STATED METHODS
invalid_method # 2 typos found
div_clean$method[div_clean$ref_2 == c("SH15L05L01")] <- "three-stratified subterranean"  # replace double-stratified by three-stratified
div_clean$method[div_clean$ref_2 == c("SH15L01L02")] <- "double-stratified subterranean"  # replace three-stratified by double-stratified

# STATED LIFE STAGES
unique(div_clean$life_stage)
life_stage_list <- split(div_clean, div_clean$life_stage) # Create a list where each element corresponds to one life stage
write_xlsx(life_stage_list, paste0(pathRepo,"checks/div_clean_by_life_stage.xlsx")) # Export the dataset to an Excel file for detailed inspection
# Some typos found during inspection and need correction
# NOTE: to do

# DUPLICATES
duplicates # no duplicates found

# REPEATED REFs
repeated_refs # Several refs are repeated
repeated_list <- lapply(repeated_refs$ref, function(x) { # Create a list where each element corresponds to one repeated reference
  div_clean %>% filter(ref == x)
})
names(repeated_list) <- repeated_refs$ref # Assign the names of the repeated refs to the list elements
write_xlsx(repeated_list, paste0(pathRepo, "checks/repeated_list.xlsx")) # Export the dataset to an Excel file for detailed inspection
# Some typos were found during inspection and need correction
# NOTE: to do

######################
# EXPORT CLEAN DATASET
######################

write_xlsx(div_clean, paste0(pathRepo, "clean_datasets/Dataset_i_clean.xlsx"))



