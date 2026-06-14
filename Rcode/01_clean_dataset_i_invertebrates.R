###############################################################################
# Script name: 01_clean_dataset_i_invertebrates.R
# Purpose: Validation and cleaning of Dataset i: Invertebrate species composition
# Author: Morales-González et al.
# Date: 26 May 2026
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
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

# Define study period (accounting for standard and subterranean sampling)
startDate <- "2023-08-02"
endDate <- "2026-01-22"

##################
# LOAD RAW DATASET
##################
div <- read_excel(paste0(pathRepo, "raw_datasets/invertebrate_diversity_FEB26.xlsx"))

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
      taxon_all <- unique(div$species) # get unique species
    }else if(targetT %in% "genera"){
      taxon_all <- unique(div$genera) # get unique genera
    }else if(targetT %in% "tribe"){
      taxon_all <- unique(div$tribe) # get unique tribe
    }else if(targetT %in% "subfamily"){
      taxon_all <- unique(div$subfamily) # get unique subfamily
    }else if(targetT %in% "family"){
      taxon_all <- unique(div$family) # get unique family
    }else if(targetT %in% "order"){
      taxon_all <- unique(div$order) # get unique order
    }else if(targetT %in% "class"){
      taxon_all <- unique(div$class) # get unique class
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
        similar_taxon <- unique(div[div$species %in% taxon_D, c("genera", "species")])
        similar_taxon <- similar_taxon[order(similar_taxon$species, similar_taxon$genera), ]
      }else if(targetT %in% "genera"){
        similar_taxon <- unique(div[div$genera %in% taxon_D, c("genera", "species")])
        similar_taxon <- similar_taxon[order(similar_taxon$genera, similar_taxon$species), ]
      }else if(targetT %in% "tribe"){
        similar_taxon <- unique(div[div$tribe %in% taxon_D, c("tribe", "genera")])
        similar_taxon <- similar_taxon[order(similar_taxon$tribe), ]
      }else if(targetT %in% "subfamily"){
        similar_taxon <- unique(div[div$subfamily %in% taxon_D, c("subfamily","tribe")])
        similar_taxon <- similar_taxon[order(similar_taxon$subfamily), ]
      }else if(targetT %in% "family"){
        similar_taxon <- unique(div[div$family %in% taxon_D, c("family","subfamily")])
        similar_taxon <- similar_taxon[order(similar_taxon$family), ]
      }else if(targetT %in% "order"){
        similar_taxon <- unique(div[div$order %in% taxon_D, c("order","family")])
        similar_taxon <- similar_taxon[order(similar_taxon$order), ]
      }else if(targetT %in% "class"){
        similar_taxon <- unique(div[div$class %in% taxon_D, c("class","order")])
        similar_taxon <- similar_taxon[order(similar_taxon$class), ]
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
        str_detect(trap_id, pattern_standard) ~ "pitfall",
        str_detect(trap_id, pattern_double)   ~ "double-stratified subterranean",
        str_detect(trap_id, pattern_three)    ~ "three-stratified subterranean",
        TRUE ~ NA_character_
      ),
      # Flag records where the stated method does not match the inferred method
      method_mismatch = !is.na(trap_type_ref) & tolower(method) != trap_type_ref
    )
  
  # Keep only rows where a mismatch is detected
  invalid_method <- div_checked %>%
    filter(method_mismatch)
  
  # Return unique combinations of pitfall ID and method
  invalid_method <- unique(invalid_method[,c("trap_id","method"),])
  
  return(invalid_method)
}

# Main validation function
validationF <- function(div){
  
  # ============================================================== #
  # 1. Standardize column names and remove unnecessary columns     #
  # ============================================================== #
  
  # Convert column names to snake_case and lowercase
  div <- div %>% 
    janitor::clean_names()
  
  # Replace "prey_item_group" by "class"
  names(div)[names(div) == "prey_item_group"] <- "class"
  
  # Replace "prey_item_order" by "order"
  names(div)[names(div) == "prey_item_order"] <- "order"
  
  # Replace "prey_item_family" by "family"
  names(div)[names(div) == "prey_item_family"] <- "family"
  
  # Replace "prey_item_subfamily" by "subfamily"
  names(div)[names(div) == "prey_item_subfamily"] <- "subfamily"
  
  # Replace "prey_item_genera" by "genera"
  names(div)[names(div) == "prey_item_genera"] <- "genera"
  
  # Replace "prey_item_species" by "species"
  names(div)[names(div) == "prey_item_species"] <- "species"
  
  # Replace "ref_2" by "trap_id"
  names(div)[names(div) == "ref_2"] <- "trap_id"
  
  # Remove ref. This ID indicates the collection reference of the specimen(s). 
  # If several specimens are clearly the same species and were collected at the same sampling occasion, 
  # pitfall, and stratum (if subterranean), they are recorded as a single entry and share the same ID. 
  # Otherwise, they receive a unique ID and entry. This ID allows experts to later assist with identifications.
  # The reference consists of NC (Northern Cape), VZR (Van Zylsrus), and a four-digit number.
  # We removed this variable as it is only important for the author´s internal libraries.
  div <- div %>% select(-ref)
  
  
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
    tidyr::drop_na(class, order,timestamp, trap_id, number_caught)
  
  # Fill missing 'method' based on 'trap_id'
  div <- div %>%
    mutate(
      method = ifelse(
        is.na(method),
        case_when(
          nchar(trap_id) == 6 ~ "pitfall",
          substr(trap_id, 7, 7) == "D" ~ "pitfall",
          substr(trap_id, 7, 7) == "1" ~ "double-stratified subterranean",
          substr(trap_id, 7, 7) == "5" ~ "three-stratified subterranean",
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
    mutate(species = tolower(species))
  div <- div %>%
    mutate(across(all_of(c("class","order","family",
                           "subfamily","tribe","genera")), ~ sapply(., capitalizeF)))
  
  # Handle species labeled as sp.X (replace with NA)
  species_all <- unique(div$species)
  sp_to_unid <- species_all[grepl("^sp\\.?\\s*\\d+$", species_all, ignore.case = TRUE)]
  div$species[div$species %in% sp_to_unid] <- NA
  rm(sp_to_unid,species_all)
  
  # Detect similar names within taxonomic levels (to inspect manually)
  similar_sp <- similar_namesF(div,"species")
  similar_gen <- similar_namesF(div,"genera")
  similar_tribe <- similar_namesF(div,"tribe")
  similar_subfam <- similar_namesF(div,"subfamily")
  similar_fam <- similar_namesF(div,"family")
  similar_order <- similar_namesF(div,"order")
  similar_class <- similar_namesF(div,"class")
  
  # Identify species epithets shared across multiple genera (ignoring "cf." prefixes),
  # and return a cleaned table with family, genus, species, and a flag indicating
  # uncertain species identification (cf.)
  duplicated_species_df <- div %>%
    filter(!is.na(species), !is.na(genera)) %>%
    mutate(
      cf_sp  = str_detect(species, "^cf\\.?\\s*"),
      species_clean = str_remove(species, "^cf\\.?\\s*")
    ) %>%
    group_by(species_clean) %>%
    filter(n_distinct(genera) > 1) %>%
    ungroup() %>%
    transmute(
      family,
      genera  = genera,
      species = species_clean,
      cf_sp
    ) %>%
    distinct() %>%
    arrange(species, genera)
  
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
             timestamp <= as.POSIXct(paste0(endDate, " 23:59:59")))
  
  # =============================== #
  # 6. Validate pitfall IDs         #
  # =============================== #
  
  # Ensure pitfall IDs are uppercase
  div$trap_id <- toupper(div$trap_id)
  
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
  
  
  return(list(div,similar_class,similar_order,similar_fam,similar_subfam,similar_tribe,similar_gen,similar_sp,
              duplicated_species_df,outlier_abundances,invalid_refs,invalid_method,duplicates))
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
duplicated_species_df <- resVal[[9]]
outlier_abundances <- resVal[[10]]
invalid_refs <- resVal[[11]]
invalid_method <- resVal[[12]]
duplicates <- resVal[[13]]
rm(resVal)

#################
# INSPECT OUTPUTS (manually)
#################

# TAXONOMIC NAMES
similar_class # no similar class names found
similar_order # no similar order names found
similar_fam # all correct
similar_subfam # all correct
similar_tribe # no similar tribe names found
similar_gen # all correct
similar_sp # 1 typo found
div_clean$species[div_clean$species %in% "andersoni" & div_clean$genera %in% "Metacatharsius"] <- "anderseni" # replace Metacatharsius andersoni by Metacatharsius anderseni
duplicated_species_df # 5 typos found
# Metacatharsius andersoni by Metacatharsius anderseni (done above)
div_clean$species[div_clean$species %in% "bimaculatus" & div_clean$genera %in% "Rhaphidosoma"] <- NA # Rhaphidosoma	bimaculatus by Rhaphidosoma	NA
div_clean$species[div_clean$species %in% "coccineus" & div_clean$genera %in% "Strangulotilla"] <- NA # Strangulotilla	coccineus by Strangulotilla	NA
div_clean$species[div_clean$species %in% "concinnus" & div_clean$genera %in% "Ammoxenus"] <- "coccineus" # Ammoxenus	concinnus by Ammoxenus coccineus
div_clean$species[div_clean$species %in% "morsitans" & div_clean$genera %in% "Zeria"] <- "monteiri" # Zeria	morsitans by Zeria	monteiri

# OUTLIERS
outlier_abundances # realistic abundances because they are ants and termites

# PITFALL IDs
invalid_refs # 4 typos found
div_clean$trap_id[div_clean$trap_id %in% "SITE03"] <- NA # unknown site, cannot fix
div_clean$trap_id[div_clean$trap_id %in% "SITE05"] <- NA # unknown site, cannot fix
div_clean$trap_id[div_clean$trap_id %in% "K18LO6"] <- "K18L06" # replace O by 0
div_clean$trap_id[div_clean$trap_id %in% "Q18L04"] <- "Q17L04" # replace 8 by 7
div_clean <- div_clean[!is.na(div_clean$trap_id),] # remove rows where trap_id is NA

# STATED METHODS
invalid_method # 3 typos found
div_clean$method[div_clean$trap_id == c("SH15L01L02")] <- "double-stratified subterranean"  # replace three-stratified by double-stratified
div_clean$method[div_clean$trap_id == c("SJ15L01L01")] <- "double-stratified subterranean"  # replace pitfall by double-stratified
div_clean$method[div_clean$trap_id == c("SH15L05L01")] <- "three-stratified subterranean"  # replace double-stratified by three-stratified

# STATED LIFE STAGES
unique(div_clean$life_stage)
div_clean$life_stage[div_clean$life_stage %in% c("juv")] <- "juvenile" # rename "juv" to "juvenile"

# DUPLICATES
duplicates # no duplicates found (although identical values, they correspond to different individuals)


######################
# EXPORT CLEAN DATASET
######################

write_xlsx(div_clean, paste0(pathRepo, "clean_datasets/Dataset_i_clean.xlsx"))



