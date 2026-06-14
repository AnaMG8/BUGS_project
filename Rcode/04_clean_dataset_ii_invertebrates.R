###############################################################################
# Script name: 04_clean_dataset_ii_invertebrates.R
# Purpose: Validation and cleaning of Dataset ii: Invertebrate biomass for pitfall traps
# Author: Morales-González et al.
# Date: 27 May 2026
# Description:
#   This script processes the raw invertebrate biomass data recorded
#   at standard pitfall traps to obtain a clean dataset.
###############################################################################

###############
# LOAD PACKAGES
###############

library(dplyr)
library(stringr)
library(writexl)

##########################
# DEFINE WORKING DIRECTORY
##########################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/SoilDataPaper/"

# Define study period for pitfall trap sampling
startDate <- "2023-08-02"
endDate <- "2026-01-14"

############################
# LOAD NAMES OF RAW DATASETS
############################

# list all csv files in the folder
nfiles <- list.files(
  path = paste0(pathRepo, "raw_datasets/stand_biomass/")
)
nfiles <- nfiles[-which(nfiles=="invertebrates_biomass_26_11_24.csv")] # we remove sheet "26-11-2024" because it has missing data

########################
# LOAD CREATED FUNCTIONS
########################

# Function to detect invalid pitfall IDs (function previously used for dataset i)
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
validationF <- function(nfiles){
  
  biom = NULL
  invalid_refs = NULL
  outlier_abundances = NULL
  outlier_weights = NULL
  duplicates = NULL
  
  
  for(i in 1:length(nfiles)){ # Apply validation steps to each biomass dataset
    
    file <- paste0(pathRepo,"raw_datasets/stand_biomass/",nfiles[i])
    first_line <- readLines(file, n = 1)
    sep_used <- if(grepl(";", first_line)) ";" else ","
    biom_T <- read.csv(paste0(pathRepo,"raw_datasets/stand_biomass/",nfiles[i]),sep = sep_used,na.strings = c("", "NA"))
    
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
    if (grepl("\\.", biom_T$timestamp[1])) {
      biom_T$timestamp <- as.POSIXct(biom_T$timestamp, format = "%d.%m.%Y %H:%M", tz = "UTC")
    } else {
      biom_T$timestamp <- as.POSIXct(biom_T$timestamp, format = "%d/%m/%y %H:%M", tz = "UTC")
    }
    
    # convert weight and number to numeric, and add characters to comments
    biom_T <- biom_T %>%
      mutate(
        # keep original values before numeric conversion
        weight_old = as.character(weight),
        number_old = as.character(number),
        
        # convert columns to numeric
        weight = suppressWarnings(as.numeric(weight)),
        number = suppressWarnings(as.numeric(number)),
        
        # store non-numeric values
        bad_values = paste(
          ifelse(is.na(weight) & !is.na(weight_old) & weight_old != "", weight_old, NA),
          ifelse(is.na(number) & !is.na(number_old) & number_old != "", number_old, NA)
        ),
        
        # clean the stored text
        bad_values = gsub("NA", "", bad_values),
        bad_values = gsub("^ / | / $", "", bad_values),
        bad_values = gsub("  +", " ", bad_values),
        bad_values = trimws(bad_values),
        bad_values = ifelse(bad_values == "", NA, bad_values),
        
        # append to comments if needed
        comments = ifelse(
          is.na(comments),
          bad_values,
          ifelse(is.na(bad_values), comments, paste(comments, bad_values, sep = " / "))
        )
      ) %>%
      select(-weight_old, -number_old, -bad_values)
    
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
    
    # Detect potential invalid pitfall IDs (to inspect manually)
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
      filter(number < 0 | number > 200) %>%
      arrange(desc(number)) %>%
      mutate(file = i)
    
    # Outliers in invertebrate weights (to inspect manually)
    outlier_weights_T <- biom_T %>%
      filter(weight < 0 | weight > 10) %>%
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
    
    if(nrow(biom_T)>0){
      biom <- bind_rows(biom,biom_T)
      invalid_refs <- c(invalid_refs, invalid_refs_T)
      outlier_abundances <- rbind(outlier_abundances, outlier_abundances_T)
      outlier_weights <- rbind(outlier_weights, outlier_weights_T)
      duplicates <- rbind(duplicates, duplicates_T)
    }
  
  }
  
  return(list(biom,invalid_refs,outlier_abundances,outlier_weights,duplicates))
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

#################
# INSPECT OUTPUTS (manually)
#################

# OURLIERS. ABUNDANCES
outlier_abundances # 1 typo found
biom_clean[biom_clean$number<0 & !is.na(biom_clean$number),"number"] <- NA # replace negative numbers by NA

# OURLIERS. WEIGHTS
(outlier_weights <- outlier_weights %>% arrange(desc(weight))) # 11 typos found

biom_clean <- biom_clean[biom_clean$weight>=0,] # remove rows with negative weights

biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2025-09-16 11:37:00", tz = "UTC")&biom_clean$trap_id%in%"L17L06D"&biom_clean$weight%in%402&biom_clean$number%in%1] <- 0.0402 # Correct wrong entry
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2025-09-16 17:25:00", tz = "UTC")&biom_clean$trap_id%in%"J13L07"&biom_clean$weight%in%143&biom_clean$number%in%1] <- 0.0143 # Correct wrong entry
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2025-09-16 15:32:00", tz = "UTC")&biom_clean$trap_id%in%"L17L08D"&biom_clean$weight%in%94&biom_clean$number%in%1] <- 0.0094 # Correct wrong entry
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2025-11-11 11:08:00", tz = "UTC")&biom_clean$trap_id%in%"P15L04D"&biom_clean$weight%in%30.0391&biom_clean$number%in%14] <- 0.0391 # Correct wrong entry
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2025-11-12 07:54:00", tz = "UTC")&biom_clean$trap_id%in%"Q17L10D"&biom_clean$weight%in%47.5626&biom_clean$number%in%7] <- 37.5626 # Correct wrong entry

biom_clean <- biom_clean[!(biom_clean$timestamp%in%as.POSIXct("2024-01-17 17:43:00", tz = "UTC")&biom_clean$trap_id%in%"H15L01"&biom_clean$weight%in%23.9664&biom_clean$number%in%1),] # Remove sand snake record

# There are a few very large weights recorded, some include vertebrates, adjust:
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2023-12-29 10:41:00", tz = "UTC")&biom_clean$trap_id%in%"P15L08"] = biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2023-12-29 10:41:00", tz = "UTC")&biom_clean$trap_id%in%"P15L08"]-7
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2024-01-03 11:49:00", tz = "UTC")&biom_clean$trap_id%in%"L15L02"] = biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2024-01-03 11:49:00", tz = "UTC")&biom_clean$trap_id%in%"L15L02"]-5.9502
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2024-01-04 08:42:00", tz = "UTC")&biom_clean$trap_id%in%"L15L05"] = biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2024-01-04 08:42:00", tz = "UTC")&biom_clean$trap_id%in%"L15L05"]-9
biom_clean$weight[biom_clean$timestamp%in%as.POSIXct("2024-04-17 10:08:00", tz = "UTC")&biom_clean$trap_id%in%"J13L08"] = 6.4


# PITFALL IDs
invalid_refs # 7 typos found
biom_clean$trap_id[biom_clean$trap_id %in% "P15102D"] <- "P15L02D" # replace 1 by L
biom_clean$trap_id[biom_clean$trap_id %in% "H15107"] <- "H15L07" # replace 1 by L
invalid_refs <- invalid_refs_TF(biom_clean) # compare trap_id again towards expected patterns to check how many typos are left
invalid_refs # 5 typos left
# extract pitfalls checked before and after to identify typos manually
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "H18L01"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean$trap_id[biom_clean$trap_id %in% "H18L01"] <- "H15L01"  # replace H18 by H15
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "O15L06D"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean$trap_id[biom_clean$trap_id %in% "O15L06D"] <- "O18L06D"  # replace O15 by O18
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "O15L09"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean$trap_id[biom_clean$trap_id %in% "O15L09"] <- "O18L09"  # replace O15 by O18
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "O15L07"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean$trap_id[biom_clean$trap_id %in% "O15L07"] <- "O18L07"  # replace O15 by O18
biom_clean %>%
  arrange(timestamp) %>%
  mutate(row_id = row_number()) %>%
  filter(
    row_id %in% unique(unlist(lapply(
      which(trap_id == "P15L10A"),
      function(i) (i-5):(i+5)
    )))
  )
biom_clean$trap_id[biom_clean$trap_id %in% "P15L10A"] <- "P15L10D"  # replace A by D
invalid_refs <- invalid_refs_TF(biom_clean) # compare trap_id again towards expected patterns to check how many typos are left
invalid_refs # no typos left

# DUPLICATES
duplicates # 5 duplicates found. We remove them all.
biom_clean <- biom_clean %>% distinct()

######################
# EXPORT CLEAN DATASET
######################

write_xlsx(biom_clean, paste0(pathRepo, "clean_datasets/Dataset_ii_clean.xlsx"))

