# Standardize validated taxonomic dataset
standardizeF <- function(div_val){
  
  # =============================== #
  # 1. Standardize column names     #
  # =============================== #
  
  # Convert column names to snake_case and lowercase
  div_val <- div_val %>% 
    janitor::clean_names()
  
  # Replace "tribe" by "prey_item_tribe" to match the other taxonomic columns
  names(div_val)[names(div_val) == "tribe"] <- "prey_item_tribe"
  
  # =============================== #
  # 2. Format and validate types    #
  # =============================== #
  
  # Normalize inconsistent NAs in taxonomic columns.
  # This includes entries that are empty, unknown, or marked as "unidentified".
  # All such values are converted to real NA.
  div_val <- div_val %>%
    mutate(
      across(
        everything(), 
        ~ {
          x <- as.character(.)
          x[x %in% c("", "NA", "na", "NANA", "unknown", "Unknown",
                     "unidentified", "Unidentified", "UNIDENTIFIED")] <- NA
          x
        }
      )
    )
  
  
  # =============================== #
  # 3. Handle missing values        #
  # =============================== #
  
  # Remove rows missing critical info
  div_val <- div_val %>%
    tidyr::drop_na(prey_item_class, prey_item_order)
  
  # =============================== #
  # 4. Taxonomic validation         #
  # =============================== #
  
  # Standardize taxonomic names (species in lowercase; the first letter of higher-level taxa capitalized)
  div_val <- div_val %>%
    mutate(prey_item_species = tolower(prey_item_species))
  div_val <- div_val %>%
    mutate(across(all_of(c("prey_item_class","prey_item_order","prey_item_family",
                           "prey_item_subfamily","prey_item_tribe","prey_item_genera")), ~ sapply(., capitalizeF)))
  
  # Handle species labeled as sp.X (replace with NA)
  species_all <- unique(div_val$prey_item_species)
  sp_to_unid <- species_all[grepl("^sp\\.?\\s*\\d+$", species_all, ignore.case = TRUE)]
  div_val$prey_item_species[div_val$prey_item_species %in% sp_to_unid] <- NA
  rm(sp_to_unid,species_all)
  
  # Detect similar names within taxonomic levels (to inspect manually)
  similar_sp <- similar_namesF(div_val,"species")
  similar_gen <- similar_namesF(div_val,"genera")
  similar_tribe <- similar_namesF(div_val,"tribe")
  similar_subfam <- similar_namesF(div_val,"subfamily")
  similar_fam <- similar_namesF(div_val,"family")
  similar_order <- similar_namesF(div_val,"order")
  similar_class <- similar_namesF(div_val,"class")
  
  # =============================== #
  # 5. Remove duplicated records    #
  # =============================== #
  
  # Remove duplicates
  div_val <- div_val %>%
    distinct()
  
  return(list(div_val,similar_class,similar_order,similar_fam,similar_subfam,similar_tribe,similar_gen,similar_sp))
}

# Fill NAs with information from validated taxonomic dataset
fill_hierarchical_NAs <- function(target_df, reference_df, levels) {
  
  df_filled <- target_df
  na_count <- setNames(rep(0, length(levels)), levels)
  
  for (lvl_idx in seq_along(levels)) {
    lvl <- levels[lvl_idx]
    lower_levels <- levels[(lvl_idx+1):length(levels)]
    lower_levels <- lower_levels[!is.na(lower_levels)]
    if(length(lower_levels) == 0) next
    
    # Filas donde lvl es NA y hay algún nivel inferior identificado
    rows_to_fill <- is.na(df_filled[[lvl]]) & apply(df_filled[lower_levels], 1, function(x) any(!is.na(x)))
    
    if(any(rows_to_fill)) {
      df_subset <- df_filled[rows_to_fill, , drop = FALSE]
      
      fill_values <- sapply(seq_len(nrow(df_subset)), function(i) {
        key <- df_subset[i, lower_levels, drop = FALSE]
        ref_match <- reference_df
        for(col in lower_levels) {
          if(!is.na(key[[col]])) {
            ref_match <- ref_match[!is.na(ref_match[[col]]) & ref_match[[col]] == key[[col]], ]
          }
        }
        if(nrow(ref_match) >= 1) ref_match[[lvl]][1] else NA
      })
      
      na_count[lvl] <- sum(!is.na(fill_values))
      df_filled[which(rows_to_fill), lvl] <- fill_values
    }
  }
  
  return(list(df = df_filled, na_filled = na_count))
}


# VALIDATED TAXONOMIC DATASET -> DELETE

# Standardize
resStand <- standardizeF(div_val)
unique_taxonV <- resStand[[1]]
similar_classV <- resStand[[2]]
similar_orderV <- resStand[[3]]
similar_famV <- resStand[[4]]
similar_subfamV <- resStand[[5]]
similar_tribeV <- resStand[[6]]
similar_genV <- resStand[[7]]
similar_spV <- resStand[[8]]

# Similar taxonomic names
head(similar_classV) # no similar class names found
head(similar_orderV) # no similar order names found
head(similar_famV)
head(similar_subfamV)
head(similar_tribeV)
head(similar_genV)
head(similar_spV)

# DETELE
# Fill missing taxonomic information in div_clean using unique_taxonV
res_div <- fill_hierarchical_NAs(div_clean, unique_taxonV, colnames(unique_taxonV))
div_clean <- res_div[[1]]
res_div[[2]] # tells you how many NAs were filled
# DELETE


# UNIQUE TAXONOMIC COMBINATIONS
# Contrast unique taxonomic combinations in the cleaned dataset against a validated reference dataset to ensure full consistency

# unique combinations of all taxonomic levels in cleaned dataset
unique_taxon <- div_clean %>%
  distinct(prey_item_class, prey_item_order, prey_item_family, 
           prey_item_subfamily, prey_item_tribe, prey_item_genera, prey_item_species)



# DELETE CODE BELOW FOR THE FINAL CODE
# Fill unique_taxonV using div_clean
res_taxV <- fill_hierarchical_NAs(unique_taxonV, div_clean, levels)
unique_taxonV <- res_taxV[[1]]
res_taxV[[2]] # tells you how many NAs were filled
# DELETE CODE ABOVE FOR THE FINAL CODE


tax_cols <- c(
  "prey_item_class",
  "prey_item_order",
  "prey_item_family",
  "prey_item_subfamily",
  "prey_item_tribe",
  "prey_item_genera",
  "prey_item_species"
)

# para ignorar cf.
div_clean <- div_clean %>%
  mutate(across(where(is.character), ~ str_remove(., "^cf\\.\\s*")))
unique_taxonV2 <- unique_taxonV %>%
  mutate(across(where(is.character), ~ str_remove(., "^cf\\.\\s*")))

# saca las filas de mi dataset que no estan en validated.
no_en_unique_taxonV <- div_clean %>%
  select(all_of(tax_cols)) %>%    
  distinct() %>%      
  anti_join(
    unique_taxonV2 %>% select(all_of(tax_cols)) %>% distinct(),
    by = tax_cols
  )

write_xlsx(no_en_unique_taxonV, paste0(pathRepo,"Walt_checks/missingRows2.xlsx"))
write_xlsx(unique_taxonV, paste0(pathRepo,"Walt_checks/unique_taxonV.xlsx"))