###############################################################################
# Script name: 03_clean_dataset_vi&vii_soil.R
# Purpose: Validation and cleaning of Dataset vi: Deployment date for loggers; 
# and Dataset vii: Soil temperature and moisture.
# Author: Morales-González et al.
# Date: 18 December 2025
# Description:
#   This script processes the raw soil temperature and moisture dataset collected
#   from TMS-4 and buriable TMS loggers to obtain a clean dataset.
#   We followed procedures by Man et al., 2023 (doi:10.1111/2041-210X.14192)
###############################################################################

###############
# LOAD PACKAGES
###############

library(myClim)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(mgcv)
library(furrr)
library(purrr)
library(hms)

################################################
# DEFINE WORKING DIRECTORY AND OUTLIERS SETTINGS
################################################

# Replace with your actual path
pathRepo <- "/Users/ana/Library/CloudStorage/OneDrive-UNIVERSIDADDESEVILLA/Documentos/Projects/SoilProject/Scientific_Data_Inv/"

# Choose here whether to remove outliers using myClim package or not
outliers_myClim <- TRUE

###################
# LOAD RAW DATASETS
###################

# Load soil data
soil <- mc_read_files(
  path = paste0(pathRepo,"raw_datasets/soil_data"),
  dataformat_name = "TOMST",              # TOMST logger format
  recursive = FALSE,                      # No subfolder search
  silent = TRUE                           # Suppress messages
)
mc_info_meta(soil)

# By default, the soil dataset includes the following variables:
# - locality_id: serial ID of the logger (duplicated by serial_number)
# - serial_number: serial ID of the logger
# - sensor_name: TMS_1, TMS_2, TMS_3 (temperature sensors) and TMS_moist (electrical conductivity sensor)
# - height: measurement depth/height as defined by the manufacturer (-6, +2, +15 cm)
# - datetime: date and time when the measurement was taken
# - time_to: date and time relative to 'datetime', indicating the sampling frequency
# - value: measured variable value

# Height values will be relabeled to match the actual sensor installation depths (see main text).


# Load logger deployment data from Excel, specifying column types
deployment <- read_excel(
  paste0(pathRepo, "raw_datasets/logger_deployment.xlsx"),
  col_types = c("text", "date", "text", "text", "text")
)


########################
# LOAD CREATED FUNCTION
########################

validationF <- function(soil,deployment){
  
  # ========================================== #
  # 1. Create timestamp in deployment dataset  #
  # ========================================== #
  
  deployment <- deployment %>%
    mutate(
      deployment_date = as.Date(deployment_date),
      hours   = floor(as.numeric(deployment_time) * 24),
      minutes = floor((as.numeric(deployment_time) * 24 - hours) * 60),
      seconds = round((((as.numeric(deployment_time) * 24 - hours) * 60) - minutes) * 60),
      timestamp = make_datetime(year = year(deployment_date),
                                month = month(deployment_date),
                                day = day(deployment_date),
                                hour = hours,
                                min = minutes,
                                sec = seconds,
                                tz = "UTC")
    ) %>%
    select(serial_number, timestamp, logger_type, pitfall_id)
  
  
  # ====================================== #
  # 2. Remove duplicates in soil dataset   #
  # ====================================== #
  
  # This is usually unnecessary because cleaning is done automatically when reading.
  soil <- suppressWarnings(mc_prep_clean(soil, silent = TRUE))

  
  # ========================================== #
  # 3. Keep only loggers of interest           #
  # ========================================== #
  
  # Remove those not listed in the deployment dataset
  soil <- mc_filter(soil,localities = deployment$serial_number)
  
  
  # ========================================== #
  # 4. Remove data before loggers deployment   #
  # ========================================== #
  
  # Create a crop table indicating, for each logger, 
  # the timestamp at which the device was deployed. 
  # These deployment times are later used to remove any records 
  # collected before the loggers were actually active.
  
  crop_table <- tibble(
    locality_id = as.character(deployment$serial_number),
    start = deployment$timestamp,
    end = as.POSIXct(NA)
  )
  
  soil <- mc_prep_crop(
    data       = soil,
    crop_table = crop_table
  )
  
  # ============================================================= #
  # 5. Remove outliers with mc_states_outlier function of myClim  #
  # ============================================================= # 
  
  if(outliers_myClim %in% TRUE){

  # Extract the default range table provided by myClim.
  # This table contains, for each sensor type, the expected minimum and maximum
  # permissible values. We need to fill thresholds for detecting sudden jumps
  # (i.e., abrupt changes between consecutive readings).
  range_table <- mc_info_range(soil)
  
  # For soil moisture sensors ("TMS_moist"), we adjust the jump thresholds.
  # These sensors show naturally high variability over short time periods,
  # so we allow larger positive and negative jumps (±500 units). 
  # This prevents physically plausible sensor signal fluctuations from being
  # incorrectly flagged as outliers.
  range_table$negative_jump[range_table$sensor_name == "TMS_moist"] <- 500
  range_table$positive_jump[range_table$sensor_name == "TMS_moist"] <- 500
  
  # For temperature sensors ("TMS_T1", "TMS_T2", "TMS_T3"),
  # we set much lower jump thresholds (±2 °C).
  # Temperature is expected to change gradually; therefore, abrupt changes 
  # exceeding 2 °C within a 15-minute interval likely indicate sensor artifacts
  # such as recording errors, resets, or physical disturbances.
  range_table$negative_jump[range_table$sensor_name %in% c("TMS_T1","TMS_T2","TMS_T3")] <- 2
  range_table$positive_jump[range_table$sensor_name %in% c("TMS_T1","TMS_T2","TMS_T3")] <- 2
  
  # Jump thresholds were set at ±500 units for raw soil moisture sensor signals
  # (TMS_moist; uncalibrated output, theoretical range 0–4000) and ±2 °C
  # for soil/air temperature sensors (theoretical range −40 to 60 °C), reflecting sensor-specific
  # physical limits and expected temporal variability.
  
  # Apply the outlier detection procedure.
  # mc_states_outlier compares each observation against the thresholds defined above.
  # Two types of anomalies are detected:
  #   - "range": values falling outside the expected physical range
  #   - "jump": abrupt changes between consecutive measurements
  # The resulting tags are stored inside the 'soil' myClim object.
  soil <- mc_states_outlier(
    data = soil,
    table = range_table,
    period = "15 minutes",    # recording frequency of the loggers
    range_tag = "range",      # tag assigned to range-based outliers
    jump_tag = "jump"         # tag assigned to jump-based outliers
  )
  
  # Summarize how many observations were flagged as each type of anomaly.
  # This helps quantify data quality and assess whether thresholds need adjustment.
  state_summary <- mc_info_states(soil) %>%
    count(tag)
  
  
  # Replace outliers with NA.
  # This step removes the flagged observations from further analyses
  # by substituting all "range" and "jump" outliers with missing values.
  # crop_margins_NA = FALSE ensures that only the tagged values are replaced
  # and that the time series itself is not trimmed at its start or end.
  soil <- mc_states_replace(
      data = soil,
      tags = c("range", "jump"),   # both types of detected outliers
      replace_value = NA,          # replace them with NA
      crop_margins_NA = FALSE      # do not crop margins
    )
  
  }else{
    state_summary <- NULL
 } 
  
  # ================================================================ #
  # 6. Convert raw moisture data to volumetric water content (VWC)   #
  # ================================================================ # 

  # Calculate volumetric water content (VWC) for cable loggers
  soil <- mc_calc_vwc(
    data = soil,
    moist_sensor = "TMS_moist",       # Raw soil moisture sensor
    temp_sensor  = "TMS_T1",          # Temperature sensor at the same depth as TMS_moist (in our case TMS_1 and TMS_T2)
    output_sensor = "VWC_moist",      # Name of the new calculated VWC sensor
    soiltype = "sand",                # Soil type used for calibration
    localities = deployment$serial_number[deployment$logger_type%in%"cable"],  # Apply to all localities in the object
    frozen2NA = TRUE                  # Set VWC to NA if soil is frozen
  )
  
  # Calculate volumetric water content (VWC) for standard loggers
  soil <- mc_calc_vwc(
    data = soil,
    moist_sensor = "TMS_moist",
    temp_sensor  = "TMS_T3",           # Temperature sensor at the same depth as TMS_moist (in our case TMS_T3)
    output_sensor = "VWC_moist",
    soiltype = "sand",
    localities = deployment$serial_number[deployment$logger_type%in%"standard"],
    frozen2NA = TRUE
  )
  
  
  # =========================== #
  # 7. Keep only relevant info  #
  # =========================== # 
  
  # Reshape dataset to long format
  soil <- mc_reshape_long(soil)
  
  soil <- soil %>%
    # Filter out rows corresponding to soil electrical conductivity data
    filter(sensor_name != "TMS_moist")%>%
    # Rename 'datetime' to 'timestamp' to match column names across datasets
    rename(timestamp = datetime)%>%
    # Select only the relevant columns for analysis
    select(serial_number, timestamp, sensor_name, height, value) %>%
    filter(complete.cases(.))
  
  
  # ====================================== #
  # 8. Relabel sensor names and heights    #
  # ====================================== #
  
  soil <- soil %>%
    # Temporarily add logger type to assign height and sensor name
    left_join(deployment %>% dplyr::select(serial_number, logger_type),by = "serial_number") %>%
    mutate(
      # Assign measurement height based on logger type and sensor name
      height = case_when(
        # --- Cable loggers ---
        # TMS_T1, TMS_T2, and VWC_moist sensors are at -50 cm depth
        logger_type %in% "cable" & sensor_name %in% c("TMS_T1", "TMS_T2", "VWC_moist") ~ "-50cm",
        # TMS_T3 sensor is at +15 cm depth
        logger_type %in% "cable" & sensor_name == "TMS_T3" ~ "+15cm",
        
        # --- Standard loggers ---
        # TMS_T1 sensor is at -6 cm
        logger_type %in% "standard" & sensor_name == "TMS_T1" ~ "-6cm",
        # TMS_T2 sensor is at +2 cm
        logger_type %in% "standard" & sensor_name == "TMS_T2" ~ "+2cm",
        # TMS_T3 sensor is at +15 cm
        logger_type %in% "standard" & sensor_name %in% c("TMS_T3") ~ "+15cm",
        # VWC_moist sensor is at 0 to -15 cm
        logger_type %in% "standard" & sensor_name %in% c("VWC_moist") ~ "0 to -15cm"
      ))%>%
    # Drop logger_type since it was only needed temporarily
    select(-logger_type)
  
  
  
  return(list(soil,deployment,state_summary))
  
}


####################################
# RUN VALIDATION AND EXTRACT OUTPUTS
####################################
resVal <- validationF(soil,deployment)
soil_clean <- resVal[[1]]
deployment_clean <- resVal[[2]]
outliers_myClim <- resVal[[3]]
rm(resVal)

#################
# INSPECT OUTPUTS (manually)
#################

outliers_myClim
# Summary of outliers detected and removed using the mc_states_outlier and the mc_states_replace
# functions of the package myClim.
# Detected outliers are grouped into:
#   - 'jump': sudden step changes between consecutive records
#   - 'range': measurements outside predefined valid ranges
#   - 'source': anomalies related to sensor source or metadata inconsistencies
# While effective at identifying anomalies, this method may not
# detect sustained shifts in sensor values, which are addressed using GAMs below.


# Temporarily add pitfall_id for steps below
soil_clean <- soil_clean %>%
  left_join(
    deployment %>% dplyr::select(serial_number, pitfall_id),
    by = "serial_number"
  )


# Identify consecutive days with missing data per sensor, height, and site (for reporting purposes only)
missing_ranges <- soil_clean %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(sensor_name, height, pitfall_id) %>%
  summarise(start_date = min(date), end_date = max(date), .groups = "drop") %>%
  tidyr::uncount(as.integer(end_date - start_date + 1)) %>%
  group_by(sensor_name, height, pitfall_id) %>%
  mutate(date = start_date + row_number() - 1) %>%
  ungroup() %>%
  anti_join(
    soil_clean %>% mutate(date = as.Date(timestamp)) %>% distinct(sensor_name, height, pitfall_id, date),
    by = c("sensor_name", "height", "pitfall_id", "date")
  ) %>%
  arrange(sensor_name, height, pitfall_id, date) %>%
  group_by(sensor_name, height, pitfall_id) %>%
  mutate(block = cumsum(date - lag(date, default = first(date)-1) > 1)) %>%
  group_by(sensor_name, height, pitfall_id, block) %>%
  summarise(
    start_missing = min(date),
    end_missing   = max(date),
    days_missing  = as.integer(end_missing - start_missing + 1),
    .groups = "drop"
  ) %>%
  select(-block) %>%
  arrange(start_missing, pitfall_id, sensor_name, height)


# The following block generates time series plots of soil sensor measurements for each logger,
# detecting prolonged anomalies using Generalized Additive Models (GAMs). 
# This provides a visual validation of the temporal patterns to 
# remove potential anomalies.

# Approach:
#   - Fit a GAM for each logger/sensor/height/
#   - Model smooth daily (hour-of-day) and seasonal (day-of-year) patterns
#   - Identify points with residuals exceeding 3 SDs as potential outliers
#   - This approach flags anomalies that may not be flagged by instantaneous 
#     outlier detection methods (e.g., mc_states_outlier). For instance, 
#     sustained shifts in sensor values caused by physical disturbance
#     of the logger (e.g. soil displacement due to animal digging).
#     Then you can decide whether flagged points should be removed from further analysis.

# For each logger:
#   - All sensors are plotted over time.
#   - Measurements are colored by time of day (day vs. night, 07:00–19:00 vs. night).
#   - Facets separate sensor types and measurement heights.
#   - Plots are saved automatically as PNG files in "checks/sensors_datasetvii" folder inside pathRepo.



# 1. Prepare dataset for GAM-based anomaly detection


# Add a column indicating whether each measurement was taken during day or night
# Day is defined as 07:00–19:00, night otherwise
soil_clean <- soil_clean %>%
  mutate(
    hour = hour(timestamp),
    day_night = ifelse(hour >= 7 & hour < 19, "day", "night")
  )

# Initialize outlier flag and extract day-of-year for smooth temporal modeling
soil_clean <- soil_clean %>% 
  mutate(
    outlier_gam = FALSE,
    day = yday(timestamp)  # day of year for cyclic smoother
  )

# Enable parallel processing
plan(multisession)


# 2. Detect prolonged anomalies using GAM


soil_clean <- soil_clean %>%
  group_by(pitfall_id, sensor_name, height) %>%
  group_modify(~{
    
    if (nrow(.x) < 50) return(.x)  # skip small groups
    
    # Fit GAM with cyclic smoothers for day-of-year and hour-of-day
    gam_mod <- gam(
      value ~ s(day, bs="cc", k=20) + s(hour, bs="cc", k=24),
      data = .x
    )
    
    resid_vals <- residuals(gam_mod)
    threshold <- 3 * sd(resid_vals, na.rm = TRUE)
    
    # Flag points exceeding threshold
    .x$outlier_gam <- abs(resid_vals) > threshold
    
    return(.x)
  }) %>%
  ungroup() 



# 3. Summarize outliers per logger, sensor and height


# Provides an overview of total outliers, number, and percentage
# Useful for identifying problematic sensors
outlier_summary <- soil_clean %>%
  group_by(pitfall_id, sensor_name, height) %>%
  summarise(
    total_points = n(),
    n_outliers_gam = sum(outlier_gam, na.rm = TRUE),
    percent_outliers_gam = 100 * n_outliers_gam / total_points,
    .groups = "drop"
  )

print(outlier_summary)


# 4. Create folder for time series plots


output_dir <- file.path(pathRepo, "checks/sensors_datasetvii")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# 5. Generate time series plots for each logger


# Plots show:
#   - Sensor measurements over time
#   - Color: day vs night
#   - Red points: GAM-flagged anomalies
#   - Facets: sensor type x height


# Get the list of unique loggers to generate plots for
pitfall_ids <- unique(soil_clean$pitfall_id)

# Loop over each logger and generate a plot of all sensors over time
walk(pitfall_ids, function(s) {
  
  # Filter the dataset for the current pitfall_id
  soil_site <- soil_clean %>%
    filter(pitfall_id == s)
  
  # Skip if there are no data for this pitfall_id
  if(nrow(soil_site) == 0) return()
  
  # Create the plot
  # - x-axis: timestamp
  # - y-axis: sensor value
  # - color: day vs. night
  # - facets: one row per sensor type, one column per height
  p <- ggplot(soil_site, aes(x = timestamp, y = value, color = day_night)) +
    geom_line() +
    geom_point(
      data = subset(soil_site, outlier_gam),
      aes(x = timestamp, y = value),
      color = "red",
      size = 1.5
    ) +
    facet_grid(sensor_name ~ height, scales = "free_y") +
    scale_color_manual(values = c("day" = "orange", "night" = "blue")) +
    labs(
      title = paste("Sensor measurements over time -", s),
      x = "timestamp",
      y = "Sensor value",
      color = "Time of day"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Save the plot as a PNG file in the designated folder
  file_name <- file.path(output_dir, paste0(gsub(" ", "_", s), ".png"))
  ggsave(file_name, plot = p, width = 10, height = 6)
  
  # Print a message indicating that the plot has been saved
  cat("Saved plot for pitfall_id:", s, "\n")
})


# After generating and inspecting the plots for all loggers, the GAM-flagged
# clear deviations from the expected temporal patterns.
# Visual inspection suggests that these anomalies likely correspond to
# erroneous measurements rather than natural variability, supporting their removal 
# for further analyses.


# 6. Remove GAM-flagged anomalies for downstream analysis


soil_clean <- soil_clean[soil_clean$outlier_gam %in% "FALSE",]
# Note: After removing anomalies, the visualization loop can be re-run
# to inspect the cleaned dataset and ensure temporal patterns are consistent.

# After outliers removal the sensor measurements appear consistent with expected 
# temporal patterns. No obvious artifacts were detected visually, 
# indicating that the data cleaning and steps produced a reliable dataset 
# suitable for further analysis.

colnames(soil_clean)
# 7. Keep only variables of interest
soil_clean <- soil_clean %>%
  select(serial_number,timestamp,sensor_name,height,value) 


######################
# EXPORT CLEAN DATASET
######################

# Export cleaned deployment dataset
write.csv(deployment_clean,paste0(pathRepo, "clean_datasets/Dataset_vi_clean.csv"),row.names = FALSE)

# Export cleaned soil sensor dataset (as compressed CSV; large file)
gz_con <- gzfile(paste0(pathRepo, "clean_datasets/Dataset_vii_clean.csv.gz"),"w")
write.csv(soil_clean,gz_con,row.names = FALSE)
close(gz_con)



