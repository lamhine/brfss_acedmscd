# 01_load_data.R
# Purpose: Load BRFSS data from .XPT files using the RStudio Project setup

# ---------------------- #
# LOAD CONFIGURATION
# ---------------------- #

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# UNZIP FILES INTO NEW FOLDER IN RAW DATA DIRECTORY
# ---------------------- #

# List available .zip files in the raw data directory
zip_files <- list.files(raw_data_dir, pattern = "\\.zip$", ignore.case = TRUE, full.names = TRUE)

if (length(zip_files) == 0) {
  stop("ERROR: No .zip files found in ", raw_data_dir, ". Ensure BRFSS .zip files are placed in the correct directory.")
}

# Extract each .zip file into the "unzipped" directory inside raw_data_dir
for (zip_file in zip_files) {
  unzip(zip_file, exdir = unzipped_dir, overwrite = TRUE)
  message("Extracted: ", zip_file, " → ", unzipped_dir)
}

# Refresh the list of extracted .XPT files
extracted_files <- list.files(unzipped_dir, pattern = "\\.XPT$", ignore.case = TRUE, full.names = TRUE) %>% str_trim()

if (length(extracted_files) == 0) {
  stop("ERROR: No .XPT files were found after extracting. Ensure the .zip files contain valid .XPT datasets.")
}

message("All BRFSS .zip files successfully extracted to: ", unzipped_dir)

# ---------------------- #
# LOAD FILES INTO GLOBAL ENVIRONMENT AND PROCESS
# ---------------------- #

# Load the extracted .XPT files into a list of data frames
df_list <- setNames(lapply(extracted_files, read_xpt), basename(extracted_files))
message("Loaded ", length(df_list), " datasets.")

# Standardize dataset names (remove .XPT from df_list names)
names(df_list) <- str_remove(names(df_list), "\\.XPT$")

# Ensure aces_scd_mods.csv exists before attempting to read it
mods_path <- file.path("aces_scd_mods.csv")
if (!file.exists(mods_path)) {
  stop("ERROR: 'aces_scd_mods.csv' not found in 'data/' folder. Ensure it exists before running this script.")
}

# Load aces_scd_mods.csv for filtering
modules <- read_csv(mods_path)
head(modules)

# ---------------------- #
# IDENTIFY VALID STATE-YEAR-VERSION COMBINATIONS 
# ---------------------- #

# Get exact valid (dataset, fips) combinations
valid_dataset_fips <- modules %>%
  filter(module %in% c("SCD", "ACE")) %>%
  group_by(state, year) %>%
  filter(n_distinct(module) == 2) %>%
  distinct(dataset, fips)

# ---------------------- #
# FILTER AND COMBINE DATASETS INTO A SINGLE LIST
# ---------------------- #

# Filter each dataset to include only those fips
filtered_dfs <- list()

for (dataset_name in unique(valid_dataset_fips$dataset)) {
  if (dataset_name %in% names(df_list)) {
    valid_fips <- valid_dataset_fips %>%
      filter(dataset == dataset_name) %>%
      pull(fips)
    
    filtered_dfs[[dataset_name]] <- df_list[[dataset_name]] %>%
      filter(`_STATE` %in% valid_fips)
  }
}

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

saveRDS(filtered_dfs, file.path(processed_data_dir, "01_filtered_data.rds"))

