# setup.R
# Purpose: Standardized project setup for all scripts

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration (defines `raw_data_dir`, `processed_data_dir`, `results_dir`)
source("config.R")

# Function to ensure directories exist, creating them if missing
dir_check_create <- function(dir_path, desc) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created missing ", desc, ": ", dir_path)
  }
}

# Ensure all required directories exist
dir_check_create(raw_data_dir, "raw data directory")
unzipped_dir <- file.path(raw_data_dir, "unzipped")  # Unzipped files inside raw data directory
dir_check_create(unzipped_dir, "unzipped data directory")
dir_check_create(processed_data_dir, "processed data directory")
dir_check_create(results_dir, "results directory")

# Message confirming setup
message("Project setup complete. 
  Raw Data: ", raw_data_dir, 
        " | Unzipped Data: ", unzipped_dir, 
        " | Processed Data: ", processed_data_dir, 
        " | Results: ", results_dir)