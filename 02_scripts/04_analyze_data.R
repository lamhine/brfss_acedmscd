# 04_analyze_data.R
# Purpose: Analyze BRFSS data, estimate mediation models

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(survey)
library(mice)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND ANALYZE IMPUTED DATASET
# ---------------------- #

# Load imputed dataset
imp <- readRDS(file.path(processed_data_dir, "03A_imputed_data.rds"))
imputed_data <- readRDS(file.path(processed_data_dir, "03B_completed_imputations.rds"))

# Create survey design objects for each imputed dataset
survey_designs <- lapply(imputed_data, function(data) {
  svydesign(
    id = ~PSU,
    strata = ~STSTR,
    weights = ~LLCPWT,
    nest = TRUE,
    data = data
  )
})



# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save results
saveRDS(survey_designs, file.path(processed_data_dir, "04A_survey_designs.rds"))
saveRDS(final_combined_df, file.path(processed_data_dir, "04B_summary_results.rds"))

# Display results
print(final_combined_df)