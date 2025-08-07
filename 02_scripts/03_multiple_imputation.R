# 03_multiple_imputation.R
# Purpose: Impute missing data for BRFSS SCD prevalence analysis

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(mice)
library(future)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND PROCESS CLEANED DATASET
# ---------------------- #

# Load cleaned dataset
df <- readRDS(file.path(processed_data_dir, "02_cleaned_data.rds"))

# Define variables to impute (exclude survey design variables)
survey_vars <- c("year", "dataset", "STATE", "STSTR", "LLCPWT", "PSU")
impute_vars <- setdiff(names(df), survey_vars)

# Subset data for imputation
df_subset <- df %>% select(all_of(impute_vars))

# Check missing data summary
missing_summary <- df_subset %>% summarise(across(everything(), ~ mean(is.na(.)) * 100))
print(missing_summary)

# Define imputation methods based on variable types
methods <- map_chr(df_subset, ~ case_when(
  is.numeric(.) && length(unique(.)) > 10 ~ "pmm",
  is.factor(.) && nlevels(.) == 2 ~ "logreg",
  is.factor(.) && nlevels(.) > 2 ~ "polyreg",
  TRUE ~ "pmm"
  )) %>%
  set_names(names(df_subset))

# Generate predictor matrix
predictor_matrix <- quickpred(df_subset)

# Enable parallel processing
plan(multisession)

# Run multiple imputations (m = 5 for testing, increase to m = 20 for final)
imp <- mice(df_subset, m = 20, maxit = 20, seed = 500, method = methods, predictorMatrix = predictor_matrix)

# Extract completed datasets from the imputation object
imputed_data <- complete(imp, action = "all") 

# Add back in survey variables
imputed_data <- lapply(imputed_data, function(df_imp) {
  bind_cols(df %>% select(all_of(survey_vars)), df_imp)
})

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save imputed datasets
saveRDS(imp, file.path(processed_data_dir, "03A_imputed_data.rds"))
saveRDS(imputed_data, file.path(processed_data_dir, "03B_completed_imputations.rds"))

# Print summary of imputations
print(imp)