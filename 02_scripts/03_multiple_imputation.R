# 03_multiple_imputation.R
# Purpose: Impute missing data for BRFSS SCD prevalence analysis

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

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

# Set RACE and SEXVAR as predictors but not to be imputed
predictor_matrix[, "RACE"] <- 1
predictor_matrix[, "SEXVAR"] <- 1

# Enable parallel processing
plan(multisession)

# Run multiple imputations (m = 5 for testing, increase to m = 20 for final)
imp <- mice(df_subset, m = 5, maxit = 5, seed = 500, method = methods, predictorMatrix = predictor_matrix)

# Extract completed datasets from the imputation object
imputed_data <- complete(imp, action = "all") 

# Add back in survey variables and create ACE summary variables
imputed_data <- lapply(imputed_data, function(df_imp) {
  df_full <- bind_cols(df %>% select(all_of(survey_vars)), df_imp)
  
  df_full <- df_full %>%
    mutate(across(starts_with("ACE"), ~ as.numeric(. == "Yes"))) %>%
    mutate(
      ACES_SUM = rowSums(select(., starts_with("ACE")), na.rm = TRUE),
      ACES4 = factor(if_else(ACES_SUM >= 4, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  return(df_full)
})

# Reorder columns so ACES_SUM and ACES4 come before the other ACE variables
imputed_data <- lapply(imputed_data, function(df_imp) {
  ace_vars <- grep("^ACE", names(df_imp), value = TRUE)           # all ACE variables
  ace_vars <- setdiff(ace_vars, c("ACES_SUM", "ACES4"))           # drop the two new ones from the list
  
  df_imp %>%
    select(ACES_SUM, ACES4, all_of(ace_vars), everything()) %>%
    labelled::set_variable_labels(
      ACES_SUM = "Total number of ACEs (0–13)",
      ACES4    = "Four or more ACEs (binary)"
    )
})

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save imputed datasets
saveRDS(imp, file.path(processed_data_dir, "03A_imputed_data.rds"))
saveRDS(imputed_data, file.path(processed_data_dir, "03B_completed_imputations.rds"))

# Print summary of imputations
print(imp)