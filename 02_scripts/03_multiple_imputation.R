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

# Add back survey vars, create ACESUM / ACES4 without altering ACE factor types,
# then place them right before the other ACE* vars (but after survey vars)
imputed_data <- lapply(imputed_data, function(df_imp) {
  # Bind survey vars back on
  df_full <- bind_cols(df %>% select(all_of(survey_vars)), df_imp)
  
  # Identify ACE item variables (exclude the two summary vars if they exist)
  ace_vars <- grep("^ACE", names(df_full), value = TRUE)
  ace_vars <- setdiff(ace_vars, c("ACESUM", "ACES4"))
  
  # Create summaries WITHOUT recoding the ACE items globally
  df_full <- df_full %>%
    mutate(
      ACESUM = rowSums(across(all_of(ace_vars), ~ . == "Yes"), na.rm = TRUE),
      ACES4  = factor(if_else(ACESUM >= 4, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Reorder: survey vars → ACESUM/ACES4 → the other ACE* items → everything else
  other_vars <- setdiff(names(df_full), c(survey_vars, "ACESUM", "ACES4", ace_vars))
  
  df_full %>%
    select(all_of(survey_vars), ACESUM, ACES4, all_of(ace_vars), all_of(other_vars)) %>%
    labelled::set_variable_labels(
      ACESUM = "Total number of ACEs (0–13)",
      ACES4  = "Four or more ACEs (binary)"
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