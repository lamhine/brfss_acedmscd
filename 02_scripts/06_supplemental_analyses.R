# 06_supplemental_analyses.R
# Purpose: Supplemental (complete case) analysis 

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(survey)
library(gt)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD CLEANED DATASET AND SUBSET TO COMPLETE CASES
# ---------------------- #

# Load cleaned dataset
df <- readRDS(file.path(processed_data_dir, "02_cleaned_data.rds"))

# Subset to only complete cases
cc_df <- df %>% drop_na()

# Ensure correct coding of MEMLOSS
cc_df <- cc_df %>% mutate(MEMLOSS = as.numeric(MEMLOSS == "Yes"))

# Create survey design object for the complete case dataset
cc_dsn <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~weight_adjusted,
  data = cc_df
)

## SAMPLE SIZE CALCULATIONS ##
calculate_sample_sizes_unweighted <- function(design) {
  design$variables %>%
    group_by(RACE) %>%
    summarize(N = n(), .groups = "drop")
}

sample_sizes <- calculate_sample_sizes_unweighted(cc_dsn)
sample_sizes <- sample_sizes %>% mutate(RACE = as.character(RACE))
overall_N <- sum(sample_sizes$N, na.rm = TRUE)

## PREVALENCE CALCULATIONS ##
calculate_crude_prevalence_cc <- function(design) {
  by_race <- svyby(
    ~MEMLOSS,
    ~RACE,
    design = design,
    svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(weighted_prevalence = MEMLOSS, se = se)
  
  overall <- svymean(
    ~MEMLOSS,
    design,
    na.rm = TRUE
  )
  
  overall <- tibble(
    RACE = "Overall",
    weighted_prevalence = coef(overall)[1],
    se = SE(overall)[1]
  )
  
  bind_rows(by_race, overall) %>%
    mutate(type = "Crude")
}

calculate_adjusted_prevalence_cc <- function(design) {
  formula <- as.formula("MEMLOSS ~ RACE + AGEG5YR + SEXVAR")
  fit <- svyglm(formula, design = design, family = quasibinomial())
  
  # Extract factor levels
  race_levels <- levels(design$variables$RACE)
  age_ref <- levels(design$variables$AGEG5YR)[1]  # Reference category for age
  sex_ref <- levels(design$variables$SEXVAR)[1]  # Reference category for sex
  
  newdata <- expand_grid(
    RACE = factor(race_levels, levels = race_levels),
    AGEG5YR = factor(age_ref, levels = levels(design$variables$AGEG5YR)),
    SEXVAR = factor(sex_ref, levels = levels(design$variables$SEXVAR))
  )
  
  # Run predictions
  predicted_race <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)
  
  # Extract values
  if (is.list(predicted_race)) {
    weighted_prevalence <- as.numeric(predicted_race$fit)
    se <- as.numeric(predicted_race$se.fit)
  } else {
    weighted_prevalence <- as.numeric(predicted_race)
    se <- sqrt(attr(predicted_race, "var"))  # Extract standard error
  }
  
  # Compute a properly weighted overall adjusted prevalence
  race_sample_sizes <- design$variables %>%
    group_by(RACE) %>%
    summarize(N = sum(weights(design)), .groups = "drop")
  
  overall_weighted_prevalence <- sum(weighted_prevalence * race_sample_sizes$N) / sum(race_sample_sizes$N)
  
  # Compute standard error for overall
  overall_var <- sum((se^2) * (race_sample_sizes$N / sum(race_sample_sizes$N))^2)
  overall_se <- sqrt(overall_var)
  
  # Create final output
  bind_rows(
    tibble(
      RACE = race_levels,
      weighted_prevalence = weighted_prevalence,
      se = se
    ),
    tibble(
      RACE = "Overall",
      weighted_prevalence = overall_weighted_prevalence,
      se = overall_se
    )
  ) %>%
    mutate(type = "Adjusted")
}

## COMBINE RESULTS ##
combine_prevalence_cc <- function(design) {
  crude_results <- calculate_crude_prevalence_cc(design)
  adjusted_results <- calculate_adjusted_prevalence_cc(design)
  
  combined_crude <- crude_results %>%
    group_by(RACE, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  combined_adjusted <- adjusted_results %>%
    group_by(RACE, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  bind_rows(combined_crude, combined_adjusted)
}

final_combined_df_cc <- combine_prevalence_cc(cc_dsn) %>%
  mutate(RACE = as.character(RACE)) %>%
  left_join(sample_sizes, by = "RACE") %>%
  mutate(
    N = ifelse(RACE == "Overall", overall_N, N),
    RACE = ifelse(
      RACE == "Overall",
      paste0("Overall\n(N=", scales::comma(round(N, 0)), ")"),
      paste0(RACE, "\n(N=", scales::comma(round(N, 0)), ")")
    )
  )

# ---------------------- #
# FORMAT INTO A TABLE
# ---------------------- #

# First, separate the Crude and Adjusted rows into two data frames
crude_df <- final_combined_df_cc %>%
  filter(type == "Crude") %>%
  select(RACE, crude_prevalence = weighted_prevalence, lower_ci_crude = lower_ci, upper_ci_crude = upper_ci)

adjusted_df <- final_combined_df_cc %>%
  filter(type == "Adjusted") %>%
  select(RACE, adjusted_prevalence = weighted_prevalence, lower_ci_adjusted = lower_ci, upper_ci_adjusted = upper_ci)

# Join the two data frames on RACE
final_combined_df_cc_wide <- left_join(crude_df, adjusted_df, by = "RACE")

# Format the prevalence values into xx.x (xx.x, xx.x) format
final_combined_df_cc_wide <- final_combined_df_cc_wide %>%
  mutate(
    # Format crude prevalence and convert to percentage
    crude_prevalence_Crude = paste0(
      round(crude_prevalence * 100, 1), 
      " (", 
      round(lower_ci_crude * 100, 1), 
      ", ", 
      round(upper_ci_crude * 100, 1), 
      ")"),
    
    # Format adjusted prevalence and convert to percentage
    adjusted_prevalence_Adjusted = paste0(
      round(adjusted_prevalence * 100, 1), 
      " (", 
      round(lower_ci_adjusted * 100, 1), 
      ", ", 
      round(upper_ci_adjusted * 100, 1), 
      ")")
  ) %>%
  select(RACE, crude_prevalence_Crude, adjusted_prevalence_Adjusted)

# Create the gt table
final_table <- final_combined_df_cc_wide %>%
  gt() %>%
  cols_label(
    RACE = "Race/Ethnicity",
    crude_prevalence_Crude = "Crude Prevalence (95% CI)",
    adjusted_prevalence_Adjusted = "Adjusted Prevalence (95% CI)"
  )

# Display the table
print(final_table)

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save results
saveRDS(cc_survey_design, file.path(processed_data_dir, "06_survey_designs_complete_case.rds"))
saveRDS(final_combined_df, file.path(processed_data_dir, "06_summary_results_complete_case.rds"))
gtsave(final_table, file.path(results_dir, "06_appendix.docx"))

# Display results
print(final_combined_df)