# 04_analyze_data.R
# Purpose: Analyze BRFSS data for SCD prevalence, applying survey weights

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
  data <- data %>% mutate(MEMLOSS = as.numeric(MEMLOSS == "Yes"))  # Ensure correct coding
  svydesign(
    id = ~1,
    strata = ~STSTR,
    weights = ~weight_adjusted,
    data = data
  )
})

## SAMPLE SIZE CALCULATIONS ##
calculate_sample_sizes_unweighted <- function(design) {
  design$variables %>%
    group_by(RACE) %>%
    summarize(N = n(), .groups = "drop")
}

combine_sample_sizes <- function(survey_designs) {
  sample_sizes <- lapply(survey_designs, calculate_sample_sizes_unweighted)
  bind_rows(sample_sizes, .id = "imputation") %>%
    group_by(RACE) %>%
    summarize(N = mean(N, na.rm = TRUE), .groups = "drop")
}

sample_sizes <- combine_sample_sizes(survey_designs)
sample_sizes <- sample_sizes %>% mutate(RACE = as.character(RACE))
overall_N <- sum(sample_sizes$N, na.rm = TRUE)

## PREVALENCE CALCULATIONS ##
calculate_crude_prevalence <- function(design) {
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

calculate_adjusted_prevalence <- function(design) {
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

combine_prevalence <- function(survey_designs) {
  crude_results <- lapply(survey_designs, calculate_crude_prevalence)
  adjusted_results <- lapply(survey_designs, calculate_adjusted_prevalence)
  
  combined_crude <- bind_rows(crude_results, .id = "imputation") %>%
    group_by(RACE, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  combined_adjusted <- bind_rows(adjusted_results, .id = "imputation") %>%
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

final_combined_df <- combine_prevalence(survey_designs) %>%
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
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save results
saveRDS(survey_designs, file.path(processed_data_dir, "04A_survey_designs.rds"))
saveRDS(final_combined_df, file.path(processed_data_dir, "04B_summary_results.rds"))

# Display results
print(final_combined_df)