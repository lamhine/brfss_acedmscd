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

# Subset to remove people with type 1 diabetes


# Counterfactual simulation
set.seed(1234)

# Assume `analysis_df` has variables:
# ACEs: exposure (binary or categorical)
# diabetes: mediator (binary; diabetes only)
# scd: outcome (binary)
# age, sex, race: baseline confounders
# adult_covs: adult confounders for M→Y

# ---- Fit mediator model ----
m_model <- brm(
  DIAG ~ ACEs + age + sex + race,
  data = analysis_df,
  family = bernoulli(link = "logit"),
  cores = 4, chains = 4, iter = 2000
)

# ---- Fit outcome model ----
y_model <- brm(
  scd ~ ACEs + diabetes + age + sex + race + 
    depression + income + education + smoking + bmi + exercise,
  data = analysis_df,
  family = bernoulli(link = "logit"),
  cores = 4, chains = 4, iter = 2000
)

# ---- Define function for counterfactual simulation ----
simulate_counterfactuals <- function(data, ACE_val, M_val = NULL) {
  newdata <- data
  newdata$ACEs <- ACE_val
  
  if (!is.null(M_val)) {
    # Force mediator to fixed value
    newdata$diabetes <- M_val
  } else {
    # Predict mediator under new ACE level
    m_pred <- posterior_epred(m_model, newdata = newdata, draws = 1)
    # Draw from Bernoulli using predicted probabilities
    newdata$diabetes <- rbinom(n = nrow(newdata), size = 1, prob = m_pred)
  }
  
  # Predict outcome under ACE_val and simulated/fixed mediator
  y_pred <- posterior_epred(y_model, newdata = newdata, draws = 1)
  return(rowMeans(y_pred))
}

# ---- Step 4a: Simulate natural course ----
nat_A0 <- simulate_counterfactuals(analysis_df, ACE_val = 0)
nat_A1 <- simulate_counterfactuals(analysis_df, ACE_val = 1)

# ---- Step 4b: Simulate natural direct effect (fix M to counterfactual A=0) ----
nde <- simulate_counterfactuals(analysis_df, ACE_val = 1, 
                                M_val = simulate_counterfactuals(analysis_df, ACE_val = 0))

# ---- Step 4c: Summarize results ----
total_effect <- mean(nat_A1 - nat_A0)
direct_effect <- mean(nde - nat_A0)
indirect_effect <- total_effect - direct_effect

effects_df <- tibble(
  total_effect = total_effect,
  direct_effect = direct_effect,
  indirect_effect = indirect_effect
)

effects_df

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save results
saveRDS(survey_designs, file.path(processed_data_dir, "04A_survey_designs.rds"))
saveRDS(final_combined_df, file.path(processed_data_dir, "04B_summary_results.rds"))

# Display results
print(final_combined_df)