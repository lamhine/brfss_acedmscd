# 05_visualize_data.R
# Purpose: Visualize survey-weighted SCD prevalence estimates and create Table 1

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(ggrepel)
library(survey)
library(gtsummary)
library(gt)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND ANALYZE DATA
# ---------------------- #

# Load cleaned dataset, imputed survey designs, and final prevalence table
df <- readRDS(file.path(processed_data_dir, "02_cleaned_data.rds"))
survey_designs <- readRDS(file.path(processed_data_dir, "04A_survey_designs.rds"))
final_combined_df <- readRDS(file.path(processed_data_dir, "04B_summary_results.rds"))

# ---------------------- #
# TABLE 1A: IMPUTED DATA
# ---------------------- #

# Extract first imputed dataset
imputed_design <- survey_designs[[1]]

# Convert MEMLOSS back to factor for proper display in Table 1B
imputed_design$variables <- imputed_design$variables %>%
  mutate(MEMLOSS = factor(MEMLOSS, levels = c(1, 0), labels = c("Yes", "No")))

# Generate table of imputed data
table1_imputed <- tbl_svysummary(
  imputed_design,
  by = "RACE",
  include = c("SEXVAR", "AGEG5YR", "MEMLOSS"),
  label = list(
    SEXVAR ~ "Sex",
    AGEG5YR ~ "Age Category",
    MEMLOSS ~ "Subjective Cognitive Decline"
  ),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  type = list(MEMLOSS ~ "categorical"),
  digits = all_categorical() ~ c(0, 1),
  missing = "no"
) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**", all_stat_cols() ~ "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(all_stat_cols() ~ "**Race/Ethnicity**") %>%
  modify_caption("Table 1B: Unweighted N and Weighted Percentages (Imputed Data)") %>%
  bold_labels()

# Print table
table1_imputed

# ---------------------- #
# TABLE 1B: COMPLETE CASE 
# ---------------------- #

df <- df %>%
  mutate(RACE = fct_na_value_to_level(RACE, level = "Missing"))  # Convert NA to "Missing"

# Create survey design without dropping missing data
complete_case_design <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~weight_adjusted,
  data = df
)

# Generate table of complete case data table
table1_complete <- tbl_svysummary(
  complete_case_design,
  by = "RACE",
  include = c("SEXVAR", "AGEG5YR", "MEMLOSS"),
  label = list(
    SEXVAR ~ "Sex",
    AGEG5YR ~ "Age Category",
    MEMLOSS ~ "Subjective Cognitive Decline"
  ),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  type = list(MEMLOSS ~ "categorical"),
  digits = all_categorical() ~ c(0, 1),
  missing = "ifany",
  missing_stat = "{N_miss_unweighted} ({p_miss_unweighted}%)",
  missing_text = "Missing"
) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**", all_stat_cols() ~ "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(all_stat_cols() ~ "**Race/Ethnicity**") %>%
  modify_caption("Table 1A: Unweighted N and Weighted Percentages for Key Demographics") %>%
  bold_labels()

# Print table
table1_complete

# ---------------------- #
# FIGURE 1: ADJUSTED PREVALENCE
# ---------------------- #

# Create prevalence plot with improved labels
plot_prevalence <- ggplot(
  final_combined_df,
  aes(
    x = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE), 
    y = weighted_prevalence, 
    group = type, 
    color = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE)
  ) 
) +
  geom_point(aes(shape = type), size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, position = position_dodge(width = 0.5)) +
  
  # Add repelled text labels with black text and gray connectors
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    position = position_dodge(width = 0.5),  # Align with points
    size = 4,  # Matches axis font size (~12pt in ggplot)
    color = "black",  # Ensure labels are black
    force = 15,  # Increase repelling force
    box.padding = 0.4,  # More space between text and points
    segment.color = "gray60",  # Light gray connector lines
    segment.size = 0.5  # Thin connector lines
  ) +
  
  # Labels
  labs(
    x = NULL,
    y = "Prevalence (%)",
    shape = "Estimate Type"
  ) +
  
  # Adjust legend markers
  scale_shape_manual(
    values = c("Crude" = 16, "Adjusted" = 17),
    labels = c("Crude", "Adjusted (sex, age)")
  ) +
  
  # Adjust y-axis
  scale_y_continuous(
    limits = c(0.05, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Adjust legend & theme
  guides(
    shape = guide_legend(order = 1, nrow = 3, byrow = TRUE),
    color = "none"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# View plot
plot_prevalence

# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

saveRDS(table1_imputed, file.path(results_dir, "table_1A.rds"))
saveRDS(table1_complete, file.path(results_dir, "table_1B.rds"))
table1_imputed %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1A.docx"))
table1_complete %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1B.docx"))
ggsave(filename = file.path(results_dir, "figure_1.png"),
       plot = plot_prevalence, width = 12, height = 5, dpi = 300)
