# 02_clean_data.R
# Purpose: Clean and preprocess BRFSS data for ACEs -> Diabetes -> SCD analysis

# ---------------------- #
# LOAD CONFIGURATION
# ---------------------- #

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND PROCESS FILTERED DATASET
# ---------------------- #

# Load the single filtered dataset
filtered_dfs <- readRDS(file.path(processed_data_dir, "01_filtered_data.rds"))

# Convert list of dataframes into a single dataframe, preserving dataset name
df <- bind_rows(filtered_dfs, .id = "dataset")  

# Extract year from dataset name
df <- df %>%
  mutate(
    year = case_when(
      str_detect(dataset, "V\\d$") ~ as.numeric(paste0("20", str_extract(dataset, "\\d{2}"))), 
      str_detect(dataset, "\\d{4}$") ~ as.numeric(str_extract(dataset, "\\d{4}$")), 
      TRUE ~ NA_real_  # Explicitly handle unexpected cases
    )
  )

# Coalesce variables that have different names across years
df <- df %>%
  mutate(
    LLCPWT = coalesce(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`),
    RACE = coalesce(`_RACE`, `_RACE1`),
    # DIABAGE = coalesce(DIABAGE3, DIABAGE4), REMOVING BECAUSE DEEMED UNNECESSARY
    PREDIAB = coalesce(PREDIAB1, PREDIAB2),
    DIABED = coalesce(DIABEDU, DIABEDU1),
    MEMLOSS = coalesce(CIMEMLOS, CIMEMLO1),
    INCOME = coalesce(INCOME2, INCOME3),
    HVYDRNK = coalesce(`_RFDRHV7`, `_RFDRHV8`),
    BPHIGH = coalesce(BPHIGH4, BPHIGH6),
    TOLDHI = coalesce(TOLDHI3, TOLDHI2)
  ) %>%
  select(
    -c(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`, `_RACE`, `_RACE1`, PREDIAB1, PREDIAB2, 
       DIABEDU, DIABEDU1, CIMEMLOS, CIMEMLO1, INCOME2, INCOME3, `_RFDRHV7`, 
       `_RFDRHV8`, BPHIGH4, BPHIGH6, TOLDHI3, TOLDHI2))

# Put variables back in nice order and rename problematic ones
df <- df %>% 
  # Reorder variables in data frame, keep extra vars for imputation
  select(
    # metadata
    year, dataset, 
    # ID variables
    `_STATE`, `_STSTR`, LLCPWT, `_PSU`, 
    # confounders
    `_AGEG5YR`, SEXVAR, RACE, 
    # exposure = ACEs
    ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR, 
    ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, ACEADNED, 
    # mediator and related vars = diabetes
    DIABETE4, PREDIAB, DIABTYPE, DIABED, 
    # outcome = subjective cognitive decline
    MEMLOSS, 
    # auxiliary variables for multiple imputation
    EDUCA, MENTHLTH, ADDEPEV3, `_SMOKER3`, EXERANY2, CVDCRHD4, CVDSTRK3, 
    `_RFBMI5`, HVYDRNK, BPHIGH, TOLDHI, INCOME
  ) %>%
  # Rename problematic variables
  rename(
    STATE = `_STATE`,
    STSTR = `_STSTR`,
    PSU = `_PSU`,
    AGEG5YR = `_AGEG5YR`,
    SMOKER3 = `_SMOKER3`,
    OVWOB = `_RFBMI5`
  )

# ---------------------- #
# RECODE VARIABLES
# ---------------------- #

# Recode variables with missing data as NA and relevant ones to factor
df <- df %>%
  mutate(
    # Variables where 7 = don't know, 9 = refuse
    across(c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, 
             ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, 
             ACEADNED, DIABETE4, PREDIAB, DIABTYPE, DIABED, MEMLOSS, ADDEPEV3, 
             EXERANY2, CVDCRHD4, CVDSTRK3, HVYDRNK, BPHIGH, TOLDHI),
           ~ case_when(
             . %in% c(7, 9) ~ NA_real_,  
             TRUE ~ .)
           ),
    # Variables where 9 = refuse, no 7 
    across(c(SMOKER3, EDUCA, OVWOB, RACE), ~ na_if(.x, 9)),
    # Other cases:
    AGEG5YR = na_if(AGEG5YR, 14),
    MENTHLTH = case_when(
      MENTHLTH == 88 ~ 0,  # None
      MENTHLTH %in% c(77, 99) ~ NA_real_,  
      TRUE ~ MENTHLTH),
    INCOME = case_when(
      INCOME %in% c(77, 99) ~ NA_real_, 
      INCOME %in% c(8, 9, 10, 11) ~ 8,  # Collapsing categories
      TRUE ~ INCOME),
    # Coerce relevant variables to factor
    across(c(STATE, AGEG5YR:INCOME), as.factor)
    )

# Recode confounder variables
df <- df %>%
  mutate(
    # Label and order AGEG5YR
    AGEG5YR = factor(AGEG5YR,
                     levels = 1:13,
                     labels = c(
                       "Age 18 to 24",
                       "Age 25 to 29",
                       "Age 30 to 34",
                       "Age 35 to 39",
                       "Age 40 to 44",
                       "Age 45 to 49",
                       "Age 50 to 54",
                       "Age 55 to 59",
                       "Age 60 to 64",
                       "Age 65 to 69",
                       "Age 70 to 74",
                       "Age 75 to 79",
                       "Age 80 or older"
                     )
    ),
    
    # Recode SEXVAR: 1 = Male, 2 = Female → factor with Female first
    SEXVAR = factor(SEXVAR,
                    levels = c(2, 1),
                    labels = c("Female", "Male")),
    
    # Recode RACE: convert to integer, replace NA with 9
    RACE = as.integer(as.character(RACE)),
    RACE = if_else(is.na(RACE), 9L, RACE),
    RACE = factor(RACE,
                  levels = c(3, 4, 2, 8, 7, 5, 1, 6, 9),
                  labels = c(
                    "American Indian or Alaska Native",
                    "Asian",
                    "Black",
                    "Hispanic",
                    "Multiracial",
                    "Native Hawaiian or Pacific Islander",
                    "White",
                    "Other race",
                    "Don't know / Refused"
                  )
    )
  )

# Recode ACEs variables (ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN already okay)
df <- df %>%
  mutate(
    # Parental divorce (1 = yes, 2 = no, 8 = never married → treat as no)
    ACEDIVRC = case_when(
      ACEDIVRC == 1 ~ 1,
      ACEDIVRC %in% c(2, 8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ACEs with 3 levels (1 = never, 2 = once, 3 = more than once → 2/3 = ever)
    across(c(ACEPUNCH, ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX), ~ case_when(
      .x %in% c(2, 3) ~ 1,
      .x == 1 ~ 0,
      TRUE ~ NA_real_
    )),
    
    # ACEADSAF (protection): reverse-coded neglect (1–3 = not protected → 1; 4–5 = protected → 0)
    ACEADSAF = case_when(
      ACEADSAF %in% 1:3 ~ 1,
      ACEADSAF %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ACEADNED (support): same coding as ACEADSAF
    ACEADNED = case_when(
      ACEADNED %in% 1:3 ~ 1,
      ACEADNED %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    # Convert all ACEs to factors with "Yes" as level 1 (for tables and models)
    across(
      c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1,
        ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, ACEADNED),
      ~ factor(.x, levels = c(1, 0), labels = c("Yes", "No"))
    )
  )


# Define mediator variable HIGLYC: ever told have non-gestational diabetes OR 
# prediabetes, never told, or type 1 diabetic
df <- df %>%
  mutate(
    HIGLYC = case_when(
      DIABTYPE == 1 ~ "Type1",
      DIABETE4 == 1 ~ "Yes",  # type 2
      DIABETE4 == 4 | PREDIAB == 1 ~ "Yes",  # prediabetes
      DIABETE4 == 3 & PREDIAB == 3 ~ "No",
      TRUE ~ NA_character_
    ),
    HIGLYC = factor(HIGLYC, levels = c("Yes", "No", "Type1"))
  ) %>% 
  select(-DIABETE4, -PREDIAB, -DIABTYPE) %>% 
  relocate(HIGLYC, .after = ACEADNED)

# Potential for future project: define and recode intervention variable DIABEDU
df <- df %>%
  mutate(
    DIABED = case_when(
      DIABED == 1 ~ "Yes",
      DIABED == 2 ~ "No",
      HIGLYC != 1 ~ "Ineligible",  # Not diabetic or unknown
      TRUE ~ NA_character_
    ),
    DIABED = factor(DIABED, levels = c("Yes", "No", "Ineligible"))
  )

# Recode outcome (MEMLOSS) - already dichotomized but rename levels
df <- df %>%
  mutate(
    MEMLOSS = case_when(
      MEMLOSS == 1 ~ "Yes",
      MEMLOSS == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    MEMLOSS = factor(MEMLOSS, levels = c("Yes", "No"))
  )

# Remove participants under age 45 and drop unused factor levels
df <- df %>%
  filter(!(AGEG5YR %in% c("1", "2", "3", "4", "5"))) %>%
  mutate(AGEG5YR = droplevels(AGEG5YR))

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(df, file.path(processed_data_dir, "02_cleaned_data.rds"))

