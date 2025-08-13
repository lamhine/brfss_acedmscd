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
    LLCPWT = coalesce(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`, `_LCPWTV3`),
    RACE = coalesce(`_RACE`, `_RACE1`),
    MEMLOSS = coalesce(CIMEMLOS, CIMEMLO1),
    INCOME = coalesce(INCOME2, INCOME3),
    HVYDRNK = coalesce(`_RFDRHV7`, `_RFDRHV8`),
    BPHIGH = coalesce(BPHIGH4, BPHIGH6),
    TOLDHI = coalesce(TOLDHI3, TOLDHI2)
  ) %>%
  select(
    -c(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`, `_LCPWTV3`, `_RACE`, `_RACE1`, 
       CIMEMLOS, CIMEMLO1, INCOME2, INCOME3, `_RFDRHV7`, `_RFDRHV8`, BPHIGH4, 
       BPHIGH6, TOLDHI3, TOLDHI2))

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
    DIABETE4, DIABTYPE, 
    # outcome = subjective cognitive decline
    MEMLOSS, 
    # auxiliary variables for multiple imputation
    `_EDUCAG`, INCOME, POORHLTH, ADDEPEV3, `_SMOKER3`, EXERANY2, CVDCRHD4, 
    CVDSTRK3, `_RFBMI5`, HVYDRNK, BPHIGH, TOLDHI 
  ) %>%
  # Rename problematic variables
  rename(
    STATE = `_STATE`,
    STSTR = `_STSTR`,
    PSU = `_PSU`,
    AGEG5YR = `_AGEG5YR`,
    EDUCA = `_EDUCAG`,
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
             ACEADNED, DIABETE4, DIABTYPE, MEMLOSS, ADDEPEV3, EXERANY2, 
             CVDCRHD4, CVDSTRK3, HVYDRNK, BPHIGH, TOLDHI),
           ~ case_when(
             . %in% c(7, 9) ~ NA_real_,  
             TRUE ~ .)
           ),
    # Variables where 9 = refuse, no 7 
    across(c(SMOKER3, EDUCA, OVWOB, RACE), ~ na_if(.x, 9)),
    # Other cases:
    AGEG5YR = na_if(AGEG5YR, 14),
    POORHLTH = case_when(
      POORHLTH == 88 ~ 0,  # None
      POORHLTH %in% c(77, 99) ~ NA_real_,  
      TRUE ~ POORHLTH),
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
    # First recode ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN: 1 = Yes, 2 = No → 1/0
    across(c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN), ~ case_when(
      .x == 1 ~ 1,
      .x == 2 ~ 0,
      TRUE ~ NA_real_
    )),
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
    # ACEADSAF and ACEADNED: reverse-coded neglect (1–3 = not protected → 1; 4–5 = protected → 0)
    across(c(ACEADSAF, ACEADNED), ~ case_when(
      .x %in% 1:3 ~ 1,
      .x %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    ))
  ) %>%
  mutate(
    # Convert all ACEs to factors with "Yes" as level 1 (for tables and models)
    across(
      c(ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1,
        ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, ACEADNED),
      ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
      )
    )


# Define mediator variable DIAB
df <- df %>%
  mutate(
    DIAB = case_when(
      DIABTYPE == 1 ~ "No",   # Type 1 diabetes
      DIABETE4 == 1 ~ "Yes",  # Type 2 diabetes
      DIABETE4 %in% c(2, 3, 4) ~ "No",
      TRUE ~ NA_character_
    ),
    DIAB = factor(DIAB, levels = c("No", "Yes"))
  ) %>% 
  select(-DIABETE4, -DIABTYPE) %>% 
  relocate(DIAB, .after = ACEADNED)

# Recode outcome (MEMLOSS) - already dichotomized but rename levels
df <- df %>%
  mutate(
    MEMLOSS = case_when(
      MEMLOSS == 1 ~ "Yes",
      MEMLOSS == 2 ~ "No",
      TRUE ~ NA_character_),
    MEMLOSS = factor(MEMLOSS, levels = c("No", "Yes"))
  )

# Recode imputation variables as necessary
df <- df %>% 
  mutate(
    EDUCA = case_when(
      EDUCA == 1 ~ "Did not graduate high school",
      EDUCA == 2 ~ "Graduated high school",
      EDUCA == 3 ~ "Attended college or technical school", 
      EDUCA == 4 ~ "Graduated college or technical school",
      TRUE ~ NA_character_),
    EDUCA = factor(
      EDUCA, levels = c(
        "Did not graduate high school", "Graduated high school",
        "Attended college or technical school", 
        "Graduated college or technical school")),
    
    INCOME = case_when(
      INCOME == 1 ~ "Less than $10,000",
      INCOME == 2 ~ "$10,000 to < $15,000",
      INCOME == 3 ~ "$15,000 to < $20,000",
      INCOME == 4 ~ "$20,000 to < $25,000",
      INCOME == 5 ~ "$25,000 to < $35,000",
      INCOME == 6 ~ "$35,000 to < $50,000",
      INCOME == 7 ~ "$50,000 to < $75,000",
      INCOME == 8 ~ "$100,000 or more",
      TRUE ~ NA_character_),
    INCOME = factor(
      INCOME, levels = c(
        "Less than $10,000", "$10,000 to < $15,000", "$15,000 to < $20,000",
        "$20,000 to < $25,000", "$25,000 to < $35,000", "$35,000 to < $50,000",
        "$50,000 to < $75,000","$100,000 or more")),
    
    POORHLTH = as.numeric(as.character(POORHLTH)),
    POORHLTH = case_when(
      POORHLTH %in% 15:30 ~ "Yes", # 15+ days 
      POORHLTH %in% 1:14 ~ "No",   # 14 or fewer days 
      TRUE ~ NA_character_),
    POORHLTH = factor(POORHLTH, levels = c("No", "Yes")),
    
    ADDEPEV3 = case_when(
      ADDEPEV3 == 1 ~ "Yes",
      ADDEPEV3 == 2 ~ "No",
      TRUE ~ NA_character_),
    ADDEPEV3 = factor(ADDEPEV3, levels = c("No", "Yes")),
    
    SMOKER3 = case_when(
      SMOKER3 %in% c(1, 2) ~ "Current", # Everyday OR some days)
      SMOKER3 == 3 ~ "Former",         
      SMOKER3 == 4 ~ "Never",
      TRUE ~ NA_character_),
    SMOKER3 = factor(SMOKER3, levels = c("Never", "Former", "Current")),
    
    EXERANY2 = case_when(
      EXERANY2 == 1 ~ "Yes",
      EXERANY2 == 2 ~ "No",
      TRUE ~ NA_character_),
    EXERANY2 = factor(EXERANY2, levels = c("No", "Yes")),
    
    CVDCRHD4 = case_when(
      CVDCRHD4 == 1 ~ "Yes",
      CVDCRHD4 == 2 ~ "No",
      TRUE ~ NA_character_),
    CVDCRHD4 = factor(CVDCRHD4, levels = c("No", "Yes")),
    
    CVDSTRK3 = case_when(
      CVDSTRK3 == 1 ~ "Yes",
      CVDSTRK3 == 2 ~ "No",
      TRUE ~ NA_character_),
    CVDSTRK3 = factor(CVDSTRK3, levels = c("No", "Yes")),
    
    OVWOB = case_when(
      OVWOB == 1 ~ "No",      # Reverse-coded
      OVWOB == 2 ~ "Yes",
      TRUE ~ NA_character_),
    OVWOB = factor(OVWOB, levels = c("No", "Yes")),
    
    HVYDRNK = case_when(
      HVYDRNK == 1 ~ "No",    # Reverse-coded
      HVYDRNK == 2 ~ "Yes",
      TRUE ~ NA_character_),
    HVYDRNK = factor(HVYDRNK, levels = c("No", "Yes")),
    
    BPHIGH = case_when(
      BPHIGH %in% c(1, 4) ~ "Yes", # HT or borderline HT
      BPHIGH %in% c(2, 3) ~ "No",
      TRUE ~ NA_character_),
    BPHIGH = factor(BPHIGH, levels = c("No", "Yes")),
    
    TOLDHI = case_when(
      TOLDHI == 1 ~ "Yes", 
      TOLDHI == 2 ~ "No",
      TRUE ~ NA_character_),
    TOLDHI = factor(TOLDHI, levels = c("No", "Yes"))
    )

# Remove participants under age 45 and drop unused factor levels
df <- df %>%
  filter(!(AGEG5YR %in% 
             c("Age 18 to 24",
               "Age 25 to 29",
               "Age 30 to 34",
               "Age 35 to 39",
               "Age 40 to 44"))) %>%
  mutate(AGEG5YR = droplevels(AGEG5YR))

# Add labels to variable names
var_label(df) <- list(
  AGEG5YR = "Age category",
  SEXVAR = "Biological sex",
  RACE = "Self-reported race or ethnicity",
  ACEDEPRS = "ACE: household mental illness",
  ACEDRINK = "ACE: household alcohol abuse",
  ACEDRUGS = "ACE: household substance abuse",
  ACEPRISN = "ACE: household member incarcerated",
  ACEDIVRC = "ACE: parental divorce or separation",
  ACEPUNCH = "ACE: household domestic violence",
  ACEHURT1 = "ACE: physical abuse",
  ACESWEAR = "ACE: verbal abuse",
  ACETOUCH = "ACE: touched inappropriately",
  ACETTHEM = "ACE: forced to touch inappropriately",
  ACEHVSEX = "ACE: forced sexual intercourse",
  ACEADSAF = "ACE: did not feel safe or protected",
  ACEADNED = "ACE: basic needs were not met",
  DIAB = "Type 2 diabetes",
  MEMLOSS = "Subjective cognitive decline",
  EDUCA = "Educational attainment",
  INCOME = "Income level",
  POORHLTH = "Poor health prevented usual activites most days",
  ADDEPEV3 = "Depressive disorder",
  SMOKER3 = "Smoking status",
  EXERANY2 = "Any exercise in last 30 days",
  CVDCRHD4 = "Angina or coronary heart disease",
  CVDSTRK3 = "Ever had stroke",
  OVWOB = "Body mass index > 25.0",
  HVYDRNK = "Binge drinking",
  BPHIGH = "High blood pressure",
  TOLDHI = "High cholesterol"
)

dfSummary(df)

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(df, file.path(processed_data_dir, "02_cleaned_data.rds"))

