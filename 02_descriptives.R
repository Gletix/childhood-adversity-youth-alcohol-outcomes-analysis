# =============================================================================
# Script 02: Descriptive Statistics
# Project:   Childhood Maltreatment and Alcohol-Related Outcomes
# Dataset:   Australian Child Maltreatment Study (ACMS)
# Author:    Dhatsayini Rattambige
# Date:      09.04.2026
#
# Description:
#   Produces descriptive statistics for the sample, including Table 1
#   (demographic and clinical characteristics), distributions of maltreatment
#   types, mental health variables, and alcohol outcomes.
#
# Prerequisite:
#   Run 01_data_preparation.R before this script.
#   This script requires `data_clean` to be loaded in your environment.
#
# Script order:
#   01_data_preparation.R
#   02_descriptives.R  <- YOU ARE HERE
#   03_regression_models.R
#   04_sem_moderation.R
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load required packages
# -----------------------------------------------------------------------------

packages <- c("tableone", "knitr", "kableExtra", "dplyr")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# -----------------------------------------------------------------------------
# 2. Age statistics
# -----------------------------------------------------------------------------

mean_age  <- mean(data_clean$ELIG1, na.rm = TRUE)
sd_age    <- sd(data_clean$ELIG1, na.rm = TRUE)
age_range <- range(data_clean$ELIG1, na.rm = TRUE)

cat("=== Age Statistics ===\n")
cat("Mean:", round(mean_age, 2), "\n")
cat("SD:  ", round(sd_age, 2), "\n")
cat("Range:", age_range[1], "to", age_range[2], "\n\n")


# -----------------------------------------------------------------------------
# 3. Helper function: calculate counts and percentages for categorical variables
# -----------------------------------------------------------------------------

get_percentages <- function(x) {
  table_counts <- table(x, useNA = "ifany")
  percentages  <- prop.table(table_counts) * 100
  return(round(cbind(Count = table_counts, Percentage = percentages), 2))
}


# -----------------------------------------------------------------------------
# 4. Maltreatment type distributions
# -----------------------------------------------------------------------------

cat("=== Maltreatment Type Distributions ===\n")

print("Emotional Abuse (E_EA):")
print(get_percentages(data_clean$E_EA))

print("Physical Abuse (E_PA):")
print(get_percentages(data_clean$E_PA))

print("Sexual Abuse (E_SAb):")
print(get_percentages(data_clean$E_SAb))

print("Neglect (E_NEG):")
print(get_percentages(data_clean$E_NEG))

print("Exposure to Domestic Violence (E_EDV):")
print(get_percentages(data_clean$E_EDV))

cat("\nNumber of maltreatment types:\n")
print(table(data_clean$NumMaltreatmentTypes))


# -----------------------------------------------------------------------------
# 5. Mental health variable distributions
# -----------------------------------------------------------------------------

cat("\n=== Mental Health Distributions ===\n")

print("PTSD:")
print(get_percentages(data_clean$ptsd))

print("Generalised Anxiety Disorder (GAD):")
print(get_percentages(data_clean$gad))

print("Major Depressive Disorder (MDD):")
print(get_percentages(data_clean$mdd))


# -----------------------------------------------------------------------------
# 6. Alcohol outcome distributions
# -----------------------------------------------------------------------------

cat("\n=== Alcohol Outcome Distributions ===\n")

print("Any Alcohol Use Disorder (AUD):")
print(get_percentages(data_clean$aud))

print("Mild AUD:")
print(get_percentages(data_clean$aud_mild))

print("Moderate AUD:")
print(get_percentages(data_clean$aud_mod))

print("Severe AUD:")
print(get_percentages(data_clean$aud_sev))

print("Alcohol use frequency (HB7):")
print(get_percentages(data_clean$HB7))

print("Binge drinking (AlcoholBinge):")
print(get_percentages(data_clean$AlcoholBinge))


# -----------------------------------------------------------------------------
# 7. Demographic distributions
# -----------------------------------------------------------------------------

cat("\n=== Demographic Distributions ===\n")

print("Sex:")
print(get_percentages(data_clean$Sex))

print("Aboriginal and/or Torres Strait Islander status (ATSI):")
print(get_percentages(data_clean$ATSI))

print("Metropolitan vs rural/remote (MetRur):")
print(get_percentages(data_clean$MetRur))


# -----------------------------------------------------------------------------
# 8. Table 1: Full descriptive summary using tableone
#    This produces a publication-ready summary table of all key variables.
# -----------------------------------------------------------------------------

# Variables to include in Table 1
myVars <- c(
  "ELIG1",               # Age
  "Sex",
  "ATSI",
  "MetRur",
  "E_EA", "E_PA", "E_SAb", "E_NEG", "E_EDV",  # Maltreatment types
  "NumMaltreatmentTypes",
  "ptsd", "gad", "mdd",                          # Mental health
  "AlcoholBinge",                                 # Alcohol outcomes
  "HB7",
  "aud", "aud_mild", "aud_mod", "aud_sev"
)

# Categorical variables (everything except age)
catVars <- c(
  "Sex", "ATSI", "MetRur",
  "E_EA", "E_PA", "E_SAb", "E_NEG", "E_EDV",
  "NumMaltreatmentTypes",
  "ptsd", "gad", "mdd",
  "AlcoholBinge", "HB7",
  "aud", "aud_mild", "aud_mod", "aud_sev"
)

# Create Table 1
tab1 <- CreateTableOne(
  vars       = myVars,
  data       = data_clean,
  factorVars = catVars
)

# Print with all factor levels shown
# Age (ELIG1) is specified as non-normal, so median and IQR are reported
print(tab1,
  showAllLevels = TRUE,
  formatOptions = list(big.mark = ","),
  nonnormal     = "ELIG1"
)


# -----------------------------------------------------------------------------
# 9. Export Table 1 as HTML
# -----------------------------------------------------------------------------

table_matrix <- print(tab1,
  printToggle   = FALSE,
  showAllLevels = TRUE
)

kable(table_matrix,
  format  = "html",
  caption = "Table 1: Sample Descriptive Statistics"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE
  ) %>%
  save_kable("table1_descriptives.html")

cat("\nTable 1 saved to: table1_descriptives.html\n")
cat("Proceed to 03_regression_models.R\n")
