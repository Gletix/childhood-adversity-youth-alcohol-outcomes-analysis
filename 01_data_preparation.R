# =============================================================================
# Script 01: Data Preparation
# Project:   Childhood Maltreatment and Alcohol-Related Outcomes
# Dataset:   Australian Child Maltreatment Study (ACMS)
# Author:    Dhatsayini Rattambige
# Date:      09.04.2026
#
# Description:
#   This script prepares the raw ACMS data for analysis. It handles missing
#   value coding, creates key derived variables (maltreatment combinations,
#   ordinal AUD severity, age-of-onset variables), and applies variable
#   transformations needed for regression and SEM models in later scripts.
#
# NOTE ON DATA ACCESS:
#   The ACMS data are not publicly available due to participant confidentiality.
#   Researchers wishing to access the data should apply through the ACMS data
#   access process: https://www.acms.au/
#   This script assumes the cleaned dataset is already loaded as `data_clean`.
#
# Script order:
#   01_data_preparation.R  <- YOU ARE HERE
#   02_descriptives.R
#   03_regression_models.R
#   04_sem_moderation.R
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load required packages
# -----------------------------------------------------------------------------

packages <- c("dplyr", "haven")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# -----------------------------------------------------------------------------
# 2. Variable name reference guide
#
#   ELIG1              = Participant age
#   Sex                = Sex (1 = Male, 2 = Female, 9 = Gender diverse)
#   ATSI               = Aboriginal and/or Torres Strait Islander status
#   MetRur             = Metropolitan vs rural/remote location
#
#   Maltreatment exposure variables (binary: 0 = No, 1 = Yes):
#     E_EA             = Emotional abuse
#     E_PA             = Physical abuse
#     E_SAb            = Sexual abuse
#     E_NEG            = Neglect
#     E_EDV            = Exposure to domestic violence
#
#   NumMaltreatmentTypes = Count of maltreatment types experienced (0-5)
#
#   Alcohol outcomes:
#     AlcoholBinge     = Binge drinking (binary)
#     HB7              = Alcohol use frequency (ordinal: 1=Never to 5=Daily)
#     aud              = Alcohol use disorder (any; binary)
#     aud_mild         = Mild AUD (binary)
#     aud_mod          = Moderate AUD (binary)
#     aud_sev          = Severe AUD (binary)
#     AUD_ordinal      = AUD severity (ordered: None/Mild/Moderate/Severe)
#
#   Mental health covariates (binary: 0 = No, 1 = Yes):
#     ptsd             = Post-traumatic stress disorder
#     gad              = Generalised anxiety disorder
#     mdd              = Major depressive disorder
#
#   ACE variables (binary: 0 = No, 1 = Yes):
#     ACE1-ACE7        = Adverse childhood experience items 1-7
#     ACE8             = ACE item 8 (recoded: >= 3 = 1, otherwise = 0)
#
#   Age of first maltreatment (used to derive EarliestOnset):
#     EA1_AgeFirst to EA3_AgeFirst   = Emotional abuse
#     NEG1_AgeFirst to NEG3_AgeFirst = Neglect
#     PA1_AgeFirst to PA2_AgeFirst   = Physical abuse
#     SA1_AgeFirst to SA5_AgeFirst   = Sexual abuse
#     EDV1_AgeFirst to EDV4_AgeFirst = Domestic violence exposure
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 3. Recode missing values
#    In ACMS, -99 and -98 are used as missing value codes.
#    We replace these with NA so R treats them as missing.
# -----------------------------------------------------------------------------

# Identify numeric columns
numeric_cols <- sapply(data_clean, is.numeric)

# Replace -99 and -98 with NA in numeric columns
data_clean[, numeric_cols][data_clean[, numeric_cols] == -99] <- NA
data_clean[, numeric_cols][data_clean[, numeric_cols] == -98] <- NA

# Replace -99 and -98 with NA in character columns (if any)
char_cols <- sapply(data_clean, is.character)
data_clean[, char_cols][data_clean[, char_cols] == "-99"] <- NA
data_clean[, char_cols][data_clean[, char_cols] == "-98"] <- NA


# -----------------------------------------------------------------------------
# 4. Convert haven-labelled variables to numeric
#    Data imported from SPSS (.sav) via haven may have labelled types.
#    These need converting before use in models.
# -----------------------------------------------------------------------------

data_clean$ptsd <- as.numeric(data_clean$ptsd)
data_clean$mdd  <- as.numeric(data_clean$mdd)
data_clean$gad  <- as.numeric(data_clean$gad)


# -----------------------------------------------------------------------------
# 5. Create ordinal AUD severity variable
#    Combines three binary AUD variables into one ordered outcome:
#      0 = No AUD
#      1 = Mild AUD
#      2 = Moderate AUD
#      3 = Severe AUD
#    Severity takes precedence (severe > moderate > mild > none).
# -----------------------------------------------------------------------------

data_clean$AUD_ordinal <- with(data_clean,
  dplyr::case_when(
    aud_sev == 1                                         ~ 3,  # Severe
    aud_sev == 0 & aud_mod == 1                          ~ 2,  # Moderate
    aud_sev == 0 & aud_mod == 0 & aud_mild == 1          ~ 1,  # Mild
    aud_sev == 0 & aud_mod == 0 & aud_mild == 0          ~ 0   # None
  )
)

data_clean$AUD_ordinal <- factor(
  data_clean$AUD_ordinal,
  levels = c(0, 1, 2, 3),
  labels = c("None", "Mild", "Moderate", "Severe"),
  ordered = TRUE
)


# -----------------------------------------------------------------------------
# 6. Convert HB7 (alcohol use frequency) to ordered factor
#    Levels: 1 = Never, 2 = Less than monthly, 3 = 1-3 times/month,
#            4 = 1-4 times/week, 5 = Daily or almost daily
# -----------------------------------------------------------------------------

data_clean$HB7 <- factor(
  data_clean$HB7,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Never", "Less than monthly", "1-3 times/month",
             "1-4 times/week", "Daily"),
  ordered = TRUE
)


# -----------------------------------------------------------------------------
# 7. Create dummy variables for Sex
#    Reference category = Male (Sex == 1)
# -----------------------------------------------------------------------------

data_clean$Sex_Female  <- as.numeric(data_clean$Sex == 2)  # 1 = Female
data_clean$Sex_Diverse <- as.numeric(data_clean$Sex == 9)  # 1 = Gender diverse


# -----------------------------------------------------------------------------
# 8. Create maltreatment combination variables
#    Each variable flags participants who experienced exactly that combination
#    of maltreatment types (and no others).
#    Used in the multi-type regression analyses in Script 03.
# -----------------------------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    # Single maltreatment types only
    CM_EA_only  = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb == 0 & E_NEG == 0 & E_EDV == 0),
    CM_PA_only  = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb == 0 & E_NEG == 0 & E_EDV == 0),
    CM_SA_only  = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb > 0 & E_NEG == 0 & E_EDV == 0),
    CM_NEG_only = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb == 0 & E_NEG > 0 & E_EDV == 0),
    CM_EDV_only = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb == 0 & E_NEG == 0 & E_EDV > 0),

    # Two-type combinations
    CM_EA_PA  = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb == 0 & E_NEG == 0 & E_EDV == 0),
    CM_EA_SA  = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb > 0 & E_NEG == 0 & E_EDV == 0),
    CM_EA_NEG = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb == 0 & E_NEG > 0 & E_EDV == 0),
    CM_EA_EDV = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb == 0 & E_NEG == 0 & E_EDV > 0),
    CM_PA_SA  = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb > 0 & E_NEG == 0 & E_EDV == 0),
    CM_PA_NEG = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb == 0 & E_NEG > 0 & E_EDV == 0),
    CM_PA_EDV = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb == 0 & E_NEG == 0 & E_EDV > 0),
    CM_SA_NEG = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb > 0 & E_NEG > 0 & E_EDV == 0),
    CM_SA_EDV = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb > 0 & E_NEG == 0 & E_EDV > 0),
    CM_NEG_EDV = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb == 0 & E_NEG > 0 & E_EDV > 0),

    # Three-type combinations
    CM_EA_PA_SA   = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb > 0 & E_NEG == 0 & E_EDV == 0),
    CM_EA_PA_NEG  = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb == 0 & E_NEG > 0 & E_EDV == 0),
    CM_EA_PA_EDV  = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb == 0 & E_NEG == 0 & E_EDV > 0),
    CM_EA_SA_NEG  = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb > 0 & E_NEG > 0 & E_EDV == 0),
    CM_EA_SA_EDV  = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb > 0 & E_NEG == 0 & E_EDV > 0),
    CM_EA_NEG_EDV = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb == 0 & E_NEG > 0 & E_EDV > 0),
    CM_PA_SA_NEG  = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb > 0 & E_NEG > 0 & E_EDV == 0),
    CM_PA_SA_EDV  = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb > 0 & E_NEG == 0 & E_EDV > 0),
    CM_PA_NEG_EDV = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb == 0 & E_NEG > 0 & E_EDV > 0),
    CM_SA_NEG_EDV = as.numeric(E_EA == 0 & E_PA == 0 & E_SAb > 0 & E_NEG > 0 & E_EDV > 0),

    # Four-type combinations
    CM_EA_PA_SA_NEG  = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb > 0 & E_NEG > 0 & E_EDV == 0),
    CM_EA_PA_SA_EDV  = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb > 0 & E_NEG == 0 & E_EDV > 0),
    CM_EA_PA_NEG_EDV = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb == 0 & E_NEG > 0 & E_EDV > 0),
    CM_EA_SA_NEG_EDV = as.numeric(E_EA > 0 & E_PA == 0 & E_SAb > 0 & E_NEG > 0 & E_EDV > 0),
    CM_PA_SA_NEG_EDV = as.numeric(E_EA == 0 & E_PA > 0 & E_SAb > 0 & E_NEG > 0 & E_EDV > 0),

    # All five maltreatment types
    CM_ALL = as.numeric(E_EA > 0 & E_PA > 0 & E_SAb > 0 & E_NEG > 0 & E_EDV > 0)
  )


# -----------------------------------------------------------------------------
# 9. Create earliest age of maltreatment onset variable
#    Takes the minimum (earliest) age across all maltreatment-specific
#    age-of-first-occurrence variables.
#    Rows where all age values are NA are set to NA (not 0 or Inf).
# -----------------------------------------------------------------------------

age_vars <- c(
  "EA1_AgeFirst", "EA2_AgeFirst", "EA3_AgeFirst",
  "NEG1_AgeFirst", "NEG2_AgeFirst", "NEG3_AgeFirst",
  "PA1_AgeFirst", "PA2_AgeFirst",
  "SA1_AgeFirst", "SA2_AgeFirst", "SA3_AgeFirst", "SA4_AgeFirst", "SA5_AgeFirst",
  "EDV1_AgeFirst", "EDV2_AgeFirst", "EDV3_AgeFirst", "EDV4_AgeFirst"
)

data_clean$EarliestOnset <- apply(
  data_clean[age_vars], 1,
  function(x) min(x, na.rm = TRUE)
)

# Replace Inf (produced when all values in a row were NA) with NA
data_clean$EarliestOnset[is.infinite(data_clean$EarliestOnset)] <- NA


# -----------------------------------------------------------------------------
# 10. Create centred variables for moderation analysis
#     Centring subtracts the mean from each value so that 0 represents the
#     average participant. This makes interaction terms easier to interpret.
# -----------------------------------------------------------------------------

data_clean$NumMaltreatmentTypes_c <- scale(
  data_clean$NumMaltreatmentTypes, center = TRUE, scale = FALSE
)

data_clean$EarliestOnset_c <- scale(
  data_clean$EarliestOnset, center = TRUE, scale = FALSE
)

# Interaction term: number of types x earliest onset (both centred)
data_clean$NT_EarliestOnset <- data_clean$NumMaltreatmentTypes_c * data_clean$EarliestOnset_c


# -----------------------------------------------------------------------------
# 11. Recode ACE variables (ACE1-ACE8)
#     ACE1-ACE7: Binary (1 = Yes, 0 = No). Recode -98/-99 to NA then to 0.
#     ACE8:      Frequency item; recoded to binary (1 = Somewhat/Very often,
#                i.e., original score >= 3).
# -----------------------------------------------------------------------------

for (i in 1:7) {
  ace_col <- paste0("ACE", i)
  data_clean[[ace_col]][data_clean[[ace_col]] %in% c(-98, -99)] <- NA
  data_clean[[ace_col]] <- as.numeric(
    !is.na(data_clean[[ace_col]]) & data_clean[[ace_col]] == 1
  )
}

data_clean$ACE8[data_clean$ACE8 %in% c(-98, -99)] <- NA
data_clean$ACE8 <- as.numeric(data_clean$ACE8 >= 3)


# -----------------------------------------------------------------------------
# 12. Create ACE interaction terms for moderation analysis
#     Each term = NumMaltreatmentTypes x ACE_i
#     Used in SEM moderation models in Script 04.
# -----------------------------------------------------------------------------

for (i in 1:8) {
  ace_col         <- paste0("ACE", i)
  interaction_col <- paste0("NumMalt_ACE", i)
  data_clean[[interaction_col]] <- data_clean$NumMaltreatmentTypes * data_clean[[ace_col]]
}


# -----------------------------------------------------------------------------
# 13. Ensure NumMaltreatmentTypes is numeric
# -----------------------------------------------------------------------------

data_clean$NumMaltreatmentTypes <- as.numeric(
  as.character(data_clean$NumMaltreatmentTypes)
)


# -----------------------------------------------------------------------------
# 14. Quick verification checks
# -----------------------------------------------------------------------------

cat("=== Data preparation complete ===\n\n")

cat("Sample size after cleaning:", nrow(data_clean), "\n\n")

cat("Age summary (ELIG1):\n")
print(summary(data_clean$ELIG1))

cat("\nAUD ordinal distribution:\n")
print(table(data_clean$AUD_ordinal, useNA = "ifany"))

cat("\nHB7 distribution:\n")
print(table(data_clean$HB7, useNA = "ifany"))

cat("\nMaltreatment type counts:\n")
cm_vars <- c("E_EA", "E_PA", "E_SAb", "E_NEG", "E_EDV")
for (v in cm_vars) {
  cat(v, ":", sum(data_clean[[v]], na.rm = TRUE), "\n")
}

cat("\nData preparation complete. Proceed to 02_descriptives.R\n")
