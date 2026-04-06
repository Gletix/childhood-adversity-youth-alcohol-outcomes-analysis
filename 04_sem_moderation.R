# =============================================================================
# Script 04: SEM Moderation Models
# Project:   Childhood Maltreatment and Alcohol-Related Outcomes
# Dataset:   Australian Child Maltreatment Study (ACMS)
# Author:    Dhatsayini Rattambige
# Date:      09.04.2026
#
# Description:
#   This script runs structural equation modelling (SEM) to test moderation
#   effects on alcohol outcomes. It is organised into four parts:
#
#   Part A: Direct effects of individual maltreatment types on alcohol outcomes
#           (SEM approach, unadjusted; complements the regression models in
#           Script 03)
#
#   Part B: Moderation by earliest age of maltreatment onset
#           Tests whether the effect of number of maltreatment types on alcohol
#           outcomes is moderated by how early maltreatment began.
#           Outcomes: HB7 (alcohol frequency), AlcoholBinge
#
#   Part C: Moderation by individual ACE types (ACE1-ACE8)
#           Tests whether each ACE moderates the effect of number of
#           maltreatment types on alcohol outcomes.
#           Outcomes: HB7, AlcoholBinge
#
#   Part D: Moderation by multitype ACE score
#           Tests whether a composite ACE count (0-8) moderates the effect
#           of maltreatment type count on alcohol outcomes.
#           Outcomes: AUD_ordinal, HB7, AlcoholBinge
#
# Estimator notes:
#   - Ordinal outcomes (HB7, AUD_ordinal): WLSMV estimator
#   - Binary outcomes (AlcoholBinge): ML estimator
#
# Prerequisite:
#   Run 01_data_preparation.R before this script.
#   This script requires `data_clean` to be loaded in your environment.
#
# Script order:
#   01_data_preparation.R
#   02_descriptives.R
#   03_regression_models.R
#   04_sem_moderation.R  <- YOU ARE HERE
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load required packages
# -----------------------------------------------------------------------------

packages <- c("lavaan", "dplyr", "openxlsx", "kableExtra", "knitr", "ggplot2")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# =============================================================================
# PART A: Direct effects of individual maltreatment types (SEM)
#         Each maltreatment type is entered simultaneously as a predictor.
#         Unlike the separate models in Script 03, this approach controls for
#         the presence of other maltreatment types.
# =============================================================================

cat("=== Part A: SEM direct effects of individual maltreatment types ===\n\n")

# Note: CM_EA_only through CM_EDV_only were created in 01_data_preparation.R
# These are binary flags for participants experiencing ONLY that maltreatment type.


# A1. AUD severity (AUD_ordinal) - ordinal outcome, use WLSMV
model_direct_aud <- '
  AUD_ordinal ~ ea*CM_EA_only + pa*CM_PA_only + sa*CM_SA_only +
                neg*CM_NEG_only + edv*CM_EDV_only
'

fit_direct_aud <- sem(model_direct_aud,
                      data      = data_clean,
                      ordered   = "AUD_ordinal",
                      estimator = "WLSMV")

cat("Direct effects on AUD_ordinal:\n")
print(summary(fit_direct_aud, fit.measures = TRUE, standardized = TRUE))


# A2. Alcohol use frequency (HB7) - ordinal outcome, use WLSMV
model_direct_hb7 <- '
  HB7 ~ ea*CM_EA_only + pa*CM_PA_only + sa*CM_SA_only +
        neg*CM_NEG_only + edv*CM_EDV_only
'

fit_direct_hb7 <- sem(model_direct_hb7,
                      data      = data_clean,
                      ordered   = "HB7",
                      estimator = "WLSMV")

cat("\nDirect effects on HB7:\n")
print(summary(fit_direct_hb7, fit.measures = TRUE, standardized = TRUE))


# A3. Binge drinking (AlcoholBinge) - binary outcome, use ML
model_direct_binge <- '
  AlcoholBinge ~ ea*CM_EA_only + pa*CM_PA_only + sa*CM_SA_only +
                 neg*CM_NEG_only + edv*CM_EDV_only
'

fit_direct_binge <- sem(model_direct_binge,
                        data      = data_clean,
                        estimator = "ML")

cat("\nDirect effects on AlcoholBinge:\n")
print(summary(fit_direct_binge, fit.measures = TRUE, standardized = TRUE))


# =============================================================================
# PART B: Moderation by earliest age of maltreatment onset
#
#         Research question: Does the age at which maltreatment first occurred
#         moderate the relationship between number of maltreatment types and
#         alcohol outcomes?
#
#         Predictors (all centred):
#           NumMaltreatmentTypes_c  = number of maltreatment types (centred)
#           EarliestOnset_c         = earliest age of onset (centred)
#           NT_EarliestOnset        = interaction term (both centred)
#
#         Covariates: Sex (Female, Diverse), PTSD, GAD, MDD
#
#         Note: Centring means we subtract the group average from each score.
#         This makes 0 = the average participant, and helps with interpretation
#         of the interaction term.
# =============================================================================

cat("\n=== Part B: Moderation by earliest age of maltreatment onset ===\n\n")

# B1. Outcome: HB7 (alcohol use frequency) - ordinal, WLSMV

sem_model_onset_hb7 <- '
  HB7 ~ b1*NumMaltreatmentTypes_c + b2*EarliestOnset_c + b3*NT_EarliestOnset +
        c1*Sex_Female + c2*Sex_Diverse +
        c3*ptsd + c4*gad + c5*mdd
'

sem_fit_onset_hb7 <- sem(sem_model_onset_hb7,
                         data      = data_clean,
                         ordered   = "HB7",
                         estimator = "WLSMV")

results_hb7 <- standardizedSolution(sem_fit_onset_hb7)
mod_hb7 <- results_hb7[results_hb7$label %in%
                          c("b1", "b2", "b3", "c1", "c2", "c3", "c4", "c5"), ]

mod_table_hb7 <- data.frame(
  Predictor = c(
    "Number of maltreatment types",
    "Earliest age of onset",
    "Interaction (types x age of onset)",
    "Sex (Female)", "Sex (Diverse)",
    "PTSD", "GAD", "MDD"
  ),
  Estimate = round(mod_hb7$est.std, 3),
  SE       = round(mod_hb7$se, 3),
  p_value  = round(mod_hb7$pvalue, 3)
)

cat("Moderation results: HB7\n")
print(mod_table_hb7)

# Export as HTML table
html_table_hb7 <- kable(mod_table_hb7,
  caption = "Moderation of HB7 by Earliest Age of Onset (with covariates)",
  format  = "html"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Standardised Results" = 3))

writeLines(as.character(html_table_hb7),
           "moderation_results_HB7_onset_with_covariates.html")
cat("Saved: moderation_results_HB7_onset_with_covariates.html\n\n")


# B2. Outcome: AlcoholBinge (binary) - ML estimator

sem_model_onset_binge <- '
  AlcoholBinge ~ b1*NumMaltreatmentTypes_c + b2*EarliestOnset_c + b3*NT_EarliestOnset +
                 c1*Sex_Female + c2*Sex_Diverse +
                 c3*ptsd + c4*gad + c5*mdd
'

sem_fit_onset_binge <- sem(sem_model_onset_binge,
                           data      = data_clean,
                           estimator = "ML")

results_binge <- standardizedSolution(sem_fit_onset_binge)
mod_binge <- results_binge[results_binge$label %in%
                             c("b1", "b2", "b3", "c1", "c2", "c3", "c4", "c5"), ]

mod_table_binge <- data.frame(
  Predictor = c(
    "Number of maltreatment types",
    "Earliest age of onset",
    "Interaction (types x age of onset)",
    "Sex (Female)", "Sex (Diverse)",
    "PTSD", "GAD", "MDD"
  ),
  Estimate = round(mod_binge$est.std, 3),
  SE       = round(mod_binge$se, 3),
  p_value  = round(mod_binge$pvalue, 3)
)

cat("Moderation results: AlcoholBinge\n")
print(mod_table_binge)

html_table_binge <- kable(mod_table_binge,
  caption = "Moderation of Binge Drinking by Earliest Age of Onset (with covariates)",
  format  = "html"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Standardised Results" = 3))

writeLines(as.character(html_table_binge),
           "moderation_results_AlcoholBinge_onset_with_covariates.html")
cat("Saved: moderation_results_AlcoholBinge_onset_with_covariates.html\n\n")

# Distribution check for EarliestOnset
cat("Summary of Earliest Age of Onset:\n")
print(summary(data_clean$EarliestOnset))


# =============================================================================
# PART C: Moderation by individual ACE types (ACE1-ACE8)
#
#         Research question: Does experiencing a specific ACE type moderate
#         the relationship between number of maltreatment types and alcohol
#         outcomes?
#
#         Model structure:
#           Outcome ~ NumMaltreatmentTypes + ACE1...ACE8 +
#                     NumMalt_ACE1...NumMalt_ACE8 +  (interaction terms)
#                     Sex_Female + Sex_Diverse + mdd + gad + ptsd
#
#         ACE variables (ACE1-ACE8) are coded 0/1.
#         Interaction terms were created in 01_data_preparation.R as
#         NumMalt_ACE1 through NumMalt_ACE8.
# =============================================================================

cat("=== Part C: Moderation by individual ACE types ===\n\n")

# Define SEM model syntax (shared across outcomes)
ace_model_syntax <- function(outcome) {
  paste0(
    outcome, ' ~ b1*NumMaltreatmentTypes +
      c1*Sex_Female + c2*Sex_Diverse +
      c3*mdd + c4*gad + c5*ptsd +
      m1*ACE1 + m2*ACE2 + m3*ACE3 + m4*ACE4 +
      m5*ACE5 + m6*ACE6 + m7*ACE7 + m8*ACE8 +
      i1*NumMalt_ACE1 + i2*NumMalt_ACE2 + i3*NumMalt_ACE3 + i4*NumMalt_ACE4 +
      i5*NumMalt_ACE5 + i6*NumMalt_ACE6 + i7*NumMalt_ACE7 + i8*NumMalt_ACE8'
  )
}

# C1. HB7 (ordinal, WLSMV)
cat("C1. HB7\n")
fit_ace_hb7 <- sem(ace_model_syntax("HB7"),
                   data      = data_clean,
                   ordered   = "HB7",
                   estimator = "WLSMV")

summary_ace_hb7 <- summary(fit_ace_hb7, fit.measures = TRUE, standardized = TRUE)
print(summary_ace_hb7)

results_ace_hb7 <- parameterEstimates(fit_ace_hb7)
write.xlsx(results_ace_hb7, file = "hb7_ACE_moderation_results.xlsx")
cat("Saved: hb7_ACE_moderation_results.xlsx\n\n")


# C2. AlcoholBinge (binary, ML)
cat("C2. AlcoholBinge\n")
fit_ace_binge <- sem(ace_model_syntax("AlcoholBinge"),
                     data      = data_clean,
                     estimator = "ML")

summary_ace_binge <- summary(fit_ace_binge, fit.measures = TRUE, standardized = TRUE)
print(summary_ace_binge)

results_ace_binge <- parameterEstimates(fit_ace_binge)
write.xlsx(results_ace_binge, file = "binge_ACE_moderation_results.xlsx")
cat("Saved: binge_ACE_moderation_results.xlsx\n\n")


# =============================================================================
# PART D: Moderation by multitype ACE score (0-8 composite)
#
#         The multitype ACE score is the sum of all 8 ACE binary items (0-8).
#         This tests whether the total burden of adverse experiences moderates
#         the relationship between maltreatment count and alcohol outcomes.
# =============================================================================

cat("=== Part D: Moderation by multitype ACE score ===\n\n")

# Create composite ACE score (0-8)
data_clean$Multitype_ACE <- rowSums(
  data_clean[, paste0("ACE", 1:8)],
  na.rm = TRUE
)

cat("Distribution of Multitype ACE score (0-8):\n")
ace_table <- data.frame(
  Number_of_ACEs       = as.numeric(names(table(data_clean$Multitype_ACE))),
  Frequency            = as.numeric(table(data_clean$Multitype_ACE))
)
ace_table$Percentage            <- round((ace_table$Frequency / sum(ace_table$Frequency)) * 100, 1)
ace_table$Cumulative_Percentage <- round(cumsum(ace_table$Percentage), 1)
print(ace_table)

hist(data_clean$Multitype_ACE,
     main   = "Distribution of Multitype ACE Score",
     xlab   = "Number of ACEs Experienced (0-8)",
     ylab   = "Frequency",
     breaks = seq(-0.5, 8.5, by = 1))


# D1. AUD severity (AUD_ordinal) - ordinal, WLSMV
cat("\nD1. AUD_ordinal\n")
model_multiace_aud <- '
  AUD_ordinal ~ b1*NumMaltreatmentTypes + m1*Multitype_ACE +
                c1*Sex_Female + c2*Sex_Diverse +
                c3*mdd + c4*gad + c5*ptsd +
                i1*NumMaltreatmentTypes:Multitype_ACE
'

fit_multiace_aud <- sem(model_multiace_aud,
                        data      = data_clean,
                        ordered   = "AUD_ordinal",
                        estimator = "WLSMV")

print(summary(fit_multiace_aud, fit.measures = TRUE, standardized = TRUE))


# D2. HB7 (ordinal, WLSMV)
cat("\nD2. HB7\n")
model_multiace_hb7 <- '
  HB7 ~ b1*NumMaltreatmentTypes + m1*Multitype_ACE +
        c1*Sex_Female + c2*Sex_Diverse +
        c3*mdd + c4*gad + c5*ptsd +
        i1*NumMaltreatmentTypes:Multitype_ACE
'

fit_multiace_hb7 <- sem(model_multiace_hb7,
                        data      = data_clean,
                        ordered   = "HB7",
                        estimator = "WLSMV")

print(summary(fit_multiace_hb7, fit.measures = TRUE, standardized = TRUE))


# D3. AlcoholBinge (binary, ML)
cat("\nD3. AlcoholBinge\n")
model_multiace_binge <- '
  AlcoholBinge ~ b1*NumMaltreatmentTypes + m1*Multitype_ACE +
                 c1*Sex_Female + c2*Sex_Diverse +
                 c3*mdd + c4*gad + c5*ptsd +
                 i1*NumMaltreatmentTypes:Multitype_ACE
'

fit_multiace_binge <- sem(model_multiace_binge,
                          data      = data_clean,
                          estimator = "ML")

print(summary(fit_multiace_binge, fit.measures = TRUE, standardized = TRUE))

cat("\nAll SEM models complete.\n")
