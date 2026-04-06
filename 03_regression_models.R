# =============================================================================
# Script 03: Regression Models
# Project:   Childhood Maltreatment and Alcohol-Related Outcomes
# Dataset:   Australian Child Maltreatment Study (ACMS)
# Author:    Dhatsayini Rattambige
# Date:      09.04.2026
#
# Description:
#   This script runs all regression analyses examining associations between
#   childhood maltreatment and alcohol-related outcomes.
#
#   It is organised into three parts:
#
#   Part A: Individual maltreatment types as predictors
#           - Logistic regression: binge drinking (AlcoholBinge)
#           - Ordinal regression: AUD severity (AUD_ordinal)
#           - Ordinal regression: alcohol use frequency (HB7)
#
#   Part B: Number of maltreatment types as a continuous predictor
#           - Logistic regression: AlcoholBinge
#           - Ordinal regression: AUD_ordinal
#           - Ordinal regression: HB7
#
#   Part C: Maltreatment type combinations as predictors
#           - Combinations with n > 30 are retained for analysis
#           - Results saved to CSV
#
# Prerequisite:
#   Run 01_data_preparation.R before this script.
#   This script requires `data_clean` to be loaded in your environment.
#
# Script order:
#   01_data_preparation.R
#   02_descriptives.R
#   03_regression_models.R  <- YOU ARE HERE
#   04_sem_moderation.R
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load required packages
# -----------------------------------------------------------------------------

packages <- c("dplyr", "MASS", "writexl", "car", "broom")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# -----------------------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------------------

# Adds significance stars to a p-value
add_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}

# Formats p-values and appends significance stars
format_p_value <- function(p) {
  formatted <- sprintf("%.3f", p)
  paste0(formatted, add_stars(p))
}


# =============================================================================
# PART A: Individual maltreatment types as predictors
#         Each maltreatment type is tested in a separate model (unadjusted).
# =============================================================================

cm_vars <- c("E_EA", "E_PA", "E_SAb", "E_NEG", "E_EDV")

# -----------------------------------------------------------------------------
# A1. Logistic regression: Binge drinking (AlcoholBinge)
#     AlcoholBinge is binary (0/1), so we use logistic regression.
#     Results are expressed as odds ratios (OR) with 95% confidence intervals.
# -----------------------------------------------------------------------------

cat("=== Part A1: Individual CM types -> Binge Drinking (Logistic) ===\n\n")

results_list <- list()

for (cm in cm_vars) {

  formula <- as.formula(paste("AlcoholBinge ~", cm))
  model   <- glm(formula, family = binomial(link = "logit"), data = data_clean)

  ci     <- confint(model)
  or_est <- exp(coef(model)[2])    # OR for the CM predictor (not the intercept)
  ci_exp <- exp(ci[2, ])           # 95% CI on the OR scale
  p_val  <- coef(summary(model))[2, 4]

  results_list[[cm]] <- data.frame(
    CM_Type      = cm,
    OR           = round(or_est, 3),
    CI_Lower     = round(ci_exp[1], 3),
    CI_Upper     = round(ci_exp[2], 3),
    P_Value      = format(p_val, scientific = TRUE, digits = 3),
    Significance = add_stars(p_val)
  )
}

binge_individual <- do.call(rbind, results_list)
print(binge_individual)
write_xlsx(binge_individual, "individual_logistic_regression_binge_results.xlsx")
cat("Saved: individual_logistic_regression_binge_results.xlsx\n\n")


# -----------------------------------------------------------------------------
# A2. Ordinal regression: AUD severity (AUD_ordinal)
#     AUD_ordinal has four ordered levels: None < Mild < Moderate < Severe.
#     We use proportional odds logistic regression (polr from MASS).
#     p-values are computed from the t-statistic using the normal approximation.
# -----------------------------------------------------------------------------

cat("=== Part A2: Individual CM types -> AUD Severity (Ordinal) ===\n\n")

results_list <- list()

for (cm in cm_vars) {

  formula <- as.formula(paste("AUD_ordinal ~", cm))
  model   <- polr(formula, data = data_clean, Hess = TRUE)

  ctable  <- coef(summary(model))
  p_val   <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

  ci      <- confint(model)
  or_est  <- exp(coef(model))
  ci_exp  <- exp(ci)

  results_list[[cm]] <- data.frame(
    CM_Type      = cm,
    OR           = round(or_est[1], 3),
    CI_Lower     = round(ci_exp[1], 3),
    CI_Upper     = round(ci_exp[2], 3),
    P_Value      = format(p_val[1], scientific = TRUE, digits = 3),
    Significance = add_stars(p_val[1])
  )

  cat(cm, "| OR:", round(or_est[1], 3),
      "| 95% CI:", round(ci_exp[1], 3), "to", round(ci_exp[2], 3),
      "| p:", format(p_val[1], scientific = TRUE, digits = 3), "\n")
}

aud_individual <- do.call(rbind, results_list)
print(aud_individual)
write_xlsx(aud_individual, "individual_ordinal_regression_AUD_results.xlsx")
cat("Saved: individual_ordinal_regression_AUD_results.xlsx\n\n")


# -----------------------------------------------------------------------------
# A3. Ordinal regression: Alcohol use frequency (HB7)
#     HB7 has five ordered levels: Never < Less than monthly < 1-3 times/month
#     < 1-4 times/week < Daily
# -----------------------------------------------------------------------------

cat("=== Part A3: Individual CM types -> Alcohol Frequency (HB7, Ordinal) ===\n\n")

results_list <- list()

for (cm in cm_vars) {

  formula <- as.formula(paste("HB7 ~", cm))
  model   <- polr(formula, data = data_clean, Hess = TRUE)

  ctable  <- coef(summary(model))
  p_val   <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

  ci      <- confint(model)
  or_est  <- exp(coef(model))
  ci_exp  <- exp(ci)

  results_list[[cm]] <- data.frame(
    CM_Type      = cm,
    OR           = round(or_est[1], 3),
    CI_Lower     = round(ci_exp[1], 3),
    CI_Upper     = round(ci_exp[2], 3),
    P_Value      = format(p_val[1], scientific = TRUE, digits = 3),
    Significance = add_stars(p_val[1])
  )
}

hb7_individual <- do.call(rbind, results_list)
print(hb7_individual)
write_xlsx(hb7_individual, "individual_ordinal_regression_HB7_results.xlsx")
cat("Saved: individual_ordinal_regression_HB7_results.xlsx\n\n")


# =============================================================================
# PART B: Number of maltreatment types as a continuous predictor
#         Tests whether experiencing more types of maltreatment is associated
#         with worse alcohol outcomes, regardless of which types.
# =============================================================================

cat("=== Part B: Number of maltreatment types (continuous) ===\n\n")

# B1. Logistic regression: Binge drinking
binge_model <- glm(AlcoholBinge ~ NumMaltreatmentTypes,
                   family = binomial(link = "logit"),
                   data   = data_clean)

binge_or <- exp(coef(binge_model)[2])
binge_ci <- exp(confint(binge_model)[2, ])
binge_p  <- summary(binge_model)$coefficients[2, 4]

binge_cont_results <- data.frame(
  Outcome      = "AlcoholBinge",
  OR           = round(binge_or, 3),
  CI_Lower     = round(binge_ci[1], 3),
  CI_Upper     = round(binge_ci[2], 3),
  P_Value      = format(binge_p, scientific = TRUE, digits = 3),
  Significance = add_stars(binge_p)
)
print(binge_cont_results)
write_xlsx(binge_cont_results, "binge_maltreatment_continuous_results.xlsx")

# Save full model summary as text
sink("binge_full_model_summary_continuous.txt")
print(summary(binge_model))
sink()
cat("Saved: binge_maltreatment_continuous_results.xlsx\n\n")


# B2. Ordinal regression: AUD severity
aud_model <- polr(AUD_ordinal ~ NumMaltreatmentTypes,
                  data = data_clean,
                  Hess = TRUE)

ctable_aud <- coef(summary(aud_model))
aud_p      <- pnorm(abs(ctable_aud[, "t value"]), lower.tail = FALSE) * 2
aud_or     <- exp(coef(aud_model)[1])
aud_ci     <- exp(confint(aud_model))

aud_cont_results <- data.frame(
  Outcome      = "AUD_ordinal",
  OR           = round(aud_or, 3),
  CI_Lower     = round(aud_ci[1], 3),
  CI_Upper     = round(aud_ci[2], 3),
  P_Value      = sprintf("%.3f", aud_p[1]),
  Significance = add_stars(aud_p[1])
)
print(aud_cont_results)
write_xlsx(aud_cont_results, "aud_maltreatment_continuous_results.xlsx")

sink("aud_full_model_summary_continuous.txt")
print(summary(aud_model))
sink()
cat("Saved: aud_maltreatment_continuous_results.xlsx\n\n")


# B3. Ordinal regression: Alcohol use frequency (HB7)
hb7_model <- polr(HB7 ~ NumMaltreatmentTypes,
                  data = data_clean,
                  Hess = TRUE)

ctable_hb7 <- coef(summary(hb7_model))
hb7_p      <- pnorm(abs(ctable_hb7[, "t value"]), lower.tail = FALSE) * 2
hb7_or     <- exp(coef(hb7_model))
hb7_ci     <- exp(confint(hb7_model))

hb7_cont_results <- data.frame(
  Outcome      = "HB7",
  OR           = round(hb7_or[1], 3),
  CI_Lower     = round(hb7_ci[1], 3),
  CI_Upper     = round(hb7_ci[2], 3),
  P_Value      = sprintf("%.3f", hb7_p[1]),
  Significance = add_stars(hb7_p[1])
)
print(hb7_cont_results)
write_xlsx(hb7_cont_results, "hb7_maltreatment_continuous_results.xlsx")
cat("Saved: hb7_maltreatment_continuous_results.xlsx\n\n")


# =============================================================================
# PART C: Maltreatment type combinations as predictors
#         Tests each specific combination of maltreatment types against each
#         alcohol outcome. Only combinations with n > 30 are included, to
#         ensure reliable estimates.
#
#         Note: CM combination variables were created in 01_data_preparation.R
# =============================================================================

cat("=== Part C: Maltreatment type combinations ===\n\n")

# Get all combination variable names (all start with "CM_")
cm_combinations <- names(data_clean)[grep("^CM_", names(data_clean))]

# Count how many participants fall into each combination
counts <- sapply(cm_combinations, function(comb) {
  sum(data_clean[[comb]], na.rm = TRUE)
})

results_df <- data.frame(CM_Type = cm_combinations, Count = counts)
results_df <- results_df[order(-results_df$Count), ]

cat("Combination counts (all):\n")
print(results_df, row.names = FALSE)
write.csv(results_df, "maltreatment_combination_counts.csv", row.names = FALSE)

# Keep only combinations with n > 30
sufficient_combinations <- results_df %>%
  filter(Count > 30) %>%
  pull(CM_Type)

cat("\nCombinations with n > 30 (used in regression):\n")
print(sufficient_combinations)


# Initialise result storage
binge_comb_df <- data.frame(CM_Type = character(), OR = numeric(),
                             CI_Lower = numeric(), CI_Upper = numeric(),
                             P_Value = numeric(), stringsAsFactors = FALSE)
aud_comb_df   <- binge_comb_df
hb7_comb_df   <- binge_comb_df

for (comb in sufficient_combinations) {

  # C1. Logistic regression: Binge drinking
  formula_binge <- as.formula(paste("AlcoholBinge ~", comb))
  model_binge   <- glm(formula_binge, family = binomial(link = "logit"),
                       data = data_clean)
  ci_binge    <- confint(model_binge)
  binge_comb_df <- rbind(binge_comb_df, data.frame(
    CM_Type  = comb,
    OR       = round(exp(coef(model_binge)[2]), 3),
    CI_Lower = round(exp(ci_binge[2, 1]), 3),
    CI_Upper = round(exp(ci_binge[2, 2]), 3),
    P_Value  = coef(summary(model_binge))[2, 4]
  ))

  # C2. Ordinal regression: AUD severity
  formula_aud <- as.formula(paste("AUD_ordinal ~", comb))
  model_aud   <- polr(formula_aud, data = data_clean, Hess = TRUE)
  ctable_aud  <- coef(summary(model_aud))
  p_aud       <- pnorm(abs(ctable_aud[, "t value"]), lower.tail = FALSE) * 2
  aud_comb_df <- rbind(aud_comb_df, data.frame(
    CM_Type  = comb,
    OR       = round(exp(coef(model_aud)[1]), 3),
    CI_Lower = round(exp(confint(model_aud))[1, 1], 3),
    CI_Upper = round(exp(confint(model_aud))[1, 2], 3),
    P_Value  = p_aud[1]
  ))

  # C3. Ordinal regression: Alcohol frequency (HB7)
  formula_hb7 <- as.formula(paste("HB7 ~", comb))
  model_hb7   <- polr(formula_hb7, data = data_clean, Hess = TRUE)
  ctable_hb7  <- coef(summary(model_hb7))
  p_hb7       <- pnorm(abs(ctable_hb7[, "t value"]), lower.tail = FALSE) * 2
  hb7_comb_df <- rbind(hb7_comb_df, data.frame(
    CM_Type  = comb,
    OR       = round(exp(coef(model_hb7)[1]), 3),
    CI_Lower = round(exp(confint(model_hb7))[1, 1], 3),
    CI_Upper = round(exp(confint(model_hb7))[1, 2], 3),
    P_Value  = p_hb7[1]
  ))
}

# Format p-values and add CI column for all three outcome tables
for (df_name in c("binge_comb_df", "aud_comb_df", "hb7_comb_df")) {
  df <- get(df_name)
  df$P_Value <- sapply(df$P_Value, format_p_value)
  df$CI      <- paste0("[", df$CI_Lower, ", ", df$CI_Upper, "]")
  df         <- df %>%
    select(CM_Type, OR, CI, P_Value) %>%
    rename(`Odds Ratio` = OR, `95% CI` = CI, `p-value` = P_Value)
  assign(df_name, df)
}

cat("\nBinge Drinking Results (combinations):\n")
print(binge_comb_df)

cat("\nAUD Severity Results (combinations):\n")
print(aud_comb_df)

cat("\nAlcohol Frequency HB7 Results (combinations):\n")
print(hb7_comb_df)

write.csv(binge_comb_df, "combination_binge_results.csv",  row.names = FALSE)
write.csv(aud_comb_df,   "combination_aud_results.csv",    row.names = FALSE)
write.csv(hb7_comb_df,   "combination_hb7_results.csv",    row.names = FALSE)

cat("\nAll combination results saved. Proceed to 04_sem_moderation.R\n")
