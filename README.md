# Childhood Maltreatment and Alcohol-Related Outcomes: Analysis Code

This repository contains the R analysis code for the PhD thesis:

> **Childhood Adversities and Youth Alcohol Use: Developmental Trajectories Across Neurobiological and Social Contexts**
> Dhatsayini Rattambige | Australian Catholic University, Institute of Child Protection Studies
> Submitted April, 2026]

---

## Overview

This code supports the analysis examining associations between childhood maltreatment experiences and alcohol-related outcomes in Australian youth, using data from the **Australian Child Maltreatment Study (ACMS)**.

The analyses include:

- Descriptive statistics and sample characterisation
- Logistic regression (binge drinking)
- Ordinal regression (AUD severity and alcohol use frequency)
- Structural equation modelling (SEM) with moderation by age of onset and ACE type

---

## Repository structure

| Script | Description |
|--------|-------------|
| `01_data_preparation.R` | Missing value recoding, variable creation, data transformations |
| `02_descriptives.R` | Table 1, sample distributions, demographic and clinical summaries |
| `03_regression_models.R` | Logistic and ordinal regression models (individual CM types, continuous count, combinations) |
| `04_sem_moderation.R` | SEM moderation models (age of onset, individual ACEs, multitype ACE score) |

Scripts should be run **in order** (01 through 04). Each script assumes that `data_clean` has been loaded and prepared by Script 01.

---

## Data access

The ACMS data are not included in this repository due to participant confidentiality agreements.

Researchers wishing to replicate or extend these analyses can apply for data access through the ACMS:
- Website: [https://www.acms.au/](https://www.acms.au/)

---

## Software and packages

All analyses were conducted in **R**. The following packages are required:

| Package | Purpose |
|---------|---------|
| `dplyr` | Data manipulation |
| `haven` | Importing SPSS (.sav) files |
| `MASS` | Ordinal regression (`polr`) |
| `lavaan` | Structural equation modelling |
| `tableone` | Descriptive statistics (Table 1) |
| `knitr`, `kableExtra` | Table formatting and HTML export |
| `writexl`, `openxlsx` | Saving results to Excel |
| `ggplot2` | Visualisation |
| `car` | Variance inflation factor (VIF) checks |

All packages can be installed from CRAN using `install.packages()`. Each script checks for and installs missing packages automatically.

---

## Variable reference

| Variable | Description |
|----------|-------------|
| `ELIG1` | Participant age |
| `Sex` | Sex (1 = Male, 2 = Female, 9 = Gender diverse) |
| `ATSI` | Aboriginal and/or Torres Strait Islander status |
| `MetRur` | Metropolitan vs rural/remote location |
| `E_EA` | Emotional abuse (binary) |
| `E_PA` | Physical abuse (binary) |
| `E_SAb` | Sexual abuse (binary) |
| `E_NEG` | Neglect (binary) |
| `E_EDV` | Exposure to domestic violence (binary) |
| `NumMaltreatmentTypes` | Count of maltreatment types (0-5) |
| `AlcoholBinge` | Binge drinking (binary) |
| `HB7` | Alcohol use frequency (ordered: 1=Never to 5=Daily) |
| `aud` | Any alcohol use disorder (binary) |
| `aud_mild` / `aud_mod` / `aud_sev` | AUD severity subcategories (binary) |
| `AUD_ordinal` | AUD severity (ordered: None/Mild/Moderate/Severe) |
| `ptsd` | Post-traumatic stress disorder (binary) |
| `gad` | Generalised anxiety disorder (binary) |
| `mdd` | Major depressive disorder (binary) |
| `ACE1`-`ACE8` | Adverse childhood experience items (binary) |
| `EarliestOnset` | Earliest age of maltreatment onset (derived) |

---

## Contact

For questions about this code, please contact:

**[Your name]**
Institute of Child Protection Studies, Australian Catholic University
[Your email address]

---

## Citation

If you use or adapt this code, please cite the associated thesis:

> [Your name] ([Year]). *[Thesis title]*. Doctoral thesis, Australian Catholic University.
