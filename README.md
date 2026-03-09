# Home Credit Default Risk - Capstone Project

## Project Overview

This project predicts loan default risk using data from the [Home Credit Default Risk](https://www.kaggle.com/competitions/home-credit-default-risk) Kaggle competition. The goal is to help Home Credit expand financial inclusion by providing loans to underserved populations while managing default risk.

The final model is a **Logistic Regression with Lasso (L1) regularization**, achieving a ROC AUC of **0.737** on the hold-out test set and **0.724** on the Kaggle public leaderboard. At the recommended decision threshold of 0.40, the model is estimated to generate approximately **$6.3M in incremental annual value** compared to a no-model baseline, by correctly identifying 68% of defaults while approving 67% of applicants.

## Repository Structure

```
├── home_credit_preprocessing.R   # Data preparation and feature engineering
├── notebooks/
│   ├── modeling.qmd              # Model training, tuning, and evaluation
│   └── model_card.qmd            # Model card documentation
├── models/                       # Saved model artifacts (gitignored)
├── data/                         # Raw and processed data files (gitignored)
├── README.md                     # This file
└── .gitignore                    # Excludes data files and model artifacts
```

## Model Card

The model card (`notebooks/model_card.qmd`) provides comprehensive documentation of the Home Credit Default Risk Classifier, including:

- **Model Details:** Logistic Regression with Lasso regularization (penalty = 0.00001), trained on 30,000 applications with 17 core features
- **Intended Use:** Credit decisioning for consumer loan origination
- **Performance Metrics:** ROC AUC 0.737 (test), 0.724 (Kaggle); precision 0.22, recall 0.68 at threshold 0.40
- **Decision Threshold Analysis:** Cost-benefit analysis using industry-standard LGD (45%) and recovery rates (30%), with profit-curve visualization and sensitivity table
- **Explainability:** SHAP-based feature importance on a 1,000-row sample; top drivers are `EXT_SOURCE_2/3/1`, `DAYS_BIRTH`, and `DAYS_EMPLOYED`
- **Adverse Action Mapping:** Regulatory-compliant plain-language reason codes for loan denials (ECOA/FCRA)
- **Fairness Analysis:** Disparate impact analysis by gender (DI ratio 0.90) and education level; no evidence of unjustified disparate treatment
- **Limitations and Risks:** Known constraints, data limitations, and production monitoring requirements
- **Executive Summary:** Business recommendation with expected financial impact and key caveats

To render the model card:

```r
quarto::quarto_render("notebooks/model_card.qmd")
```

## Data Preparation Script

### `home_credit_preprocessing.R`

A comprehensive R script containing reusable functions for cleaning, transforming, and feature engineering on the Home Credit dataset.

#### What It Does

1. **Cleans application data** based on EDA findings:
   - Fixes the `DAYS_EMPLOYED = 365243` anomaly (placeholder for unemployed/pensioners)
   - Imputes missing values in `EXT_SOURCE` variables using median
   - Converts "XNA" strings to NA
   - Standardizes column names to snake_case

2. **Creates engineered features**:
   - **Demographic features**: Age in years, employment duration, age groups
   - **Financial ratios**: Credit-to-income, annuity-to-income (DTI), loan-to-value proxy, debt service ratio, affordability metrics
   - **Missing data indicators**: Binary flags for predictive missingness patterns
   - **Interaction terms**: Binned variables, categorical interactions, polynomial features

3. **Aggregates supplementary data** to applicant level (`SK_ID_CURR`):
   - `bureau.csv`: Credit counts, active/closed ratios, overdue amounts, utilization
   - `previous_application.csv`: Application counts, approval/refusal rates
   - `installments_payments.csv`: Late payment rates, payment trends

4. **Ensures train/test consistency**:
   - Computes medians and bin thresholds from training data only
   - Stores parameters for reuse on test data
   - Aligns columns between train and test sets

#### How To Use

```r
# Load the preprocessing functions
source("home_credit_preprocessing.R")

# Run the complete pipeline
result <- run_preprocessing_pipeline(
  train_path = "application_train.csv",
  test_path = "application_test.csv",
  bureau_path = "bureau.csv",
  prev_app_path = "previous_application.csv",
  installments_path = "installments_payments.csv"
)

# Access processed data
train_processed <- result$train
test_processed <- result$test
params <- result$params  # Stored for reproducibility
```

#### Key Functions

| Function | Purpose |
|----------|---------|
| `clean_application_data()` | Fix anomalies, impute missing values, standardize data |
| `create_demographic_features()` | Transform age/employment from days to years, create age groups |
| `create_financial_ratios()` | Compute credit risk ratios (DTI, LTV, affordability) |
| `create_missing_indicators()` | Add binary flags for missing value patterns |
| `create_interaction_features()` | Create binned variables, interactions, polynomials |
| `aggregate_bureau()` | Summarize Credit Bureau history per applicant |
| `aggregate_previous_application()` | Summarize prior Home Credit applications |
| `aggregate_installments()` | Summarize payment behavior |
| `run_preprocessing_pipeline()` | Execute the complete pipeline |

#### Dependencies

```r
install.packages(c("tidyverse", "janitor"))
```

## Data Files

Data files are **not included** in this repository. Download from:
- [Kaggle Competition Data](https://www.kaggle.com/competitions/home-credit-default-risk/data)

Required files:
- `application_train.csv`
- `application_test.csv`
- `bureau.csv`
- `previous_application.csv`
- `installments_payments.csv`

## Author

Kate Klinger
