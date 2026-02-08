# ==============================================================================
# Home Credit Default Risk - Data Cleaning and Feature Engineering Pipeline
# ==============================================================================
# 
# This script provides reusable functions for preprocessing the Home Credit 
# Default Risk dataset. Functions are designed to:
#   - Clean and transform application data based on EDA findings
#   - Create engineered features (demographic, financial ratios, interactions)
#   - Aggregate supplementary data (bureau, previous_application, installments)
#   - Ensure train/test consistency by fitting parameters on train only
#
# Author: Generated with AI assistance
# Date: 2026-02-04
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(tidyverse)
library(janitor)

# ------------------------------------------------------------------------------
# SECTION 1: DATA CLEANING FUNCTIONS
# ------------------------------------------------------------------------------

#' Fix DAYS_EMPLOYED anomaly (365243 placeholder)
#' 
#' Replaces the placeholder value 365243 in DAYS_EMPLOYED with NA.
#' This value represents unemployed/pensioners and is not a valid day count.
#'
#' @param df A data frame containing days_employed column
#' @return Data frame with anomaly replaced by NA
fix_days_employed_anomaly <- function(df) {
  # The value 365243 (~1000 years) is a placeholder for unemployed/pensioners

  # Identified in EDA: ~55,374 records have this anomalous value
  df |>
    mutate(
      # Create flag before replacing (missingness may be predictive)
      days_employed_anomaly = if_else(days_employed == 365243, 1L, 0L),
      # Replace placeholder with NA
      days_employed = if_else(days_employed == 365243, NA_real_, days_employed)
    )
}

#' Impute EXT_SOURCE variables
#'
#' Imputes missing values in ext_source_1, ext_source_2, ext_source_3 using
#' median values computed from training data.
#'
#' @param df A data frame containing ext_source columns
#' @param medians Named vector of medians (NULL to compute from df)
#' @return List with imputed data frame and medians used
impute_ext_source <- function(df, medians = NULL) {
  # EXT_SOURCE variables are highly predictive but have significant missingness:
  #   ext_source_1: ~56% missing
  #   ext_source_2: ~0.2% missing  
  #   ext_source_3: ~20% missing
  
  ext_source_cols <- c("ext_source_1", "ext_source_2", "ext_source_3")
  
  # Compute medians from data if not provided (training mode)
  if (is.null(medians)) {
    medians <- df |>
      summarise(across(all_of(ext_source_cols), \(x) median(x, na.rm = TRUE))) |>
      unlist()
  }
  
  # Apply median imputation

  df_imputed <- df |>
    mutate(
      ext_source_1 = coalesce(ext_source_1, medians["ext_source_1"]),
      ext_source_2 = coalesce(ext_source_2, medians["ext_source_2"]),
      ext_source_3 = coalesce(ext_source_3, medians["ext_source_3"])
    )
  
  list(
    data = df_imputed,
    medians = medians
  )
}

#' Clean application data
#'
#' Master function that applies all cleaning transformations:
#'   - Fixes DAYS_EMPLOYED anomaly
#'   - Handles other placeholder values
#'   - Standardizes column names
#'
#' @param df Raw application data frame
#' @return Cleaned data frame
clean_application_data <- function(df) {
  df |>
    # Standardize column names to snake_case
    clean_names() |>
    # Fix the DAYS_EMPLOYED placeholder anomaly
    fix_days_employed_anomaly() |>
    # Handle other potential anomalies identified in EDA
    mutate(
      # Convert XNA strings to NA in categorical variables
      across(where(is.character), \(x) na_if(x, "XNA")),
      # ORGANIZATION_TYPE has "XNA" values - convert to NA
      organization_type = na_if(organization_type, "XNA"),
      # Ensure consistent factor levels for binary flags
      across(starts_with("flag_"), as.integer)
    )
}

# ------------------------------------------------------------------------------
# SECTION 2: FEATURE ENGINEERING FUNCTIONS
# ------------------------------------------------------------------------------

#' Create demographic features
#'
#' Transforms negative day values to positive years/months and creates
#' age-related features.
#'
#' @param df Data frame with days_birth, days_employed, days_registration, etc.
#' @return Data frame with new demographic features
create_demographic_features <- function(df) {
  df |>
    mutate(
      # Convert negative days to positive years
      # DAYS_BIRTH is negative (days before application)
      age_years = abs(days_birth) / 365.25,
      
      # Employment duration in years (already fixed anomaly, now convert)
      employment_years = abs(days_employed) / 365.25,
      
      # Registration duration (how long client has been registered)
      registration_years = abs(days_registration) / 365.25,
      
      # ID change duration (how long since last ID document change)
      id_publish_years = abs(days_id_publish) / 365.25,
      
      # Last phone change duration
      phone_change_years = abs(days_last_phone_change) / 365.25,
      
      # Age-related ratios and interactions
      # Employment ratio: what fraction of adult life employed
      employment_to_age_ratio = employment_years / pmax(age_years - 18, 1),
      
      # Age bins (useful for non-linear relationships)
      age_group = cut(
        age_years,
        breaks = c(0, 25, 35, 45, 55, 65, Inf),
        labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"),
        include.lowest = TRUE
      ),
      
      # Working age indicator
      is_working_age = if_else(age_years >= 18 & age_years <= 65, 1L, 0L),
      
      # Young adult (potentially riskier)
      is_young_adult = if_else(age_years < 30, 1L, 0L)
    )
}

#' Create financial ratios
#'
#' Computes common credit risk ratios including:
#'   - Credit-to-income ratio
#'   - Annuity-to-income ratio (payment burden)
#'   - Loan-to-value proxy
#'   - Debt service ratios
#'
#' @param df Data frame with amt_credit, amt_income_total, amt_annuity, etc.
#' @return Data frame with new ratio features
create_financial_ratios <- function(df) {
  df |>
    mutate(
      # ---- Core Credit Ratios ----
      
      # Credit-to-Income Ratio: Total loan amount relative to income
      # Higher values indicate higher leverage
      credit_to_income = amt_credit / pmax(amt_income_total, 1),
      
      # Annuity-to-Income Ratio (Payment Burden / DTI proxy)
      # What fraction of income goes to loan payments
      annuity_to_income = amt_annuity / pmax(amt_income_total, 1),
      
      # Loan-to-Goods Ratio (LTV proxy)
      # How much credit relative to goods price - >1 means financing more than goods
      credit_to_goods = amt_credit / pmax(amt_goods_price, 1),
      
      # ---- Payment and Term Ratios ----
      
      # Credit Term (in months, derived from credit/annuity)
      credit_term_months = amt_credit / pmax(amt_annuity, 1),
      
      # Annual payment as multiple of credit
      annuity_to_credit = amt_annuity / pmax(amt_credit, 1),
      
      # ---- Income and Expense Ratios ----
      
      # Income per family member
      income_per_person = amt_income_total / pmax(cnt_fam_members, 1),
      
      # Credit per family member
      credit_per_person = amt_credit / pmax(cnt_fam_members, 1),
      
      # Income per child (for families with children)
      income_per_child = if_else(
        cnt_children > 0,
        amt_income_total / cnt_children,
        amt_income_total  # No children: full income
      ),
      
      # ---- Employment-Adjusted Ratios ----
      
      # Income stability: income relative to employment years
      income_per_employment_year = amt_income_total / pmax(employment_years, 0.5),
      
      # Credit per year of employment (risk if high credit, short employment)
      credit_per_employment_year = amt_credit / pmax(employment_years, 0.5),
      
      # ---- Goods and Down Payment Proxies ----
      
      # Down payment proxy (difference between goods price and credit)
      down_payment_proxy = pmax(amt_goods_price - amt_credit, 0),
      
      # Down payment ratio
      down_payment_ratio = down_payment_proxy / pmax(amt_goods_price, 1),
      
      # ---- Additional Credit Risk Ratios ----
      
      # Debt-to-Assets Proxy: Credit relative to goods (collateral proxy)
      # Higher values = more leveraged against assets
      debt_to_asset_proxy = amt_credit / pmax(amt_goods_price, 1),
      
      # Annual Debt Service Ratio: Annual payments / Annual income
      # Industry standard is typically < 0.36 for healthy finances
      annual_debt_service_ratio = (amt_annuity * 12) / pmax(amt_income_total, 1),
      
      # Credit Capacity: How many months of income equals the credit
      credit_months_of_income = amt_credit / pmax(amt_income_total / 12, 1),
      
      # Affordability Index: Income minus annuity (disposable after payment)
      affordability_monthly = (amt_income_total / 12) - amt_annuity,
      affordability_ratio = affordability_monthly / pmax(amt_income_total / 12, 1),
      
      # Payment Intensity: Annuity relative to goods price
      # How aggressive is the payment schedule relative to item value
      payment_intensity = amt_annuity / pmax(amt_goods_price, 1),
      
      # ---- Age-Adjusted Financial Ratios ----
      
      # Income per year of age (earning power trajectory)
      income_per_age_year = amt_income_total / pmax(age_years, 18),
      
      # Loan burden relative to remaining working years (assuming retirement at 65)
      remaining_work_years = pmax(65 - age_years, 1),
      credit_per_remaining_year = amt_credit / remaining_work_years,
      
      # ---- External Score Combinations ----
      # EXT_SOURCE variables are highly predictive; create combinations
      
      # Mean of external scores (robust to individual missingness after imputation)
      ext_source_mean = (ext_source_1 + ext_source_2 + ext_source_3) / 3,
      
      # Weighted mean (EXT_SOURCE_2 and _3 more predictive based on EDA)
      ext_source_weighted = 0.2 * ext_source_1 + 0.4 * ext_source_2 + 0.4 * ext_source_3,
      
      # Product (captures interaction effect)
      ext_source_product = ext_source_1 * ext_source_2 * ext_source_3,
      
      # Min/max spread (variability in scores)
      ext_source_min = pmin(ext_source_1, ext_source_2, ext_source_3, na.rm = TRUE),
      ext_source_max = pmax(ext_source_1, ext_source_2, ext_source_3, na.rm = TRUE),
      ext_source_range = ext_source_max - ext_source_min,
      
      # ---- Risk Score Interactions with Financial Variables ----
      
      # External score adjusted for credit burden
      ext_score_credit_adjusted = ext_source_mean / pmax(credit_to_income, 0.01),
      
      # External score adjusted for payment burden
      ext_score_annuity_adjusted = ext_source_mean / pmax(annuity_to_income, 0.01)
    )
}

#' Create missing data indicators
#'
#' Adds binary flags for key variables that have missing values,
#' as missingness patterns are often predictive.
#'
#' @param df Data frame
#' @param vars Character vector of variable names to create indicators for
#' @return Data frame with missing indicator columns
create_missing_indicators <- function(df, vars = NULL) {
  # Default variables to track (based on EDA findings)
  if (is.null(vars)) {
    vars <- c(
      # High-value variables with missingness
      "ext_source_1",
      "ext_source_2", 
      "ext_source_3",
      "amt_annuity",
      "amt_goods_price",
      "cnt_fam_members",
      # Occupation and employment
      "occupation_type",
      "days_employed",
      # Housing and contact info
      "own_car_age",
      "obs_30_cnt_social_circle",
      "obs_60_cnt_social_circle",
      "def_30_cnt_social_circle",
      "def_60_cnt_social_circle"
    )
  }
  
  # Only process variables that exist in the data
  vars_present <- intersect(vars, names(df))
  
  # Create missing indicators
  for (var in vars_present) {
    indicator_name <- paste0("missing_", var)
    df <- df |>
      mutate(!!indicator_name := if_else(is.na(.data[[var]]), 1L, 0L))
  }
  
  # Create aggregate missing count
  missing_cols <- names(df)[str_starts(names(df), "missing_")]
  df |>
    mutate(
      total_missing_count = rowSums(across(all_of(missing_cols)), na.rm = TRUE)
    )
}

#' Create interaction and binned features
#'
#' Creates interaction terms and binned versions of continuous variables.
#'
#' @param df Data frame
#' @param bin_params List of binning parameters (NULL to compute from df)
#' @return List with data frame and bin_params used
create_interaction_features <- function(df, bin_params = NULL) {
  # Compute binning quantiles from data if not provided (training mode)
  if (is.null(bin_params)) {
    bin_params <- list(
      income_breaks = quantile(df$amt_income_total, 
                                probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                                na.rm = TRUE),
      credit_breaks = quantile(df$amt_credit, 
                                probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                                na.rm = TRUE),
      age_breaks = c(0, 25, 35, 45, 55, 65, Inf)
    )
  }
  
  df_transformed <- df |>
    mutate(
      # ---- Binned Variables ----
      
      # Income quintiles
      income_bin = cut(
        amt_income_total,
        breaks = bin_params$income_breaks,
        labels = c("Q1_lowest", "Q2", "Q3", "Q4", "Q5_highest"),
        include.lowest = TRUE
      ),
      
      # Credit quintiles
      credit_bin = cut(
        amt_credit,
        breaks = bin_params$credit_breaks,
        labels = c("Q1_lowest", "Q2", "Q3", "Q4", "Q5_highest"),
        include.lowest = TRUE
      ),
      
      # ---- Interaction Terms ----
      
      # Gender x Car ownership (different risk profiles)
      gender_car_interaction = paste0(code_gender, "_", flag_own_car),
      
      # Education x Income interaction
      education_income_interaction = paste0(name_education_type, "_", income_bin),
      
      # Family status x Children interaction
      family_children_interaction = paste0(
        name_family_status, 
        "_children_", 
        if_else(cnt_children > 0, "yes", "no")
      ),
      
      # Housing x Region interaction
      housing_region_interaction = paste0(name_housing_type, "_reg_", region_rating_client),
      
      # ---- Numeric Interactions ----
      
      # Age x Income (older + higher income = more stable)
      age_income_interaction = age_years * log1p(amt_income_total),
      
      # External score x Credit (high score + high credit = different risk)
      ext_score_credit_interaction = ext_source_mean * log1p(amt_credit),
      
      # Employment x Age interaction
      employment_age_interaction = employment_years * age_years,
      
      # ---- Polynomial Features for Key Variables ----
      
      # Squared terms for non-linear relationships
      ext_source_2_squared = ext_source_2^2,
      ext_source_3_squared = ext_source_3^2,
      age_squared = age_years^2,
      
      # ---- Document Flag Aggregates ----
      
      # Count of documents provided (more docs = more engaged applicant)
      document_count = rowSums(across(starts_with("flag_document_")), na.rm = TRUE),
      
      # ---- Contact Information Aggregates ----
      
      # Count of contact methods provided
      contact_count = flag_mobil + flag_emp_phone + flag_work_phone + 
        flag_cont_mobile + flag_phone + flag_email
    )
  
  list(
    data = df_transformed,
    bin_params = bin_params
  )
}

# ------------------------------------------------------------------------------
# SECTION 3: AGGREGATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Aggregate bureau data to applicant level
#'
#' Creates summary features from bureau.csv including:
#'   - Count of prior credits (total, active, closed)
#'   - Overdue amounts and counts
#'   - Debt ratios and credit utilization
#'
#' @param bureau_df Bureau data frame
#' @return Aggregated data frame at sk_id_curr level
aggregate_bureau <- function(bureau_df) {
  bureau_df <- bureau_df |> clean_names()
  
  bureau_df |>
    group_by(sk_id_curr) |>
    summarise(
      # ---- Credit Counts ----
      
      # Total number of previous credits from Credit Bureau
      bureau_credit_count = n(),
      
      # Count by credit status
      bureau_active_count = sum(credit_active == "Active", na.rm = TRUE),
      bureau_closed_count = sum(credit_active == "Closed", na.rm = TRUE),
      bureau_sold_count = sum(credit_active == "Sold", na.rm = TRUE),
      bureau_bad_debt_count = sum(credit_active == "Bad debt", na.rm = TRUE),
      
      # Ratio of active to total credits
      bureau_active_ratio = bureau_active_count / pmax(bureau_credit_count, 1),
      
      # ---- Credit Types ----
      
      # Count of different credit types
      bureau_credit_type_count = n_distinct(credit_type),
      
      # Consumer credit count (most common type)
      bureau_consumer_credit_count = sum(credit_type == "Consumer credit", na.rm = TRUE),
      
      # ---- Amounts ----
      
      # Total and average credit amounts
      bureau_credit_sum = sum(amt_credit_sum, na.rm = TRUE),
      bureau_credit_mean = mean(amt_credit_sum, na.rm = TRUE),
      bureau_credit_max = max(amt_credit_sum, na.rm = TRUE),
      
      # Total and average debt amounts
      bureau_debt_sum = sum(amt_credit_sum_debt, na.rm = TRUE),
      bureau_debt_mean = mean(amt_credit_sum_debt, na.rm = TRUE),
      
      # Credit utilization (debt / credit limit)
      bureau_credit_utilization = sum(amt_credit_sum_debt, na.rm = TRUE) / 
        pmax(sum(amt_credit_sum_limit, na.rm = TRUE), 1),
      
      # ---- Overdue Information ----
      
      # Total overdue amount
      bureau_overdue_sum = sum(amt_credit_sum_overdue, na.rm = TRUE),
      
      # Count of credits with any overdue
      bureau_overdue_count = sum(amt_credit_sum_overdue > 0, na.rm = TRUE),
      
      # Max overdue amount
      bureau_overdue_max = max(amt_credit_sum_overdue, na.rm = TRUE),
      
      # Ratio of credits with overdue
      bureau_overdue_ratio = bureau_overdue_count / pmax(bureau_credit_count, 1),
      
      # ---- Days Past Due ----
      
      # Max days past due (ever)
      bureau_dpd_max = max(credit_day_overdue, na.rm = TRUE),
      
      # Mean days past due
      bureau_dpd_mean = mean(credit_day_overdue, na.rm = TRUE),
      
      # Count with any DPD
      bureau_dpd_count = sum(credit_day_overdue > 0, na.rm = TRUE),
      
      # ---- Timing Features ----
      
      # Days since most recent credit (closer to 0 = more recent)
      bureau_days_credit_min = min(days_credit, na.rm = TRUE),
      bureau_days_credit_max = max(days_credit, na.rm = TRUE),
      bureau_days_credit_mean = mean(days_credit, na.rm = TRUE),
      
      # Days until credit end date
      bureau_days_enddate_min = min(days_credit_enddate, na.rm = TRUE),
      bureau_days_enddate_mean = mean(days_credit_enddate, na.rm = TRUE),
      
      # Credit duration (end - start)
      bureau_credit_duration_mean = mean(days_credit_enddate - days_credit, na.rm = TRUE),
      
      # ---- Prolongation ----
      
      # Count of credit prolongations
      bureau_prolongation_count = sum(cnt_credit_prolong, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    # Handle infinite values from max/min on empty groups
    mutate(across(where(is.numeric), \(x) if_else(is.infinite(x), NA_real_, x)))
}

#' Aggregate previous applications to applicant level
#'
#' Creates summary features from previous_application.csv including:
#'   - Count of prior applications
#'   - Approval/refusal rates
#'   - Application type distributions
#'
#' @param prev_app_df Previous application data frame
#' @return Aggregated data frame at sk_id_curr level
aggregate_previous_application <- function(prev_app_df) {
  prev_app_df <- prev_app_df |> clean_names()
  
  prev_app_df |>
    group_by(sk_id_curr) |>
    summarise(
      # ---- Application Counts ----
      
      # Total previous applications
      prev_app_count = n(),
      
      # Count by status
      prev_app_approved_count = sum(name_contract_status == "Approved", na.rm = TRUE),
      prev_app_refused_count = sum(name_contract_status == "Refused", na.rm = TRUE),
      prev_app_canceled_count = sum(name_contract_status == "Canceled", na.rm = TRUE),
      prev_app_unused_count = sum(name_contract_status == "Unused offer", na.rm = TRUE),
      
      # Approval and refusal rates
      prev_app_approval_rate = prev_app_approved_count / pmax(prev_app_count, 1),
      prev_app_refusal_rate = prev_app_refused_count / pmax(prev_app_count, 1),
      
      # ---- Contract Types ----
      
      # Cash vs Revolving loans
      prev_app_cash_count = sum(name_contract_type == "Cash loans", na.rm = TRUE),
      prev_app_revolving_count = sum(name_contract_type == "Revolving loans", na.rm = TRUE),
      prev_app_consumer_count = sum(name_contract_type == "Consumer loans", na.rm = TRUE),
      
      # ---- Amounts ----
      
      # Applied amounts
      prev_app_amt_application_sum = sum(amt_application, na.rm = TRUE),
      prev_app_amt_application_mean = mean(amt_application, na.rm = TRUE),
      prev_app_amt_application_max = max(amt_application, na.rm = TRUE),
      
      # Credited amounts (what was actually given)
      prev_app_amt_credit_sum = sum(amt_credit, na.rm = TRUE),
      prev_app_amt_credit_mean = mean(amt_credit, na.rm = TRUE),
      
      # Ratio of credit to application (how much of request was granted)
      prev_app_credit_to_application_ratio = sum(amt_credit, na.rm = TRUE) / 
        pmax(sum(amt_application, na.rm = TRUE), 1),
      
      # Down payment information
      prev_app_down_payment_sum = sum(amt_down_payment, na.rm = TRUE),
      prev_app_down_payment_mean = mean(amt_down_payment, na.rm = TRUE),
      
      # ---- Timing ----
      
      # Days since most recent previous application
      prev_app_days_decision_min = min(days_decision, na.rm = TRUE),
      prev_app_days_decision_max = max(days_decision, na.rm = TRUE),
      prev_app_days_decision_mean = mean(days_decision, na.rm = TRUE),
      
      # ---- Payment Terms ----
      
      # Number of installments
      prev_app_cnt_payment_mean = mean(cnt_payment, na.rm = TRUE),
      prev_app_cnt_payment_sum = sum(cnt_payment, na.rm = TRUE),
      
      # ---- Seller and Channel ----
      
      # Number of unique sellers
      prev_app_seller_count = n_distinct(name_seller_industry, na.rm = TRUE),
      
      # Number of unique product types
      prev_app_product_count = n_distinct(name_product_type, na.rm = TRUE),
      
      # ---- Refusal Reasons (for refused applications) ----
      
      # Client-related refusals
      prev_app_refused_client = sum(
        name_contract_status == "Refused" & 
          code_reject_reason %in% c("CLIENT", "HC"), 
        na.rm = TRUE
      ),
      
      # Scoring-related refusals
      prev_app_refused_scofr = sum(
        name_contract_status == "Refused" & 
          code_reject_reason == "SCOFR", 
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), \(x) if_else(is.infinite(x), NA_real_, x)))
}

#' Aggregate installment payments to applicant level
#'
#' Creates payment behavior features from installments_payments.csv including:
#'   - Late payment percentages
#'   - Payment trends (early vs late)
#'   - Average days past due
#'
#' @param installments_df Installments payments data frame
#' @return Aggregated data frame at sk_id_curr level
aggregate_installments <- function(installments_df) {
  installments_df <- installments_df |> clean_names()
  
  # First aggregate to loan level, then to customer level
  installments_df |>
    mutate(
      # Calculate days early/late for each payment
      # Negative = early, Positive = late
      days_late = days_entry_payment - days_instalment,
      
      # Payment difference (underpayment or overpayment)
      payment_diff = amt_instalment - amt_payment,
      
      # Flags
      is_late = days_late > 0,
      is_very_late = days_late > 30,
      is_underpaid = payment_diff > 0
    ) |>
    group_by(sk_id_curr) |>
    summarise(
      # ---- Payment Counts ----
      
      # Total installment payments
      inst_payment_count = n(),
      
      # Number of unique loans
      inst_loan_count = n_distinct(sk_id_prev),
      
      # Average payments per loan
      inst_payments_per_loan = inst_payment_count / pmax(inst_loan_count, 1),
      
      # ---- Late Payment Metrics ----
      
      # Count and rate of late payments
      inst_late_count = sum(is_late, na.rm = TRUE),
      inst_late_rate = inst_late_count / pmax(inst_payment_count, 1),
      
      # Count and rate of very late payments (30+ days)
      inst_very_late_count = sum(is_very_late, na.rm = TRUE),
      inst_very_late_rate = inst_very_late_count / pmax(inst_payment_count, 1),
      
      # ---- Days Late Statistics ----
      
      # Average days late/early
      inst_days_late_mean = mean(days_late, na.rm = TRUE),
      inst_days_late_max = max(days_late, na.rm = TRUE),
      inst_days_late_min = min(days_late, na.rm = TRUE),
      inst_days_late_std = sd(days_late, na.rm = TRUE),
      
      # Total days late (only positive)
      inst_days_late_sum = sum(pmax(days_late, 0), na.rm = TRUE),
      
      # ---- Payment Amount Metrics ----
      
      # Total amounts
      inst_amt_instalment_sum = sum(amt_instalment, na.rm = TRUE),
      inst_amt_payment_sum = sum(amt_payment, na.rm = TRUE),
      
      # Payment ratio (actual / expected)
      inst_payment_ratio = inst_amt_payment_sum / pmax(inst_amt_instalment_sum, 1),
      
      # Underpayment metrics
      inst_underpaid_count = sum(is_underpaid, na.rm = TRUE),
      inst_underpaid_rate = inst_underpaid_count / pmax(inst_payment_count, 1),
      inst_underpaid_amount_sum = sum(pmax(payment_diff, 0), na.rm = TRUE),
      
      # ---- Version/Timing ----
      
      # Max version (indicates payment plan changes)
      inst_version_max = max(num_instalment_version, na.rm = TRUE),
      inst_version_mean = mean(num_instalment_version, na.rm = TRUE),
      
      # Number of different payment versions used
      inst_version_count = n_distinct(num_instalment_version),
      
      # ---- Trend Features ----
      # Compare first half vs second half of payments
      
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), \(x) if_else(is.infinite(x), NA_real_, x)))
}

# ------------------------------------------------------------------------------
# SECTION 4: TRAIN/TEST CONSISTENCY FUNCTIONS
# ------------------------------------------------------------------------------

#' Fit preprocessing parameters from training data
#'
#' Computes all parameters needed for preprocessing (medians, bin thresholds,
#' etc.) from training data only to prevent data leakage.
#'
#' @param train_df Training data frame (cleaned)
#' @return List of preprocessing parameters
fit_preprocessing_params <- function(train_df) {
  # This function ONLY computes parameters - does not transform data
  # Call this on training data, then use apply_preprocessing() on both train/test
  
  message("Fitting preprocessing parameters from training data...")
  
  # EXT_SOURCE medians for imputation
  ext_source_medians <- train_df |>
    summarise(
      ext_source_1 = median(ext_source_1, na.rm = TRUE),
      ext_source_2 = median(ext_source_2, na.rm = TRUE),
      ext_source_3 = median(ext_source_3, na.rm = TRUE)
    ) |>
    unlist()
  
  # Binning thresholds (quintiles)
  income_breaks <- quantile(
    train_df$amt_income_total, 
    probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    na.rm = TRUE
  )
  
  credit_breaks <- quantile(
    train_df$amt_credit, 
    probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    na.rm = TRUE
  )
  
  # Age breaks (fixed)
  age_breaks <- c(0, 25, 35, 45, 55, 65, Inf)
  
  # Column names for ensuring consistency
  # (Will be populated after first full transform)
  
  list(
    ext_source_medians = ext_source_medians,
    bin_params = list(
      income_breaks = income_breaks,
      credit_breaks = credit_breaks,
      age_breaks = age_breaks
    ),
    fitted_on = Sys.time(),
    n_train = nrow(train_df)
  )
}

#' Apply preprocessing with stored parameters
#'
#' Applies all feature engineering transformations using pre-computed
#' parameters. Use for both train (after fitting) and test data.
#'
#' @param df Data frame to transform
#' @param params Preprocessing parameters from fit_preprocessing_params()
#' @return Transformed data frame
apply_preprocessing <- function(df, params, is_train = TRUE) {
  message(sprintf("Applying preprocessing to %s data (%d rows)...", 
                  if(is_train) "training" else "test", nrow(df)))
  
  # Step 1: Clean the data
  df_clean <- df |>
    clean_application_data()
  
  message("  - Data cleaned")
  
  # Step 2: Impute EXT_SOURCE using stored medians
  impute_result <- impute_ext_source(df_clean, medians = params$ext_source_medians)
  df_imputed <- impute_result$data
  
  message("  - EXT_SOURCE imputed")
  
  # Step 3: Create demographic features
  df_demo <- df_imputed |>
    create_demographic_features()
  
  message("  - Demographic features created")
  
  # Step 4: Create financial ratios
  df_ratios <- df_demo |>
    create_financial_ratios()
  
  message("  - Financial ratios created")
  
  # Step 5: Create missing indicators
  df_missing <- df_ratios |>
    create_missing_indicators()
  
  message("  - Missing indicators created")

  # Step 6: Create interaction and binned features using stored bin_params
  interaction_result <- create_interaction_features(df_missing, bin_params = params$bin_params)
  df_final <- interaction_result$data
  
  message("  - Interaction features created")
  message(sprintf("  - Final feature count: %d", ncol(df_final)))
  
  df_final
}

#' Join all features to application data
#'
#' Joins aggregated bureau, previous_application, and installments features
#' to the main application data.
#'
#' @param app_df Application data frame
#' @param bureau_agg Aggregated bureau features (or NULL)
#' @param prev_app_agg Aggregated previous application features (or NULL)
#' @param installments_agg Aggregated installments features (or NULL)
#' @return Data frame with all features joined
join_all_features <- function(app_df, bureau_agg = NULL, prev_app_agg = NULL, 
                               installments_agg = NULL) {
  result <- app_df
  
  # Join bureau aggregations
  if (!is.null(bureau_agg)) {
    result <- result |>
      left_join(bureau_agg, by = "sk_id_curr")
    message(sprintf("  - Joined bureau features (%d columns)", ncol(bureau_agg) - 1))
  }
  
  # Join previous application aggregations
  if (!is.null(prev_app_agg)) {
    result <- result |>
      left_join(prev_app_agg, by = "sk_id_curr")
    message(sprintf("  - Joined previous application features (%d columns)", ncol(prev_app_agg) - 1))
  }
  
  # Join installments aggregations
  if (!is.null(installments_agg)) {
    result <- result |>
      left_join(installments_agg, by = "sk_id_curr")
    message(sprintf("  - Joined installments features (%d columns)", ncol(installments_agg) - 1))
  }
  
  message(sprintf("  - Total columns after join: %d", ncol(result)))
  
  result
}

#' Ensure column consistency between train and test
#'
#' Ensures test data has the same columns as train (except TARGET).
#' Adds missing columns with NA, removes extra columns.
#'
#' @param train_df Processed training data
#' @param test_df Processed test data
#' @return Test data with consistent columns
ensure_column_consistency <- function(train_df, test_df) {
  train_cols <- setdiff(names(train_df), "target")
  test_cols <- names(test_df)
  
  # Columns in train but not test - add with NA
  missing_in_test <- setdiff(train_cols, test_cols)
  if (length(missing_in_test) > 0) {
    message(sprintf("  - Adding %d missing columns to test", length(missing_in_test)))
    for (col in missing_in_test) {
      test_df[[col]] <- NA
    }
  }
  
  # Columns in test but not train - remove
  extra_in_test <- setdiff(test_cols, train_cols)
  extra_in_test <- setdiff(extra_in_test, "sk_id_curr")  # Keep ID
  if (length(extra_in_test) > 0) {
    message(sprintf("  - Removing %d extra columns from test", length(extra_in_test)))
    test_df <- test_df |> select(-all_of(extra_in_test))
  }
  
  # Reorder test columns to match train (minus target)
  test_df |> select(all_of(intersect(train_cols, names(test_df))))
}

# ------------------------------------------------------------------------------
# SECTION 5: MAIN PIPELINE FUNCTION
# ------------------------------------------------------------------------------

#' Run full preprocessing pipeline
#'
#' Executes the complete preprocessing pipeline for train and test data.
#'
#' @param train_path Path to application_train.csv
#' @param test_path Path to application_test.csv
#' @param bureau_path Path to bureau.csv (optional)
#' @param prev_app_path Path to previous_application.csv (optional)
#' @param installments_path Path to installments_payments.csv (optional)
#' @return List with processed train, test, and preprocessing params
run_preprocessing_pipeline <- function(train_path, 
                                        test_path,
                                        bureau_path = NULL,
                                        prev_app_path = NULL,
                                        installments_path = NULL) {
  
  message("=" |> rep(60) |> paste(collapse = ""))
  message("HOME CREDIT PREPROCESSING PIPELINE")
  message("=" |> rep(60) |> paste(collapse = ""))
  
  # --------------------------------------------------------------------------
  # STEP 1: Load raw data
  # --------------------------------------------------------------------------
  message("\n[1/6] Loading raw data...")
  
  train_raw <- read_csv(train_path, show_col_types = FALSE)
  test_raw <- read_csv(test_path, show_col_types = FALSE)
  
  message(sprintf("  - Train: %d rows, %d columns", nrow(train_raw), ncol(train_raw)))
  message(sprintf("  - Test:  %d rows, %d columns", nrow(test_raw), ncol(test_raw)))
  
  # --------------------------------------------------------------------------
  # STEP 2: Aggregate supplementary data (computed once, used for both)
  # --------------------------------------------------------------------------
  message("\n[2/6] Aggregating supplementary data...")
  
  bureau_agg <- NULL
  prev_app_agg <- NULL
  installments_agg <- NULL
  
  if (!is.null(bureau_path) && file.exists(bureau_path)) {
    message("  - Processing bureau.csv...")
    bureau_raw <- read_csv(bureau_path, show_col_types = FALSE)
    bureau_agg <- aggregate_bureau(bureau_raw)
    message(sprintf("    Aggregated to %d customers, %d features", 
                    nrow(bureau_agg), ncol(bureau_agg) - 1))
    rm(bureau_raw)  # Free memory
  }
  
  if (!is.null(prev_app_path) && file.exists(prev_app_path)) {
    message("  - Processing previous_application.csv...")
    prev_app_raw <- read_csv(prev_app_path, show_col_types = FALSE)
    prev_app_agg <- aggregate_previous_application(prev_app_raw)
    message(sprintf("    Aggregated to %d customers, %d features", 
                    nrow(prev_app_agg), ncol(prev_app_agg) - 1))
    rm(prev_app_raw)
  }
  
  if (!is.null(installments_path) && file.exists(installments_path)) {
    message("  - Processing installments_payments.csv...")
    installments_raw <- read_csv(installments_path, show_col_types = FALSE)
    installments_agg <- aggregate_installments(installments_raw)
    message(sprintf("    Aggregated to %d customers, %d features", 
                    nrow(installments_agg), ncol(installments_agg) - 1))
    rm(installments_raw)
  }
  
  # --------------------------------------------------------------------------
  # STEP 3: Clean training data and fit preprocessing parameters
  # --------------------------------------------------------------------------
  message("\n[3/6] Cleaning training data and fitting parameters...")
  
  # Initial cleaning (needed before fitting params)
  train_clean <- train_raw |> clean_application_data()
  
  # Fit parameters from training data only
  params <- fit_preprocessing_params(train_clean)
  
  # --------------------------------------------------------------------------
  # STEP 4: Apply full preprocessing to training data
  # --------------------------------------------------------------------------
  message("\n[4/6] Applying preprocessing to training data...")
  
  train_processed <- apply_preprocessing(train_raw, params, is_train = TRUE)
  
  # Join aggregated features
  train_processed <- join_all_features(
    train_processed, 
    bureau_agg, 
    prev_app_agg, 
    installments_agg
  )
  
  # --------------------------------------------------------------------------
  # STEP 5: Apply preprocessing to test data using stored parameters
  # --------------------------------------------------------------------------
  message("\n[5/6] Applying preprocessing to test data...")
  
  test_processed <- apply_preprocessing(test_raw, params, is_train = FALSE)
  
  # Join aggregated features
  test_processed <- join_all_features(
    test_processed, 
    bureau_agg, 
    prev_app_agg, 
    installments_agg
  )
  
  # --------------------------------------------------------------------------
  # STEP 6: Ensure column consistency
  # --------------------------------------------------------------------------
  message("\n[6/6] Ensuring column consistency...")
  
  test_processed <- ensure_column_consistency(train_processed, test_processed)
  
  # Store column names in params for future reference
  params$feature_names <- setdiff(names(train_processed), "target")
  params$n_features <- length(params$feature_names)
  
  # --------------------------------------------------------------------------
  # Summary
  # --------------------------------------------------------------------------
  message("\n" |> paste0("=" |> rep(60) |> paste(collapse = "")))
  message("PIPELINE COMPLETE")
  message("=" |> rep(60) |> paste(collapse = ""))
  message(sprintf("  Train: %d rows, %d features", nrow(train_processed), ncol(train_processed)))
  message(sprintf("  Test:  %d rows, %d features", nrow(test_processed), ncol(test_processed)))
  message(sprintf("  New features created: %d", params$n_features - ncol(train_raw) + 1))
  
  list(
    train = train_processed,
    test = test_processed,
    params = params,
    aggregations = list(
      bureau = bureau_agg,
      previous_application = prev_app_agg,
      installments = installments_agg
    )
  )
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# To run the full pipeline, uncomment and modify the paths below:
#
# # Set your data directory
# data_dir <- "/Users/kateklinger/Desktop/Grad School/Capstone 1/MSBA_CAPSTONE_1/data"
# 
# # Run the full pipeline
# result <- run_preprocessing_pipeline(
#   train_path = file.path(data_dir, "application_train.csv"),
#   test_path = file.path(data_dir, "application_test.csv"),
#   bureau_path = file.path(data_dir, "bureau.csv"),
#   prev_app_path = file.path(data_dir, "previous_application.csv"),
#   installments_path = file.path(data_dir, "installments_payments.csv")
# )
# 
# # Access results
# train_processed <- result$train
# test_processed <- result$test
# params <- result$params
# 
# # Save processed data
# write_csv(train_processed, file.path(data_dir, "train_processed.csv"))
# write_csv(test_processed, file.path(data_dir, "test_processed.csv"))
# 
# # Save preprocessing parameters for reproducibility
# saveRDS(params, file.path(data_dir, "preprocessing_params.rds"))

# ------------------------------------------------------------------------------
# INDIVIDUAL FUNCTION USAGE EXAMPLES
# ------------------------------------------------------------------------------

# You can also use functions individually:
#
# # Load and clean data
# train <- read_csv("application_train.csv") |> clean_application_data()
# 
# # Create features step by step
# train <- train |>
#   create_demographic_features() |>
#   create_financial_ratios() |>
#   create_missing_indicators()
# 
# # Aggregate bureau data separately
# bureau <- read_csv("bureau.csv")
# bureau_features <- aggregate_bureau(bureau)
# 
# # Join to main data
# train <- train |> left_join(bureau_features, by = "sk_id_curr")
