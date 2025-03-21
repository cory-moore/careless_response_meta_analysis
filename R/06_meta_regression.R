###############################################################################
# 06_meta_regression.R
# 
# This script implements comprehensive meta-regression analyses to identify
# factors influencing careless responding prevalence.
#
# The script runs and saves ALL results regardless of statistical significance:
# 1. Univariate meta-regressions for all viable predictors
# 2. Bivariate meta-regressions for ALL possible predictor combinations
# 3. Multivariate models with the best predictors
# 4. Special analyses including temporal trends
###############################################################################

library(tidyverse)
library(metafor)
library(glue)

# Create output directory
dir.create("output/r_results/meta_regression", recursive = TRUE, showWarnings = FALSE)

# Load prepared data
first_method_data <- read_csv("data/for_r_meta/first_method_data.csv", show_col_types = FALSE)
cat(glue("Loaded First-Method dataset: {nrow(first_method_data)} studies\n"))

# Load codebook if available
load_codebook <- \(path = "codebook.json") {
  if (file.exists(path)) {
    jsonlite::read_json(path)
  } else {
    NULL
  }
}
codebook <- load_codebook()

#------------------------------------------------------------------------------
# Define predictor variables based on available subgroup dimensions
#------------------------------------------------------------------------------

# Classify predictors
categorical_predictors <- c(
  "sample_source", "sample_recruitment", "sample_platform", 
  "sample_method", "journal", "sample_level", "sample_incentive", 
  "sample_country", "design_method", "design_location"
)

continuous_predictors <- c("year", "sample_size", "sample_age", "total_items")

# Filter to available predictors in dataset
categorical_predictors <- categorical_predictors[categorical_predictors %in% colnames(first_method_data)]
continuous_predictors <- continuous_predictors[continuous_predictors %in% colnames(first_method_data)]

cat("\nAnalyzing predictors of careless responding rates:\n")
cat("  Categorical predictors:", paste(categorical_predictors, collapse = ", "), "\n")
cat("  Continuous predictors:", paste(continuous_predictors, collapse = ", "), "\n")
cat("\nNote: method_type excluded from meta-regression due to extreme imbalance\n")

#------------------------------------------------------------------------------
# Incorporate method type results from previous subgroup analysis
#------------------------------------------------------------------------------

cat("\nINCORPORATING METHOD TYPE RESULTS FROM SUBGROUP ANALYSIS\n")
cat("======================================================\n")

method_type_subgroups <- try(
  read_csv("output/r_results/primary/subgroup_method_type.csv", show_col_types = FALSE),
  silent = TRUE
)

if(!inherits(method_type_subgroups, "try-error")) {
  cat(glue("  Successfully imported method type subgroup results\n"))
  cat(glue("  {nrow(method_type_subgroups)} method types analyzed in subgroup analysis\n"))
  
  # Display subgroup results
  for(i in 1:nrow(method_type_subgroups)) {
    cat(glue("    {method_type_subgroups$Subgroup[i]}: {round(method_type_subgroups$Pooled_Proportion[i] * 100, 1)}% ",
             "[{round(method_type_subgroups$CI_Lower[i] * 100, 1)}-{round(method_type_subgroups$CI_Upper[i] * 100, 1)}%] ",
             "(k = {method_type_subgroups$k[i]})\n"))
  }
  
  # Save method type subgroups for reference
  write_csv(method_type_subgroups, "output/r_results/meta_regression/method_type_subgroups.csv")
} else {
  cat("  Could not import method type subgroup results\n")
}

#------------------------------------------------------------------------------
# Functions for meta-regression analysis
#------------------------------------------------------------------------------

# Check if a categorical variable has enough data per category
check_category_counts <- function(data, var, min_count = 2) {
  if(!var %in% colnames(data)) return(FALSE)
  
  counts <- table(data[[var]])
  min_obs <- min(counts)
  
  if(min_obs < min_count) {
    cat(glue("  Note: {var} has some categories with only {min_obs} observations\n"))
    # We'll still proceed, but with a warning
    return(TRUE)
  }
  
  # Check for extreme imbalance
  if(length(counts) > 1) {
    max_pct <- max(counts) / sum(counts)
    if(max_pct > 0.9) {
      cat(glue("  Note: {var} has extreme imbalance (max category = {round(max_pct*100)}% of data)\n"))
      # We'll still proceed, but with a warning
      return(TRUE)
    }
  }
  
  return(TRUE)
}

# Run a simple meta-regression for a single predictor
run_simple_meta <- function(data, var, is_categorical = TRUE) {
  # Check if categorical variable has enough data (but still proceed with warning)
  if(is_categorical) {
    check_category_counts(data, var)
  }
  
  # Create formula
  formula <- if(is_categorical) {
    as.formula(paste("logit_prop ~", paste0("factor(", var, ")")))
  } else {
    as.formula(paste("logit_prop ~", var))
  }
  
  # Run null model for comparison
  null_model <- try(rma(logit_prop, vi = var_logit, data = data, method = "REML"), silent = TRUE)
  if(inherits(null_model, "try-error")) {
    cat(glue("  Error in null model: {attr(null_model, 'condition')$message}\n"))
    return(NULL)
  }
  
  # Run model
  model <- try(rma(formula, vi = var_logit, data = data, method = "REML"), silent = TRUE)
  if(inherits(model, "try-error")) {
    cat(glue("  Error in {var} model: {attr(model, 'condition')$message}\n"))
    return(NULL)
  }
  
  # Calculate R-squared
  r_squared <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
  
  # Capture results
  results <- tibble(
    predictor = var,
    n_predictors = length(model$b) - 1,
    k = model$k,
    QM = model$QM,
    QM_df = model$p - 1,
    QM_p = model$QMp,
    R2 = r_squared,
    tau2 = model$tau2,
    I2 = model$I2,
    AIC = -2 * model$ll + 2 * (length(model$b) + 1),
    is_significant = model$QMp < 0.05
  )
  
  # Extract coefficients (for all models, not just significant ones)
  coefs <- try({
    coef_summary <- coef(summary(model))
    as_tibble(coef_summary, rownames = "term") %>%
      mutate(
        predictor = var,
        term = str_remove(term, "factor\\("),
        term = str_remove(term, "\\)"),
        sig = case_when(
          pval < 0.001 ~ "***",
          pval < 0.01 ~ "**",
          pval < 0.05 ~ "*",
          pval < 0.1 ~ ".",
          TRUE ~ ""
        )
      )
  }, silent = TRUE)
  
  if(!inherits(coefs, "try-error")) {
    write_csv(coefs, glue("output/r_results/meta_regression/coefs_{var}.csv"))
  }
  
  return(list(
    results = results,
    model = model,
    null_model = null_model
  ))
}

#------------------------------------------------------------------------------
# Run univariate meta-regressions
#------------------------------------------------------------------------------

cat("\nRUNNING UNIVARIATE META-REGRESSIONS\n")
cat("===================================\n")

# Process categorical predictors
cat_results <- tibble()
cat_models <- list()

for(var in categorical_predictors) {
  cat(glue("  Analyzing {var}...\n"))
  
  result <- run_simple_meta(first_method_data, var, TRUE)
  if(!is.null(result)) {
    cat_results <- bind_rows(cat_results, result$results)
    cat_models[[var]] <- list(
      model = result$model,
      null_model = result$null_model
    )
  }
}

# Process continuous predictors
cont_results <- tibble()
cont_models <- list()

for(var in continuous_predictors) {
  cat(glue("  Analyzing {var}...\n"))
  
  result <- run_simple_meta(first_method_data, var, FALSE)
  if(!is.null(result)) {
    cont_results <- bind_rows(cont_results, result$results)
    cont_models[[var]] <- list(
      model = result$model,
      null_model = result$null_model
    )
  }
}

# Combine results
all_results <- bind_rows(cat_results, cont_results) %>%
  arrange(QM_p)

# Save results
if(nrow(all_results) > 0) {
  write_csv(all_results, "output/r_results/meta_regression/01_univariate_results.csv")
  
  cat("\nUnivariate Meta-Regression Results:\n")
  cat(glue("  {nrow(all_results)} predictors analyzed\n"))
  cat(glue("  {sum(all_results$is_significant)} significant predictors found\n"))
  
  # Display top predictors
  if(sum(all_results$is_significant) > 0) {
    sig_results <- all_results %>% filter(is_significant)
    cat("\nSignificant predictors (p < 0.05):\n")
    for(i in 1:nrow(sig_results)) {
      cat(glue("  {i}. {sig_results$predictor[i]}: R² = {round(sig_results$R2[i]*100, 1)}%, ",
               "p = {format.pval(sig_results$QM_p[i], digits=3)}\n"))
    }
  } else {
    cat("\nNo significant predictors found at p < 0.05 level.\n")
  }
  
  # Create a combined coefficients file
  all_coefs_files <- list.files("output/r_results/meta_regression", pattern = "^coefs_.*\\.csv$", full.names = TRUE)
  
  if(length(all_coefs_files) > 0) {
    all_coefs <- map_dfr(all_coefs_files, read_csv, show_col_types = FALSE)
    write_csv(all_coefs, "output/r_results/meta_regression/01_all_univariate_coefficients.csv")
  }
} else {
  cat("\nNo valid univariate meta-regression results were found.\n")
  cat("This could be due to data issues or modeling problems.\n")
  
  # Create empty results file
  write_csv(tibble(note = "No valid meta-regression results"), 
           "output/r_results/meta_regression/01_univariate_results.csv")
}

#------------------------------------------------------------------------------
# Bivariate meta-regressions
#------------------------------------------------------------------------------

# Proceed with bivariate analysis regardless of significance
cat("\nRUNNING BIVARIATE META-REGRESSIONS\n")
cat("================================\n")

# Get all predictors that had valid models
all_predictors <- names(c(cat_models, cont_models))

# Function to run bivariate model
run_bivariate_meta <- function(var1, var2) {
  cat(glue("  Analyzing {var1} + {var2}...\n"))
  
  # Determine if categorical
  var1_cat <- var1 %in% categorical_predictors
  var2_cat <- var2 %in% categorical_predictors
  
  # Create formula
  formula_str <- paste(
    "logit_prop ~", 
    paste0(ifelse(var1_cat, paste0("factor(", var1, ")"), var1), " + ",
           ifelse(var2_cat, paste0("factor(", var2, ")"), var2))
  )
  formula <- as.formula(formula_str)
  
  # Run model
  model <- try(rma(formula, vi = var_logit, data = first_method_data, method = "REML"), silent = TRUE)
  if(inherits(model, "try-error")) {
    cat(glue("  Error in bivariate model: {attr(model, 'condition')$message}\n"))
    return(NULL)
  }
  
  # Calculate R-squared vs null model
  null_model <- rma(logit_prop, vi = var_logit, data = first_method_data, method = "REML")
  r_squared <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
  
  # Results
  results <- tibble(
    predictors = paste(var1, "+", var2),
    n_predictors = length(model$b) - 1,
    k = model$k,
    QM = model$QM,
    QM_df = model$p - 1,
    QM_p = model$QMp,
    R2 = r_squared,
    tau2 = model$tau2,
    I2 = model$I2,
    AIC = -2 * model$ll + 2 * (length(model$b) + 1),
    is_significant = model$QMp < 0.05
  )
  
  # Extract coefficients (for all models, not just significant ones)
  coefs <- try({
    coef_summary <- coef(summary(model))
    as_tibble(coef_summary, rownames = "term") %>%
      mutate(
        model = paste(var1, "+", var2),
        term = str_remove(term, "factor\\("),
        term = str_remove(term, "\\)"),
        sig = case_when(
          pval < 0.001 ~ "***",
          pval < 0.01 ~ "**",
          pval < 0.05 ~ "*",
          pval < 0.1 ~ ".",
          TRUE ~ ""
        )
      )
  }, silent = TRUE)
  
  if(!inherits(coefs, "try-error")) {
    write_csv(coefs, glue("output/r_results/meta_regression/bivar_coefs_{var1}_{var2}.csv"))
  }
  
  return(list(
    results = results,
    model = model
  ))
}

# Generate all pairs of predictors (for efficiency, limit to 20 combinations maximum)
if(length(all_predictors) >= 2) {
  # Use a pragmatic approach for generating pairs - limit to manageable number
  # We'll prioritize the top predictors by QM value
  
  # Sort predictors by QM value (higher is better)
  sorted_predictors <- all_results %>%
    arrange(desc(QM)) %>%
    pull(predictor)
  
  # Take the top 5 or fewer predictors for combinations (to avoid computational explosion)
  if(length(sorted_predictors) > 5) {
    top_predictors <- sorted_predictors[1:5]
  } else {
    top_predictors <- sorted_predictors
  }
  
  # Generate combinations
  bivariate_results <- tibble()
  bivariate_models <- list()
  
  # Get all pairs from top predictors
  all_pairs <- list()
  for(i in 1:(length(top_predictors)-1)) {
    for(j in (i+1):length(top_predictors)) {
      all_pairs[[length(all_pairs) + 1]] <- c(top_predictors[i], top_predictors[j])
    }
  }
  
  cat(glue("  Generating {length(all_pairs)} bivariate models from top predictors\n"))
  
  # Run models for all pairs
  for(pair in all_pairs) {
    result <- run_bivariate_meta(pair[1], pair[2])
    if(!is.null(result)) {
      bivariate_results <- bind_rows(bivariate_results, result$results)
      bivariate_models[[paste(pair[1], "+", pair[2])]] <- result$model
    }
  }
  
  # Save results
  if(nrow(bivariate_results) > 0) {
    bivariate_results <- bivariate_results %>% arrange(AIC)
    write_csv(bivariate_results, "output/r_results/meta_regression/02_bivariate_results.csv")
    
    cat("\nBivariate Meta-Regression Results:\n")
    cat(glue("  {nrow(bivariate_results)} bivariate models analyzed\n"))
    cat(glue("  {sum(bivariate_results$is_significant)} significant models found\n"))
    
    # Display top models
    if(nrow(bivariate_results) > 0) {
      top_models <- head(bivariate_results, 3)
      cat("\nTop 3 bivariate models (by AIC):\n")
      for(i in 1:nrow(top_models)) {
        cat(glue("  {i}. {top_models$predictors[i]}: R² = {round(top_models$R2[i]*100, 1)}%, ",
                 "AIC = {round(top_models$AIC[i], 1)}\n"))
      }
    }
    
    # Create a combined coefficients file
    bivar_coefs_files <- list.files("output/r_results/meta_regression", pattern = "^bivar_coefs_.*\\.csv$", full.names = TRUE)
    
    if(length(bivar_coefs_files) > 0) {
      bivar_coefs <- map_dfr(bivar_coefs_files, read_csv, show_col_types = FALSE)
      write_csv(bivar_coefs, "output/r_results/meta_regression/02_all_bivariate_coefficients.csv")
    }
  } else {
    cat("\nNo valid bivariate meta-regression results were found.\n")
  }
} else {
  cat("\nInsufficient valid predictors for bivariate analysis.\n")
}

#------------------------------------------------------------------------------
# Multivariate model with best predictors
#------------------------------------------------------------------------------

cat("\nRUNNING MULTIVARIATE META-REGRESSION\n")
cat("==================================\n")

# Use the top 3 predictors regardless of significance
if(nrow(all_results) >= 3) {
  top_predictors <- all_results %>%
    arrange(desc(QM)) %>%
    head(3) %>%
    pull(predictor)
  
  # Create formula string
  formula_parts <- c()
  
  for(var in top_predictors) {
    var_term <- if(var %in% categorical_predictors) {
      paste0("factor(", var, ")")
    } else {
      var
    }
    formula_parts <- c(formula_parts, var_term)
  }
  
  formula_str <- paste("logit_prop ~", paste(formula_parts, collapse = " + "))
  
  cat(glue("  Running multivariate model: {formula_str}\n"))
  
  # Run model
  multi_model <- try(rma(as.formula(formula_str), vi = var_logit, 
                        data = first_method_data, method = "REML"), silent = TRUE)
  
  if(!inherits(multi_model, "try-error")) {
    # Calculate R-squared vs null model
    null_model <- rma(logit_prop, vi = var_logit, data = first_method_data, method = "REML")
    r_squared <- max(0, (null_model$tau2 - multi_model$tau2) / null_model$tau2)
    
    # Results
    multi_results <- tibble(
      predictors = paste(top_predictors, collapse = " + "),
      n_predictors = length(multi_model$b) - 1,
      k = multi_model$k,
      QM = multi_model$QM,
      QM_df = multi_model$p - 1,
      QM_p = multi_model$QMp,
      R2 = r_squared,
      tau2 = multi_model$tau2,
      I2 = multi_model$I2,
      AIC = -2 * multi_model$ll + 2 * (length(multi_model$b) + 1),
      is_significant = multi_model$QMp < 0.05
    )
    
    write_csv(multi_results, "output/r_results/meta_regression/03_multivariate_results.csv")
    
    cat(glue("  Multivariate model results: R² = {round(r_squared*100, 1)}%, p = {format.pval(multi_model$QMp, digits=3)}\n"))
    
    # Extract coefficients
    multi_coefs <- try({
      coef_summary <- coef(summary(multi_model))
      as_tibble(coef_summary, rownames = "term") %>%
        mutate(
          model = "Multivariate",
          term = str_remove(term, "factor\\("),
          term = str_remove(term, "\\)"),
          sig = case_when(
            pval < 0.001 ~ "***",
            pval < 0.01 ~ "**",
            pval < 0.05 ~ "*",
            pval < 0.1 ~ ".",
            TRUE ~ ""
          )
        )
    }, silent = TRUE)
    
    if(!inherits(multi_coefs, "try-error")) {
      write_csv(multi_coefs, "output/r_results/meta_regression/03_multivariate_coefficients.csv")
    }
  } else {
    cat(glue("  Error in multivariate model: {attr(multi_model, 'condition')$message}\n"))
  }
} else {
  cat("  Insufficient valid predictors for multivariate analysis.\n")
}

#------------------------------------------------------------------------------
# Special Analysis: Temporal Trends
#------------------------------------------------------------------------------

if("year" %in% colnames(first_method_data)) {
  cat("\nANALYZING TEMPORAL TRENDS\n")
  cat("=======================\n")
  
  # Center year
  first_method_data <- first_method_data %>%
    mutate(
      year_centered = year - mean(year, na.rm = TRUE),
      year_squared = year_centered^2
    )
  
  # Linear trend
  linear_model <- try(rma(logit_prop ~ year_centered, vi = var_logit, 
                          data = first_method_data, method = "REML"), silent = TRUE)
  
  # Non-linear trend
  quadratic_model <- try(rma(logit_prop ~ year_centered + year_squared, vi = var_logit, 
                            data = first_method_data, method = "REML"), silent = TRUE)
  
  temporal_results <- tibble()
  
  # Process linear model
  if(!inherits(linear_model, "try-error")) {
    null_model <- rma(logit_prop, vi = var_logit, data = first_method_data, method = "REML")
    r_squared <- max(0, (null_model$tau2 - linear_model$tau2) / null_model$tau2)
    
    linear_results <- tibble(
      model = "Year (Linear)",
      n_predictors = length(linear_model$b) - 1,
      k = linear_model$k,
      QM = linear_model$QM,
      QM_p = linear_model$QMp,
      R2 = r_squared,
      tau2 = linear_model$tau2,
      I2 = linear_model$I2,
      AIC = -2 * linear_model$ll + 2 * (length(linear_model$b) + 1),
      is_significant = linear_model$QMp < 0.05
    )
    
    temporal_results <- bind_rows(temporal_results, linear_results)
    
    cat(glue("  Linear trend: p = {format.pval(linear_model$QMp, digits=3)}\n"))
    
    # Extract coefficients
    linear_coefs <- try({
      coef_summary <- coef(summary(linear_model))
      as_tibble(coef_summary, rownames = "term") %>%
        mutate(
          model = "Linear Trend",
          sig = case_when(
            pval < 0.001 ~ "***",
            pval < 0.01 ~ "**",
            pval < 0.05 ~ "*",
            pval < 0.1 ~ ".",
            TRUE ~ ""
          )
        )
    }, silent = TRUE)
    
    if(!inherits(linear_coefs, "try-error")) {
      write_csv(linear_coefs, "output/r_results/meta_regression/04_linear_trend_coefs.csv")
    }
  }
  
  # Process quadratic model
  if(!inherits(quadratic_model, "try-error")) {
    null_model <- rma(logit_prop, vi = var_logit, data = first_method_data, method = "REML")
    r_squared <- max(0, (null_model$tau2 - quadratic_model$tau2) / null_model$tau2)
    
    quadratic_results <- tibble(
      model = "Year (Quadratic)",
      n_predictors = length(quadratic_model$b) - 1,
      k = quadratic_model$k,
      QM = quadratic_model$QM,
      QM_p = quadratic_model$QMp,
      R2 = r_squared,
      tau2 = quadratic_model$tau2,
      I2 = quadratic_model$I2,
      AIC = -2 * quadratic_model$ll + 2 * (length(quadratic_model$b) + 1),
      is_significant = quadratic_model$QMp < 0.05
    )
    
    temporal_results <- bind_rows(temporal_results, quadratic_results)
    
    cat(glue("  Quadratic trend: p = {format.pval(quadratic_model$QMp, digits=3)}\n"))
    
    # Extract coefficients
    quadratic_coefs <- try({
      coef_summary <- coef(summary(quadratic_model))
      as_tibble(coef_summary, rownames = "term") %>%
        mutate(
          model = "Quadratic Trend",
          sig = case_when(
            pval < 0.001 ~ "***",
            pval < 0.01 ~ "**",
            pval < 0.05 ~ "*",
            pval < 0.1 ~ ".",
            TRUE ~ ""
          )
        )
    }, silent = TRUE)
    
    if(!inherits(quadratic_coefs, "try-error")) {
      write_csv(quadratic_coefs, "output/r_results/meta_regression/04_quadratic_trend_coefs.csv")
    }
  }
  
  # Save results
  if(nrow(temporal_results) > 0) {
    write_csv(temporal_results, "output/r_results/meta_regression/04_temporal_trends.csv")
  }
}

#------------------------------------------------------------------------------
# Create model comparison summary
#------------------------------------------------------------------------------

cat("\nCREATING MODEL COMPARISON SUMMARY\n")
cat("===============================\n")

model_comparison <- tibble()

# Add univariate results
if(exists("all_results") && nrow(all_results) > 0) {
  univariate_comparison <- all_results %>%
    mutate(
      model_type = "Univariate",
      model_name = predictor,
      n_parameters = n_predictors + 1  # +1 for intercept
    ) %>%
    select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
  
  model_comparison <- bind_rows(model_comparison, univariate_comparison)
}

# Add bivariate results
if(exists("bivariate_results") && nrow(bivariate_results) > 0) {
  bivariate_comparison <- bivariate_results %>%
    mutate(
      model_type = "Bivariate",
      model_name = predictors,
      n_parameters = n_predictors + 1  # +1 for intercept
    ) %>%
    select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
  
  model_comparison <- bind_rows(model_comparison, bivariate_comparison)
}

# Add multivariate results
if(exists("multi_results") && nrow(multi_results) > 0) {
  multi_comparison <- multi_results %>%
    mutate(
      model_type = "Multivariate",
      model_name = predictors,
      n_parameters = n_predictors + 1  # +1 for intercept
    ) %>%
    select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
  
  model_comparison <- bind_rows(model_comparison, multi_comparison)
}

# Add temporal results
if(exists("temporal_results") && nrow(temporal_results) > 0) {
  temporal_comparison <- temporal_results %>%
    mutate(
      model_type = "Temporal",
      model_name = model,
      n_parameters = case_when(
        model == "Year (Linear)" ~ 2,
        model == "Year (Quadratic)" ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
  
  model_comparison <- bind_rows(model_comparison, temporal_comparison)
}

# Sort by AIC
if(nrow(model_comparison) > 0) {
  model_comparison <- model_comparison %>%
    arrange(AIC) %>%
    mutate(AIC_weight = exp(-(AIC - min(AIC))/2) / sum(exp(-(AIC - min(AIC))/2)))
  
  # Export comparison table
  write_csv(model_comparison, "output/r_results/meta_regression/05_model_comparison.csv")
  
  cat(glue("\nCreated model comparison summary with {nrow(model_comparison)} models\n"))
  cat("  Top 5 models by AIC:\n")
  
  top_models <- head(model_comparison, min(5, nrow(model_comparison)))
  for(i in 1:nrow(top_models)) {
    cat(glue("    {i}. {top_models$model_type[i]}: {top_models$model_name[i]} ",
             "(AIC = {round(top_models$AIC[i], 1)}, R² = {round(top_models$R2[i] * 100, 1)}%, ",
             "AIC weight = {round(top_models$AIC_weight[i] * 100, 1)}%)\n"))
  }
}

#------------------------------------------------------------------------------
# Create a summary of effect size for dissertation
#------------------------------------------------------------------------------

cat("\nCREATING EFFECT SIZE SUMMARY FOR DISSERTATION\n")
cat("==========================================\n")

# Create a consolidated summary for use in results/discussion
effect_summary <- tibble()

# Add univariate R² values
if(exists("all_results") && nrow(all_results) > 0) {
  univariate_effect <- all_results %>%
    select(model_type = "Univariate", variable = predictor, R2, QM_p) %>%
    arrange(desc(R2)) %>%
    mutate(R2_formatted = paste0(round(R2 * 100, 1), "%"),
           significance = ifelse(QM_p < 0.05, "Significant", "Non-significant"))
  
  effect_summary <- bind_rows(effect_summary, univariate_effect)
}

# Add bivariate R² values
if(exists("bivariate_results") && nrow(bivariate_results) > 0) {
  bivariate_effect <- bivariate_results %>%
    select(model_type = "Bivariate", variable = predictors, R2, QM_p) %>%
    arrange(desc(R2)) %>%
    mutate(R2_formatted = paste0(round(R2 * 100, 1), "%"),
           significance = ifelse(QM_p < 0.05, "Significant", "Non-significant"))
  
  effect_summary <- bind_rows(effect_summary, bivariate_effect)
}

# Add multivariate R² values
if(exists("multi_results") && nrow(multi_results) > 0) {
  multivariate_effect <- multi_results %>%
    select(model_type = "Multivariate", variable = predictors, R2, QM_p) %>%
    arrange(desc(R2)) %>%
    mutate(R2_formatted = paste0(round(R2 * 100, 1), "%"),
           significance = ifelse(QM_p < 0.05, "Significant", "Non-significant"))
  
  effect_summary <- bind_rows(effect_summary, multivariate_effect)
}

# Add temporal trend R² values
if(exists("temporal_results") && nrow(temporal_results) > 0) {
  temporal_effect <- temporal_results %>%
    select(model_type = "Temporal", variable = model, R2, QM_p) %>%
    arrange(desc(R2)) %>%
    mutate(R2_formatted = paste0(round(R2 * 100, 1), "%"),
           significance = ifelse(QM_p < 0.05, "Significant", "Non-significant"))
  
  effect_summary <- bind_rows(effect_summary, temporal_effect)
}

# Sort and save effect summary
if(nrow(effect_summary) > 0) {
  effect_summary <- effect_summary %>%
    arrange(desc(R2))
  
  write_csv(effect_summary, "output/r_results/meta_regression/06_effect_size_summary.csv")
  
  # Print top effect sizes
  cat("\nTop 5 predictors by effect size (R²):\n")
  top_effects <- head(effect_summary, min(5, nrow(effect_summary)))
  for(i in 1:nrow(top_effects)) {
    cat(glue("  {i}. {top_effects$model_type[i]}: {top_effects$variable[i]} - R² = {top_effects$R2_formatted[i]} ({top_effects$significance[i]})\n"))
  }
}

#------------------------------------------------------------------------------
# Examine interaction effects if we have significant predictors
#------------------------------------------------------------------------------

if(exists("all_results") && sum(all_results$is_significant) >= 2) {
  cat("\nEXAMINING INTERACTION EFFECTS\n")
  cat("===========================\n")
  
  # Get top two significant predictors
  sig_predictors <- all_results %>%
    filter(is_significant) %>%
    head(2) %>%
    pull(predictor)
  
  if(length(sig_predictors) >= 2) {
    var1 <- sig_predictors[1]
    var2 <- sig_predictors[2]
    
    # Check if categorical
    var1_cat <- var1 %in% categorical_predictors
    var2_cat <- var2 %in% categorical_predictors
    
    # Create interaction formula
    formula_str <- paste(
      "logit_prop ~", 
      paste0(ifelse(var1_cat, paste0("factor(", var1, ")"), var1), " * ",
             ifelse(var2_cat, paste0("factor(", var2, ")"), var2))
    )
    
    cat(glue("  Testing interaction: {formula_str}\n"))
    
    # Run interaction model
    interaction_model <- try(rma(as.formula(formula_str), vi = var_logit, 
                                data = first_method_data, method = "REML"), silent = TRUE)
    
    if(!inherits(interaction_model, "try-error")) {
      # Create main effects model for comparison
      main_effects_str <- paste(
        "logit_prop ~", 
        paste0(ifelse(var1_cat, paste0("factor(", var1, ")"), var1), " + ",
               ifelse(var2_cat, paste0("factor(", var2, ")"), var2))
      )
      
      main_effects_model <- try(rma(as.formula(main_effects_str), vi = var_logit, 
                                    data = first_method_data, method = "REML"), silent = TRUE)
      
      if(!inherits(main_effects_model, "try-error")) {
        # Calculate improvement with interaction
        r_squared_main <- max(0, (null_model$tau2 - main_effects_model$tau2) / null_model$tau2)
        r_squared_interaction <- max(0, (null_model$tau2 - interaction_model$tau2) / null_model$tau2)
        r_squared_improvement <- r_squared_interaction - r_squared_main
        
        # Compare models
        cat(glue("  Main effects model: R² = {round(r_squared_main*100, 1)}%\n"))
        cat(glue("  Interaction model: R² = {round(r_squared_interaction*100, 1)}%\n"))
        cat(glue("  Improvement with interaction: {round(r_squared_improvement*100, 1)}%\n"))
        
        # Get p-value for interaction
        interaction_coefs <- coef(summary(interaction_model))
        interaction_terms <- rownames(interaction_coefs)[grepl(":", rownames(interaction_coefs))]
        
        if(length(interaction_terms) > 0) {
          interaction_p_values <- interaction_coefs[interaction_terms, "pval"]
          min_p_value <- min(interaction_p_values)
          cat(glue("  Interaction significance: p = {format.pval(min_p_value, digits=3)}\n"))
        }
        
        # Create interaction model summary
        interaction_results <- tibble(
          model = paste(var1, "*", var2),
          main_effects_R2 = r_squared_main,
          interaction_R2 = r_squared_interaction,
          R2_improvement = r_squared_improvement,
          interaction_p = if(length(interaction_terms) > 0) min_p_value else NA,
          AIC_main = -2 * main_effects_model$ll + 2 * (length(main_effects_model$b) + 1),
          AIC_interaction = -2 * interaction_model$ll + 2 * (length(interaction_model$b) + 1),
          is_significant_improvement = if(length(interaction_terms) > 0) min_p_value < 0.05 else FALSE
        )
        
        write_csv(interaction_results, "output/r_results/meta_regression/07_interaction_results.csv")
        
        # Extract coefficients
        interaction_coefs_df <- try({
          coef_summary <- coef(summary(interaction_model))
          as_tibble(coef_summary, rownames = "term") %>%
            mutate(
              model = paste(var1, "*", var2),
              term = str_remove_all(term, "factor\\(|\\)"),
              is_interaction = grepl(":", term),
              sig = case_when(
                pval < 0.001 ~ "***",
                pval < 0.01 ~ "**",
                pval < 0.05 ~ "*",
                pval < 0.1 ~ ".",
                TRUE ~ ""
              )
            )
        }, silent = TRUE)
        
        if(!inherits(interaction_coefs_df, "try-error")) {
          write_csv(interaction_coefs_df, "output/r_results/meta_regression/07_interaction_coefficients.csv")
        }
      }
    } else {
      cat(glue("  Error in interaction model: {attr(interaction_model, 'condition')$message}\n"))
    }
  } else {
    cat("  Insufficient significant predictors for interaction analysis.\n")
  }
}

cat("\nMeta-regression analysis complete! All results saved to output/r_results/meta_regression/\n")