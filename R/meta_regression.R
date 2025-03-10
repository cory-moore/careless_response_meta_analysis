###############################################################################
# R script for meta-regression analysis on careless responding data
#
# This script performs meta-regression analyses to identify moderators that 
# influence the prevalence of careless responding in survey data. It implements
# mixed-effects meta-regression models using the metafor package to examine both
# continuous and categorical moderators.
#
# Workflow:
# 1. Load and install required packages:
#    - dplyr: For data manipulation and transformation
#    - metafor: For meta-regression functions (rma)
#    - ggplot2: For creating visualization plots
#    - gridExtra: For arranging multiple plots
#
# 2. Define `prepare_data` function that:
#    - Reads data from CSV files
#    - Validates presence of required columns
#    - Computes proportions if not directly available
#    - Applies logit transformation to proportions for analysis
#    - Calculates study-specific variances based on sample size and proportion
#    - Returns prepared dataset ready for meta-regression
#
# 3. Define `run_meta_regression` function that:
#    - Fits both intercept-only and moderator models using REML estimation
#    - Handles errors gracefully with informative messages
#    - Returns both models for comparison and R² calculation
#
# 4. Define `format_meta_regression_results` function that:
#    - Extracts key statistics from meta-regression models
#    - Calculates R² to quantify heterogeneity explained by moderators
#    - Formats results for reporting (QM, QE, I², p-values)
#
# 5. Define `main` function that:
#    - Locates and loads the appropriate data file
#    - Prepares data for analysis using the prepare_data function
#    - Identifies potential continuous and categorical moderators
#    - Runs meta-regression for each moderator
#    - Compiles results into summary tables
#    - Extracts coefficients for significant moderators
#    - Converts logit results back to proportion scale for interpretability
#    - Saves all results to CSV files
#
# Key statistics reported:
# - QM: Test statistic for the omnibus test of moderators
# - QE: Test statistic for residual heterogeneity
# - I²: Percentage of total variability due to heterogeneity
# - R²: Percentage of heterogeneity explained by the moderator
#
###############################################################################

library(dplyr)
library(metafor)
library(ggplot2)
library(gridExtra)

prepare_data <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  cat("Columns in data file:", paste(colnames(data), collapse=", "), "\n")
  
  if (!"proportions_total" %in% colnames(data)) {
    if ("cr_total_amount" %in% colnames(data) && "sample_size" %in% colnames(data)) {
      cat("Computing proportions_total from cr_total_amount and sample_size\n")
      data$proportions_total <- data$cr_total_amount / data$sample_size
    } else {
      stop("Cannot find or compute proportions_total column. Available columns: ", 
           paste(colnames(data), collapse=", "))
    }
  }
  
  data$proportions_total <- pmin(pmax(data$proportions_total, 0.001), 0.999)
  data$logit_p <- log(data$proportions_total / (1 - data$proportions_total))
  data$var <- 1 / (data$sample_size * data$proportions_total * (1 - data$proportions_total))
  
  return(data)
}

run_meta_regression <- function(data, moderator_formula) {
  tryCatch({
    intercept_model <- rma(yi = logit_p, vi = var, data = data, method = "REML")
    model <- rma(yi = logit_p, vi = var, data = data, method = "REML", 
                mods = as.formula(paste("~", moderator_formula)))
    
    if (model$k < 5) {
      cat("  Warning: Model has fewer than 5 data points (", model$k, ")\n", sep="")
    }
    
    return(list(model = model, intercept_model = intercept_model))
  }, error = function(e) {
    cat("Error fitting model with moderator:", moderator_formula, "\n")
    cat("  Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
}

format_meta_regression_results <- function(model_results, moderator) {
  if (is.null(model_results)) {
    return(data.frame(
      Moderator = moderator,
      QM = NA, QM_p = NA, QE = NA, QE_p = NA, I2 = NA, R2 = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  model <- model_results$model
  QM <- model$QM
  QM_p <- model$QMp
  QE <- model$QE
  QE_p <- model$QEp
  I2 <- model$I2
  
  R2 <- NA
  if (!is.null(model_results$intercept_model)) {
    tau2_base <- model_results$intercept_model$tau2
    tau2_mod <- model$tau2
    R2 <- max(0, (tau2_base - tau2_mod) / tau2_base)  
  }
  
  results <- data.frame(
    Moderator = moderator,
    QM = round(QM, 2),
    QM_p = format.pval(QM_p, digits = 3),
    QE = round(QE, 2),
    QE_p = format.pval(QE_p, digits = 3),
    I2 = paste0(round(I2, 1), "%"),
    R2 = ifelse(is.na(R2), NA, paste0(round(R2 * 100, 1), "%")),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

main <- function() {
  if (!dir.exists("output/meta_regression")) {
    dir.create("output/meta_regression", recursive = TRUE)
  }
  
  possible_files <- c(
    "data/careless_data.csv",
    "output/proportions.xlsx",
    "output/proportions_r.xlsx",
    "output/pooled_proportions.csv",
    "output/pooled_proportions_r.csv",
    "output/random_effects_pooled_proportions.csv",
    "output/random_effects_pooled_proportions_R.csv"
  )
  
  data_file <- NULL
  for (file in possible_files) {
    if (file.exists(file)) {
      data_file <- file
      cat("Using data file:", data_file, "\n")
      break
    }
  }
  
  if (is.null(data_file)) {
    stop("No suitable data file found. Tried: ", paste(possible_files, collapse=", "))
  }
  
  data <- prepare_data(data_file)
  all_columns <- colnames(data)
  
  continuous_moderators <- all_columns[all_columns %in% c("year", "sample_size")]
  categorical_moderators <- all_columns[all_columns %in% c(
    "journal_code", "journal", "sample_source", "sample_method", 
    "sample_platform", "cr_multiple", "cr_sequential"
  )]
  
  cat("Using continuous moderators:", paste(continuous_moderators, collapse=", "), "\n")
  cat("Using categorical moderators:", paste(categorical_moderators, collapse=", "), "\n")
  
  all_results <- data.frame(
    Moderator = character(), QM = numeric(), QM_p = character(),
    QE = numeric(), QE_p = character(), I2 = character(), R2 = character(),
    stringsAsFactors = FALSE
  )
  
  cat("Running meta-regression for continuous moderators...\n")
  for (moderator in continuous_moderators) {
    cat("  - Analyzing moderator:", moderator, "\n")
    model_results <- run_meta_regression(data, moderator)
    results <- format_meta_regression_results(model_results, moderator)
    all_results <- rbind(all_results, results)
  }
  
  cat("Running meta-regression for categorical moderators...\n")
  for (moderator in categorical_moderators) {
    cat("  - Analyzing moderator:", moderator, "\n")
    
    if (length(unique(na.omit(data[[moderator]]))) > 1) {
      model_results <- run_meta_regression(data, paste("factor(", moderator, ")", sep = ""))
      results <- format_meta_regression_results(model_results, moderator)
      all_results <- rbind(all_results, results)
    } else {
      cat("    Skipping", moderator, "- not enough variation (only one level)\n")
    }
  }
  
  write.csv(all_results, "output/meta_regression/meta_regression_results.csv", row.names = FALSE)
  cat("Results saved to output/meta_regression/meta_regression_results.csv\n")
  
  cat("Creating coefficient tables for all moderators...\n")
  coef_results <- data.frame()
  
  for (moderator in unique(all_results$Moderator)) {
    cat("  - Extracting coefficients for moderator:", moderator, "\n")
    
    formula <- ifelse(moderator %in% categorical_moderators, 
                     paste("factor(", moderator, ")", sep = ""), 
                     moderator)
    
    model_results <- run_meta_regression(data, formula)
    
    if (!is.null(model_results)) {
      model <- model_results$model
      coefs <- coef(summary(model))
      coefs_df <- as.data.frame(coefs)
      coefs_df$Moderator <- rownames(coefs_df)
      coefs_df$moderator_group <- moderator
      
      coefs_df$prop_estimate <- exp(coefs_df$estimate) / (1 + exp(coefs_df$estimate))
      
      ci_lower <- exp(coefs_df$estimate - 1.96 * coefs_df$se) / 
                 (1 + exp(coefs_df$estimate - 1.96 * coefs_df$se))
      ci_upper <- exp(coefs_df$estimate + 1.96 * coefs_df$se) / 
                 (1 + exp(coefs_df$estimate + 1.96 * coefs_df$se))
      
      coefs_df$prop_ci <- paste0(round(ci_lower, 3), " - ", round(ci_upper, 3))
      coef_results <- rbind(coef_results, coefs_df)
    }
  }
  
  write.csv(coef_results, "output/meta_regression/meta_regression_results_coefficients.csv", row.names = FALSE)
  cat("All moderator coefficients saved to output/meta_regression/meta_regression_results_coefficients.csv\n")
}

main()