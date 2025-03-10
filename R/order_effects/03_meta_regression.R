###############################################################################
# R script for focused meta-regression of position effects in careless responding
#
# This script performs meta-regression analyses on methods and method types
# with sufficient data across multiple positions to test whether position in a
# screening sequence affects careless responding detection rates.
#
# OVERVIEW AND RATIONALE:
#
# Order effects in careless responding detection are a critical methodological 
# challenge that can significantly impact data quality assessment. When researchers
# apply multiple detection methods sequentially, each method is applied to a 
# progressively "cleaner" sample (as careless responders have already been 
# removed by earlier methods). This creates two key challenges for comparing 
# detection rates across positions:
#
# 1. The sample composition changes with each screening step
# 2. The detection rates can appear artificially lower for later methods when
#    using the original sample size as denominator
#
# This script implements a sophisticated meta-regression approach that allows us to:
#
# - Directly test whether position significantly moderates detection rates
# - Analyze both original and adjusted proportions to understand denominator effects
# - Quantify how much of the variation in detection rates is explained by position
# - Control for between-study heterogeneity using mixed-effects models
# - Compare position effects across different detection methods and method types
#
# The analysis focuses on methods and method types identified through exploratory
# analysis as having sufficient data across multiple positions. This ensures 
# statistical validity while making optimal use of the available evidence.
#
# For each viable method and method type, we:
# 1. Filter data to extract only relevant observations
# 2. Apply logit transformation to handle bounded proportions
# 3. Fit meta-regression models with position as categorical moderator
# 4. Calculate statistical measures (QM, QE, R²) to assess position effects
# 5. Back-transform results for interpretability
#
# The findings will help researchers understand how detection rates vary by
# position and optimize their screening protocols accordingly.
###############################################################################

library(dplyr)
library(metafor)
library(readr)

load_position_dataset <- function() {
  file_path <- "output/order_effects/data/position_regression.csv"
  
  if (!file.exists(file_path)) {
    stop("Position regression dataset not found. Run 01_create_datasets.R first.")
  }
  
  return(read_csv(file_path, show_col_types = FALSE))
}

inspect_dataset <- function(data) {
  cat("\nDATASET INSPECTION:\n")
  cat("\nColumns in the dataset:", paste(colnames(data), collapse=", "), "\n")
  
  cat("\nDistribution of method_code values:\n")
  method_table <- table(data$method_code)
  for (code in names(method_table)) {
    cat("  Method code", code, ":", method_table[code], "observations\n")
  }
  
  cat("\nDistribution of method_type values:\n")
  type_table <- table(data$method_type)
  for (type in names(type_table)) {
    cat("  Method type", type, ":", type_table[type], "observations\n")
  }
  
  method_1_data <- data[data$method_code == 1, ]
  cat("\nFiltering test - Method 1 (response time):\n")
  cat("  Should have", method_table["1"], "observations\n")
  cat("  Actually has", nrow(method_1_data), "observations\n")
  
  response_time_data <- data[data$method_type == "response_time", ]
  cat("\nFiltering test - Method type 'response_time':\n")
  cat("  Should have", type_table["response_time"], "observations\n")
  cat("  Actually has", nrow(response_time_data), "observations\n")
}

run_method_position_regression <- function(data, target_methods) {
  cat("Running method-specific position meta-regression...\n")
  
  results <- data.frame()
  method_coefs <- data.frame()
  
  for (i in 1:nrow(target_methods)) {
    method_code <- target_methods$method_code[i]
    method_name <- target_methods$method_name[i]
    
    cat("  Analyzing method", method_code, ":", method_name, "\n")
    method_data <- data[data$method_code == method_code, ]
    cat("    Found", nrow(method_data), "observations for this method\n")
    
    if (nrow(method_data) == 0) {
      cat("    No data found for method", method_code, "\n")
      next
    }
    
    position_counts <- table(method_data$method_position)
    cat("    Position counts for this method:\n")
    for (pos in names(position_counts)) {
      cat("      Position", pos, ":", position_counts[pos], "observations\n")
    }
    
    valid_positions <- as.numeric(names(position_counts[position_counts >= 3]))
    
    if (length(valid_positions) < 2) {
      cat("    Not enough positions with sufficient data (need at least 2 positions with 3+ observations)\n")
      next
    }
    
    filtered_data <- method_data[method_data$method_position %in% valid_positions, ]
    cat("    Selected", nrow(filtered_data), "observations across", length(valid_positions), "positions\n")
    
    tryCatch({
      filtered_data <- filtered_data[is.finite(filtered_data$var_proportion), ]
      
      if (nrow(filtered_data) < 5) {
        cat("    Not enough observations with valid variance for meta-regression\n")
        next
      }
      
      model_original <- rma(yi = logit_proportion, vi = var_proportion, 
                          mods = ~ factor(method_position) - 1, 
                          data = filtered_data, method = "REML")
      
      null_original <- rma(yi = logit_proportion, vi = var_proportion,
                         data = filtered_data, method = "REML")
      
      r_squared <- max(0, (null_original$tau2 - model_original$tau2) / null_original$tau2)
      
      results <- rbind(results, data.frame(
        Analysis_Type = "Method",
        Method_Code = method_code,
        Method_Name = method_name,
        Method_Type = unique(filtered_data$method_type)[1],
        Proportion_Type = "Original",
        QM = model_original$QM,
        QM_df = model_original$p - 1,
        QM_p = model_original$QMp,
        QE = model_original$QE,
        QE_df = model_original$k - model_original$p,
        QE_p = model_original$QEp,
        R_squared = r_squared,
        I_squared = model_original$I2,
        stringsAsFactors = FALSE
      ))
      
      coefs_original <- as.data.frame(summary(model_original)$coefficients)
      coefs_original$position <- as.integer(gsub("factor\\(method_position\\)([0-9]+)", "\\1", 
                                              rownames(coefs_original)))
      coefs_original$Method_Code <- method_code
      coefs_original$Method_Name <- method_name
      coefs_original$Method_Type <- unique(filtered_data$method_type)[1]
      coefs_original$Proportion_Type <- "Original"
      coefs_original$Analysis_Type <- "Method"
      
      coefs_original$proportion <- exp(coefs_original$estimate) / (1 + exp(coefs_original$estimate))
      coefs_original$proportion_lower <- exp(coefs_original$estimate - 1.96 * coefs_original$se) / 
                                       (1 + exp(coefs_original$estimate - 1.96 * coefs_original$se))
      coefs_original$proportion_upper <- exp(coefs_original$estimate + 1.96 * coefs_original$se) / 
                                       (1 + exp(coefs_original$estimate + 1.96 * coefs_original$se))
      
      method_coefs <- rbind(method_coefs, coefs_original)
      cat("    Successfully completed original proportion meta-regression\n")
    }, error = function(e) {
      cat("    Error in position meta-regression for method", method_code, 
          "(original proportions):", conditionMessage(e), "\n")
    })
    
    tryCatch({
      filtered_data <- filtered_data[is.finite(filtered_data$var_adjusted), ]
      
      if (nrow(filtered_data) < 5) {
        cat("    Not enough observations with valid variance for meta-regression\n")
        next
      }
      
      model_adjusted <- rma(yi = logit_adjusted, vi = var_adjusted, 
                          mods = ~ factor(method_position) - 1, 
                          data = filtered_data, method = "REML")
      
      null_adjusted <- rma(yi = logit_adjusted, vi = var_adjusted,
                         data = filtered_data, method = "REML")
      
      r_squared <- max(0, (null_adjusted$tau2 - model_adjusted$tau2) / null_adjusted$tau2)
      
      results <- rbind(results, data.frame(
        Analysis_Type = "Method",
        Method_Code = method_code,
        Method_Name = method_name,
        Method_Type = unique(filtered_data$method_type)[1],
        Proportion_Type = "Adjusted",
        QM = model_adjusted$QM,
        QM_df = model_adjusted$p - 1,
        QM_p = model_adjusted$QMp,
        QE = model_adjusted$QE,
        QE_df = model_adjusted$k - model_adjusted$p,
        QE_p = model_adjusted$QEp,
        R_squared = r_squared,
        I_squared = model_adjusted$I2,
        stringsAsFactors = FALSE
      ))
      
      coefs_adjusted <- as.data.frame(summary(model_adjusted)$coefficients)
      coefs_adjusted$position <- as.integer(gsub("factor\\(method_position\\)([0-9]+)", "\\1", 
                                              rownames(coefs_adjusted)))
      coefs_adjusted$Method_Code <- method_code
      coefs_adjusted$Method_Name <- method_name
      coefs_adjusted$Method_Type <- unique(filtered_data$method_type)[1]
      coefs_adjusted$Proportion_Type <- "Adjusted"
      coefs_adjusted$Analysis_Type <- "Method"
      
      coefs_adjusted$proportion <- exp(coefs_adjusted$estimate) / (1 + exp(coefs_adjusted$estimate))
      coefs_adjusted$proportion_lower <- exp(coefs_adjusted$estimate - 1.96 * coefs_adjusted$se) / 
                                       (1 + exp(coefs_adjusted$estimate - 1.96 * coefs_adjusted$se))
      coefs_adjusted$proportion_upper <- exp(coefs_adjusted$estimate + 1.96 * coefs_adjusted$se) / 
                                       (1 + exp(coefs_adjusted$estimate + 1.96 * coefs_adjusted$se))
      
      method_coefs <- rbind(method_coefs, coefs_adjusted)
      cat("    Successfully completed adjusted proportion meta-regression\n")
    }, error = function(e) {
      cat("    Error in position meta-regression for method", method_code, 
          "(adjusted proportions):", conditionMessage(e), "\n")
    })
  }
  
  if (nrow(method_coefs) > 0) {
    write_csv(method_coefs, "output/order_effects/results/method_position_coefs.csv")
  }
  
  return(results)
}

run_method_type_position_regression <- function(data, target_types) {
  cat("Running method type position meta-regression...\n")
  
  results <- data.frame()
  type_coefs <- data.frame()
  
  for (i in 1:nrow(target_types)) {
    method_type <- target_types$method_type[i]
    
    cat("  Analyzing method type:", method_type, "\n")
    type_data <- data[data$method_type == method_type, ]
    cat("    Found", nrow(type_data), "observations for this method type\n")
    
    if (nrow(type_data) == 0) {
      cat("    No data found for method type", method_type, "\n")
      next
    }
    
    position_counts <- table(type_data$method_position)
    cat("    Position counts for this method type:\n")
    for (pos in names(position_counts)) {
      cat("      Position", pos, ":", position_counts[pos], "observations\n")
    }
    
    valid_positions <- as.numeric(names(position_counts[position_counts >= 3]))
    
    if (length(valid_positions) < 2) {
      cat("    Not enough positions with sufficient data (need at least 2 positions with 3+ observations)\n")
      next
    }
    
    filtered_data <- type_data[type_data$method_position %in% valid_positions, ]
    cat("    Selected", nrow(filtered_data), "observations across", length(valid_positions), "positions\n")
    
    tryCatch({
      filtered_data <- filtered_data[is.finite(filtered_data$var_proportion), ]
      
      if (nrow(filtered_data) < 5) {
        cat("    Not enough observations with valid variance for meta-regression\n")
        next
      }
      
      model_original <- rma(yi = logit_proportion, vi = var_proportion, 
                          mods = ~ factor(method_position) - 1, 
                          data = filtered_data, method = "REML")
      
      null_original <- rma(yi = logit_proportion, vi = var_proportion,
                         data = filtered_data, method = "REML")
      
      r_squared <- max(0, (null_original$tau2 - model_original$tau2) / null_original$tau2)
      
      results <- rbind(results, data.frame(
        Analysis_Type = "Method_Type",
        Method_Type = method_type,
        Proportion_Type = "Original",
        QM = model_original$QM,
        QM_df = model_original$p - 1,
        QM_p = model_original$QMp,
        QE = model_original$QE,
        QE_df = model_original$k - model_original$p,
        QE_p = model_original$QEp,
        R_squared = r_squared,
        I_squared = model_original$I2,
        stringsAsFactors = FALSE
      ))
      
      coefs_original <- as.data.frame(summary(model_original)$coefficients)
      coefs_original$position <- as.integer(gsub("factor\\(method_position\\)([0-9]+)", "\\1", 
                                              rownames(coefs_original)))
      coefs_original$Method_Type <- method_type
      coefs_original$Proportion_Type <- "Original"
      coefs_original$Analysis_Type <- "Method_Type"
      
      coefs_original$proportion <- exp(coefs_original$estimate) / (1 + exp(coefs_original$estimate))
      coefs_original$proportion_lower <- exp(coefs_original$estimate - 1.96 * coefs_original$se) / 
                                       (1 + exp(coefs_original$estimate - 1.96 * coefs_original$se))
      coefs_original$proportion_upper <- exp(coefs_original$estimate + 1.96 * coefs_original$se) / 
                                       (1 + exp(coefs_original$estimate + 1.96 * coefs_original$se))
      
      type_coefs <- rbind(type_coefs, coefs_original)
      cat("    Successfully completed original proportion meta-regression\n")
    }, error = function(e) {
      cat("    Error in position meta-regression for method type", method_type, 
          "(original proportions):", conditionMessage(e), "\n")
    })
    
    tryCatch({
      filtered_data <- filtered_data[is.finite(filtered_data$var_adjusted), ]
      
      if (nrow(filtered_data) < 5) {
        cat("    Not enough observations with valid variance for meta-regression\n")
        next
      }
      
      model_adjusted <- rma(yi = logit_adjusted, vi = var_adjusted, 
                          mods = ~ factor(method_position) - 1, 
                          data = filtered_data, method = "REML")
      
      null_adjusted <- rma(yi = logit_adjusted, vi = var_adjusted,
                         data = filtered_data, method = "REML")
      
      r_squared <- max(0, (null_adjusted$tau2 - model_adjusted$tau2) / null_adjusted$tau2)
      
      results <- rbind(results, data.frame(
        Analysis_Type = "Method_Type",
        Method_Type = method_type,
        Proportion_Type = "Adjusted",
        QM = model_adjusted$QM,
        QM_df = model_adjusted$p - 1,
        QM_p = model_adjusted$QMp,
        QE = model_adjusted$QE,
        QE_df = model_adjusted$k - model_adjusted$p,
        QE_p = model_adjusted$QEp,
        R_squared = r_squared,
        I_squared = model_adjusted$I2,
        stringsAsFactors = FALSE
      ))
      
      coefs_adjusted <- as.data.frame(summary(model_adjusted)$coefficients)
      coefs_adjusted$position <- as.integer(gsub("factor\\(method_position\\)([0-9]+)", "\\1", 
                                              rownames(coefs_adjusted)))
      coefs_adjusted$Method_Type <- method_type
      coefs_adjusted$Proportion_Type <- "Adjusted"
      coefs_adjusted$Analysis_Type <- "Method_Type"
      
      coefs_adjusted$proportion <- exp(coefs_adjusted$estimate) / (1 + exp(coefs_adjusted$estimate))
      coefs_adjusted$proportion_lower <- exp(coefs_adjusted$estimate - 1.96 * coefs_adjusted$se) / 
                                       (1 + exp(coefs_adjusted$estimate - 1.96 * coefs_adjusted$se))
      coefs_adjusted$proportion_upper <- exp(coefs_adjusted$estimate + 1.96 * coefs_adjusted$se) / 
                                       (1 + exp(coefs_adjusted$estimate + 1.96 * coefs_adjusted$se))
      
      type_coefs <- rbind(type_coefs, coefs_adjusted)
      cat("    Successfully completed adjusted proportion meta-regression\n")
    }, error = function(e) {
      cat("    Error in position meta-regression for method type", method_type, 
          "(adjusted proportions):", conditionMessage(e), "\n")
    })
  }
  
  if (nrow(type_coefs) > 0) {
    write_csv(type_coefs, "output/order_effects/results/method_type_position_coefs.csv")
  }
  
  return(results)
}

main <- function() {
  if (!dir.exists("output/order_effects/results")) {
    dir.create("output/order_effects/results", recursive = TRUE)
  }
  
  cat("Loading position regression dataset...\n")
  pos_data <- load_position_dataset()
  
  inspect_dataset(pos_data)
  
  pos_data <- pos_data %>%
    mutate(
      clean_proportion = pmin(pmax(proportion, 0.00001), 0.99999),
      clean_adjusted = pmin(pmax(adjusted_proportion, 0.00001), 0.99999),
      logit_proportion = log(clean_proportion / (1 - clean_proportion)),
      logit_adjusted = log(clean_adjusted / (1 - clean_adjusted)),
      var_proportion = 1 / (sample_size * clean_proportion * (1 - clean_proportion)),
      var_adjusted = 1 / (remaining_sample * clean_adjusted * (1 - clean_adjusted))
    )
  
  target_methods <- data.frame(
    method_code = c(1, 4, 5, 14),
    method_name = c("response time", "mahalanobis D", "univariate outlier", "unspecified longstring"),
    stringsAsFactors = FALSE
  )
  
  target_types <- data.frame(
    method_type = c("consistency_indices", "outlier_analysis", "response_pattern", "response_time"),
    stringsAsFactors = FALSE
  )
  
  method_results <- run_method_position_regression(pos_data, target_methods)
  type_results <- run_method_type_position_regression(pos_data, target_types)
  
  all_results <- bind_rows(method_results, type_results)
  
  if (nrow(all_results) > 0) {
    all_results <- all_results %>%
      mutate(
        QM_p_formatted = sprintf("%.3f", QM_p),
        QM_p_formatted = ifelse(QM_p < 0.001, "<0.001", QM_p_formatted),
        QE_p_formatted = sprintf("%.3f", QE_p),
        QE_p_formatted = ifelse(QE_p < 0.001, "<0.001", QE_p_formatted),
        R_squared_formatted = sprintf("%.1f%%", R_squared * 100),
        I_squared_formatted = sprintf("%.1f%%", I_squared)
      )
    
    write_csv(all_results, "output/order_effects/results/position_meta_regression_results.csv")
    
    cat("\nMeta-regression analysis complete. Results saved to output/order_effects/results/\n")
    cat("Method-specific results:", nrow(method_results), "rows\n")
    cat("Method type results:", nrow(type_results), "rows\n")
  } else {
    cat("\nWARNING: No valid results were generated from meta-regression analysis.\n")
  }
  
  return(all_results)
}

main()