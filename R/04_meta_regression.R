# 06_meta_regression.R
#
# This script implements a focused approach to meta-regression following best 
# practices for model building in meta-analysis. It examines moderators of careless
# responding rates following a structured analytical strategy:
# 1. Univariate testing of all potential moderators
# 2. Selective bivariate testing of theoretically meaningful combinations
# 3. Parsimonious multivariate modeling with top predictors
# 4. Temporal trend analysis

library(tidyverse)
library(metafor)
library(glue)

# Create output directory
dir.create("output/r_results/meta_regression", recursive = TRUE, showWarnings = FALSE)

# Helper Functions --------------------------------------------------------

calculate_safe_aic <- function(model) {
  if(is.null(model$ll) || is.na(model$ll)) {
    return(NA_real_)  # Return NA if log-likelihood is not available
  } else {
    return(-2 * model$ll + 2 * (length(model$b) + 1))
  }
}

# Ensure output files always exist, even for failed analyses
ensure_output <- function(data, filename, default_message = "Insufficient data for analysis") {
  if(is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
    write_csv(data.frame(
      Message = default_message,
      Details = "See logs for more information",
      Timestamp = Sys.time()
    ), filename)
    message(glue("  Created placeholder file: {default_message}"))
    return(FALSE)
  }
  
  write_csv(data, filename)
  return(TRUE)
}

# Load data with appropriate error handling
read_meta_data <- function(path = "data/for_r_meta/first_method_data.csv") {
  if (!file.exists(path)) {
    message(glue("Error: Data file not found: {path}"))
    return(NULL)
  }
  
  read_csv(path, show_col_types = FALSE)
}

# Run meta-regression for a given moderator variable
run_meta_regression <- function(data, moderator, is_categorical = TRUE) {
  # Skip if variable doesn't exist or has insufficient data
  if(!moderator %in% names(data) || sum(!is.na(data[[moderator]])) < 10) {
    return(NULL)
  }
  
  # Create formula
  formula <- if(is_categorical) {
    as.formula(paste("logit_prop ~", paste0("factor(", moderator, ")")))
  } else {
    as.formula(paste("logit_prop ~", moderator))
  }
  
  # Run null model and moderator model
  null_model <- tryCatch({
    rma(logit_prop, vi = var_logit, data = data, method = "REML")
  }, error = function(e) NULL)
  
  if(is.null(null_model)) return(NULL)
  
  mod_model <- tryCatch({
    rma(formula, vi = var_logit, data = data, method = "REML")
  }, error = function(e) NULL)
  
  # After successful model fit
  if(is.null(mod_model)) {
    message(glue("  ** Cannot proceed with {moderator} due to moderator model failure"))
    return(NULL)
  }
  
  # Calculate proportion of heterogeneity explained
  r_squared <- max(0, (null_model$tau2 - mod_model$tau2) / null_model$tau2)
  
  # Create scalar values for EVERY field to ensure consistent row counts
  n_pred <- ifelse(is.null(mod_model$b), 0, max(0, length(mod_model$b) - 1))
  k_val <- ifelse(is.null(mod_model$k), NA_integer_, mod_model$k)
  qm_val <- ifelse(is.null(mod_model$QM), NA_real_, mod_model$QM)
  qm_df_val <- ifelse(is.null(mod_model$p), NA_integer_, max(0, mod_model$p - 1))
  qm_p_val <- ifelse(is.null(mod_model$QMp), NA_real_, mod_model$QMp)
  tau2_val <- ifelse(is.null(mod_model$tau2), NA_real_, mod_model$tau2)
  i2_val <- ifelse(is.null(mod_model$I2), NA_real_, mod_model$I2)
  ll_val <- ifelse(is.null(mod_model$ll), NA_real_, mod_model$ll)
  aic_val <- ifelse(is.null(ll_val) || is.na(ll_val), 
                   NA_real_, 
                   -2 * ll_val + 2 * (n_pred + 1))
  is_sig <- ifelse(is.null(qm_p_val) || is.na(qm_p_val), 
                  FALSE, 
                  qm_p_val < 0.05)
  
  # Now create the data frame with guaranteed scalar values
  tryCatch({
    summary_stats <- data.frame(
      moderator = as.character(moderator),
      n_predictors = as.integer(n_pred),
      k = as.integer(k_val),
      QM = as.numeric(qm_val),
      QM_df = as.integer(qm_df_val),
      QM_p = as.numeric(qm_p_val),
      R2 = as.numeric(r_squared),
      tau2 = as.numeric(tau2_val),
      I2 = as.numeric(i2_val),
      AIC = as.numeric(aic_val),
      is_significant = as.logical(is_sig),
      stringsAsFactors = FALSE
    )
    
    message(glue("  ** Successfully created summary_stats data frame: {nrow(summary_stats)} rows"))
    
  }, error = function(e) {
    message(glue("  ** Error creating summary_stats: {e$message}"))
    summary_stats <- NULL
  })
  
  if(is.null(summary_stats)) {
    message(glue("  ** Could not create valid summary statistics for {moderator}"))
    return(NULL)
  }
  
  message(glue("  {moderator}: QM = {round(qm_val, 2)}, p = {format.pval(qm_p_val)}, R² = {round(r_squared*100, 1)}%"))
  
  # Extract coefficients with similar error handling
  coefficients <- tryCatch({
    coef_df <- coef(summary(mod_model))
    if(is.null(coef_df)) {
      message("  ** No coefficients available")
      NULL
    } else {
      coef_result <- as.data.frame(coef_df) %>%
        rownames_to_column("term") %>%
        mutate(
          moderator = moderator,
          sig = case_when(
            pval < 0.001 ~ "***",
            pval < 0.01 ~ "**",
            pval < 0.05 ~ "*", 
            pval < 0.1 ~ ".",
            TRUE ~ ""
          ),
          term = str_remove(term, "factor\\("),
          term = str_remove(term, "\\)")
        )
      coef_result
    }
  }, error = function(e) {
    message(glue("  ** Error extracting coefficients: {e$message}"))
    NULL
  })
  
  return(list(
    summary = summary_stats,
    coefficients = coefficients,
    model = mod_model,
    null_model = null_model
  ))
}

# Run all univariate meta-regressions
run_all_univariate <- function(data, categorical_vars, continuous_vars) {
  # Create a data frame to collect results
  result_df <- data.frame(
    moderator = character(),
    n_predictors = integer(),
    k = integer(),
    QM = numeric(),
    QM_df = integer(),
    QM_p = numeric(),
    R2 = numeric(),
    tau2 = numeric(),
    I2 = numeric(),
    AIC = numeric(),
    is_significant = logical(),
    stringsAsFactors = FALSE
  )
  
  # Process categorical variables
  message("  Processing categorical variables...")
  for(var in categorical_vars) {
    message(glue("  Analyzing {var}..."))
    model_result <- run_meta_regression(data, var, TRUE)
    if(!is.null(model_result)) {
      # Save coefficients
      ensure_output(
        model_result$coefficients, 
        glue("output/r_results/meta_regression/coefs_{var}.csv"),
        glue("No valid coefficients for {var}")
      )
      
      # Add summary to results data frame
      if(!is.null(model_result$summary) && is.data.frame(model_result$summary) && nrow(model_result$summary) > 0) {
        message(glue("  ** Adding results for {var}: rows before = {nrow(result_df)}"))
        result_df <- rbind(result_df, model_result$summary)
        message(glue("  ** Rows after adding {var} = {nrow(result_df)}"))
      } else {
        message(glue("  ** WARNING: No valid summary for {var}"))
      }
    }
  }
  
  # Similar change for continuous variables section
  
  # Debugging - print the structure to verify data collection
  message(glue("  Collected {nrow(result_df)} univariate results"))
  
  # Return sorted results if we have any
  if(nrow(result_df) > 0) {
    return(result_df[order(result_df$QM_p),])
  } else {
    return(result_df)
  }
}

# Focused bivariate testing with promising predictors
run_all_bivariate <- function(data, predictors, univariate_results, categorical_vars) {
  # Quick validation of inputs
  if(is.null(univariate_results) || nrow(univariate_results) == 0 || !"QM_p" %in% names(univariate_results)) {
    message("  No valid univariate results provided")
    return(data.frame())
  }
  
  # Select promising predictors (p < 0.20)
  promising_predictors <- univariate_results$moderator[univariate_results$QM_p < 0.20]
  
  if(length(promising_predictors) == 0) {
    message("  No promising predictors (p < 0.20) identified")
    return(data.frame())
  }
  
  message(glue("  Found {length(promising_predictors)} promising predictors with p < 0.20: {paste(promising_predictors, collapse=', ')}"))
  
  # Generate pairs with at least one promising predictor
  predictor_pairs <- combn(predictors, 2, simplify = FALSE) %>%
    keep(~.x[1] %in% promising_predictors || .x[2] %in% promising_predictors)
  
  # Filter out method_type + cr_method combination
  predictor_pairs <- predictor_pairs %>%
    keep(~!all(c("method_type", "cr_method") %in% .x))
  
  if(length(predictor_pairs) == 0) {
    message("  No valid predictor pairs identified")
    return(data.frame())
  }
  
  message(glue("  Testing {length(predictor_pairs)} bivariate combinations with promising predictors"))
  
  # Run bivariate model for each pair
  result_list <- list()
  
  for(pair in predictor_pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    message(glue("  Analyzing {var1} + {var2}..."))
    
    # Skip if insufficient complete cases
    complete_cases <- sum(complete.cases(data[, c(var1, var2)]))
    if(complete_cases < 10) {
      message(glue("  Insufficient complete cases ({complete_cases}) for {var1} + {var2}"))
      next
    }
    
    # Determine if variables are categorical
    var1_cat <- var1 %in% categorical_vars
    var2_cat <- var2 %in% categorical_vars
    
    # Create formula
    formula_str <- paste(
      "logit_prop ~", 
      paste0(
        ifelse(var1_cat, paste0("factor(", var1, ")"), var1), 
        " + ",
        ifelse(var2_cat, paste0("factor(", var2, ")"), var2)
      )
    )
    
    # Run model with error handling
    tryCatch({
      # Run null model
      null_model <- rma(logit_prop, vi = var_logit, data = data, method = "REML")
      
      # Run bivariate model
      model <- suppressWarnings(rma(as.formula(formula_str), vi = var_logit, data = data, method = "REML"))
      
      # Check if model contains any predictors after redundancy removal
      if(length(model$b) <= 1) {
        message(glue("  ** All predictors were dropped from the model"))
        next
      }
      
      # Calculate R-squared
      r_squared <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
      
      # Structure coefficients data frame carefully
      tryCatch({
        # Get the coefficient summary
        coef_summary <- coef(summary(model))
        
        if(is.null(coef_summary) || nrow(coef_summary) == 0) {
          message("  ** No coefficients available for this model")
          next
        }
        
        # Safe coefficient extraction
        coefficients <- data.frame(
          term = rownames(coef_summary),
          estimate = coef_summary[,"estimate"],
          se = coef_summary[,"se"],
          zval = coef_summary[,"zval"],
          pval = coef_summary[,"pval"],
          model = rep(paste(var1, "+", var2), nrow(coef_summary)),
          moderator = rep(paste(var1, "+", var2), nrow(coef_summary)),
          sig = rep("", nrow(coef_summary))
        )
        
        # Add significance indicators
        coefficients$sig <- ifelse(coefficients$pval < 0.001, "***",
                                ifelse(coefficients$pval < 0.01, "**", 
                                      ifelse(coefficients$pval < 0.05, "*",
                                            ifelse(coefficients$pval < 0.1, ".", ""))))
        
        # Clean up term names if needed
        coefficients$term <- gsub("factor\\(", "", coefficients$term)
        coefficients$term <- gsub("\\)", "", coefficients$term)
        
        # Save coefficients
        ensure_output(
          coefficients, 
          glue("output/r_results/meta_regression/bivar_coefs_{var1}_{var2}.csv"),
          glue("No valid coefficients for {var1} + {var2}")
        )
      }, error = function(e) {
        message(glue("  ** Error processing coefficients: {e$message}"))
        # Continue with model summary even if coefficient processing fails
      })
      
      # Create model summary using individual scalar values for data.frame creation
      tryCatch({
        model_entry <- data.frame(
          predictors = paste(var1, "+", var2),
          n_predictors = length(model$b) - 1,
          k = model$k,
          QM = model$QM,
          QM_df = model$p - 1,
          QM_p = model$QMp,
          R2 = r_squared,
          tau2 = model$tau2,
          I2 = model$I2,
          AIC = calculate_safe_aic(model),
          is_significant = model$QMp < 0.05,
          stringsAsFactors = FALSE
        )
        
        result_list[[paste(var1, var2, sep="_")]] <- model_entry
        
        message(glue("  ** Successful bivariate model: R² = {round(r_squared*100, 1)}%, p = {format.pval(model$QMp)}"))
      }, error = function(e) {
        message(glue("  ** Error creating model entry: {e$message}"))
      })
      
    }, error = function(e) {
      message(glue("  ** Error in bivariate model: {e$message}"))
    })
  }
  
  # Combine and return results
  if(length(result_list) > 0) {
    result_df <- do.call(rbind, result_list)
    # Sort by R2 descending
    return(result_df[order(-result_df$R2),])
  } else {
    message("  ** No valid bivariate models found")
    return(data.frame())
  }
}

# Run multivariate model with top predictors
run_multivariate_model <- function(data, univariate_results, categorical_vars, max_predictors = 5) {
  # Basic validation
  if(is.null(univariate_results) || nrow(univariate_results) < 2) {
    message("  Insufficient univariate results for multivariate model")
    return(NULL)
  }
  
  # Select promising predictors (p < 0.20)
  promising_predictors <- univariate_results$moderator[univariate_results$QM_p < 0.20]
  
  # Need at least 2 predictors for multivariate model
  if(length(promising_predictors) < 2) {
    message("  Insufficient promising predictors for multivariate model")
    return(NULL)
  }
  
  message(glue("  Found {length(promising_predictors)} promising predictors for multivariate model: {paste(promising_predictors, collapse=', ')}"))
  
  # Known redundant variables - remove redundant ones
  if("method_type" %in% promising_predictors && "cr_method" %in% promising_predictors) {
    # Keep the one with lower p-value
    method_type_p <- univariate_results$QM_p[univariate_results$moderator == "method_type"]
    cr_method_p <- univariate_results$QM_p[univariate_results$moderator == "cr_method"]
    
    if(method_type_p <= cr_method_p) {
      message("  Removing cr_method due to redundancy with method_type")
      promising_predictors <- promising_predictors[promising_predictors != "cr_method"]
    } else {
      message("  Removing method_type due to redundancy with cr_method")
      promising_predictors <- promising_predictors[promising_predictors != "method_type"]
    }
  }
  
  # Limit to max_predictors, but ensure we have at least 2
  promising_predictors <- promising_predictors[1:min(length(promising_predictors), max_predictors)]
  
  # Check for sufficient complete cases
  complete_cases <- sum(complete.cases(data[, promising_predictors]))
  if(complete_cases < 10) {
    message(glue("  Insufficient complete cases ({complete_cases}) for multivariate model"))
    return(NULL)
  }
  
  message(glue("  Running multivariate model with promising predictors: {paste(promising_predictors, collapse=', ')}"))
  
  # Create formula
  formula_parts <- sapply(promising_predictors, function(x) {
    if(x %in% categorical_vars) paste0("factor(", x, ")") else x
  })
  
  formula_str <- paste("logit_prop ~", paste(formula_parts, collapse = " + "))
  
  # Run model with error handling
  tryCatch({
    # Run null model
    null_model <- rma(logit_prop, vi = var_logit, data = data, method = "REML")
    
    # Run multivariate model with warnings suppressed
    model <- suppressWarnings(rma(as.formula(formula_str), vi = var_logit, data = data, method = "REML"))
    
    # Check if model has any predictors after redundancy removal
    if(length(model$b) <= 1) {
      message("  ** All predictors were dropped from the model")
      return(NULL)
    }
    
    # Calculate R-squared
    r_squared <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
    
    # Extract coefficients
    coefficients <- coef(summary(model)) %>%
      as.data.frame() %>%
      rownames_to_column("term") %>%
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
    
    # Save coefficients
    ensure_output(
      coefficients, 
      "output/r_results/meta_regression/03_multivariate_coefficients.csv",
      "No valid coefficients for multivariate model"
    )
    
    # Return model summary
    data.frame(
      predictors = paste(promising_predictors, collapse = " + "),
      n_predictors = length(model$b) - 1,
      k = model$k,
      QM = model$QM,
      QM_df = model$p - 1,
      QM_p = model$QMp,
      R2 = r_squared,
      tau2 = model$tau2,
      I2 = model$I2,
      AIC = calculate_safe_aic(model),
      is_significant = model$QMp < 0.05,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message(glue("  ** Error in multivariate model: {e$message}"))
    NULL
  })
}

# Run temporal trend analysis
analyze_temporal_trend <- function(data) {
  if(!"year" %in% names(data) || sum(!is.na(data$year)) < 10) {
    return(NULL)
  }
  
  # Check year variation
  year_range <- range(data$year, na.rm = TRUE)
  if(diff(year_range) < 3) {
    message(glue("Insufficient year variation: studies only span {diff(year_range)} years"))
    return(NULL)
  }
  
  # Center year to reduce collinearity
  data <- data %>%
    mutate(year_centered = year - mean(year, na.rm = TRUE))
  
  # Run linear trend model with robust error handling
  tryCatch({
    # Run null model
    null_model <- rma(logit_prop, vi = var_logit, data = data, method = "REML")
    
    # Run linear model
    linear_model <- rma(logit_prop ~ year_centered, vi = var_logit, data = data, method = "REML")
    
    # Calculate R-squared
    r_squared <- max(0, (null_model$tau2 - linear_model$tau2) / null_model$tau2)
    
    message(glue("Linear trend model: β = {round(linear_model$b[2], 4)}, p = {format.pval(linear_model$pval[2])}, R² = {round(r_squared*100, 1)}%"))
    
    # Extract coefficients
    linear_coefs <- coef(summary(linear_model)) %>%
      as.data.frame() %>%
      rownames_to_column("term") %>%
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
    
    # Save coefficients
    ensure_output(
      linear_coefs, 
      "output/r_results/meta_regression/04_linear_trend_coefs.csv",
      "No valid coefficients for linear trend model"
    )
    
    # Return results
    tibble(
      model = "Year (Linear)",
      n_predictors = length(linear_model$b) - 1,
      k = linear_model$k,
      QM = linear_model$QM,
      QM_p = linear_model$QMp,
      R2 = r_squared,
      tau2 = linear_model$tau2,
      I2 = linear_model$I2,
      AIC = calculate_safe_aic(linear_model),
      is_significant = linear_model$QMp < 0.05
    )
  }, error = function(e) {
    message(glue("Error in temporal trend analysis: {e$message}"))
    NULL
  })
}

# Create model comparison table with improved robustness
create_model_comparison <- function(univariate_results, bivariate_results, 
                                   multivariate_results, temporal_results) {
  # Create an empty result dataframe
  result <- tibble()
  
  # Add univariate results
  if(!is.null(univariate_results) && nrow(univariate_results) > 0) {
    # Keep only top 5 univariate models for comparison
    uni_top <- univariate_results %>%
      arrange(QM_p) %>%
      head(5)
    
    result <- bind_rows(result, 
      uni_top %>%
        mutate(
          model_type = "Univariate",
          model_name = moderator,
          n_parameters = n_predictors + 1
        ) %>%
        select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
    )
  }
  
  # Add bivariate results
  if(!is.null(bivariate_results) && nrow(bivariate_results) > 0) {
    result <- bind_rows(result, 
      bivariate_results %>%
        mutate(
          model_type = "Bivariate",
          model_name = predictors,
          n_parameters = n_predictors + 1
        ) %>%
        select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
    )
  }
  
  # Add multivariate results
  if(!is.null(multivariate_results) && nrow(multivariate_results) > 0) {
    result <- bind_rows(result, 
      multivariate_results %>%
        mutate(
          model_type = "Multivariate",
          model_name = predictors,
          n_parameters = n_predictors + 1
        ) %>%
        select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
    )
  }
  
  # Add temporal results
  if(!is.null(temporal_results) && nrow(temporal_results) > 0) {
    result <- bind_rows(result, 
      temporal_results %>%
        mutate(
          model_type = "Temporal",
          model_name = model,
          n_parameters = 2  # Intercept + slope
        ) %>%
        select(model_type, model_name, n_parameters, k, QM, QM_p, R2, AIC, is_significant)
    )
  }
  
  # Sort and add AIC weights if we have any results
  if(nrow(result) > 0) {
    result <- result %>%
      arrange(desc(R2)) %>% # Sort by R² instead of AIC to avoid name conflicts
      mutate(
        # Calculate AIC weights explicitly
        delta_AIC = .data[["AIC"]] - min(.data[["AIC"]]),
        AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
      ) %>%
      select(-delta_AIC)
  }
  
  return(result)
}

# Create effect size summary table with improved robustness
create_effect_size_summary <- function(univariate_results, bivariate_results, 
                                      multivariate_results, temporal_results) {
  # Initialize empty result
  result <- tibble()
  
  # Add univariate R² values
  if(!is.null(univariate_results) && nrow(univariate_results) > 0) {
    result <- bind_rows(result,
      univariate_results %>%
        transmute(
          model_type = "Univariate",
          variable = moderator,
          R2 = R2,
          QM_p = QM_p,
          R2_formatted = sprintf("%.1f%%", R2 * 100),
          significance = ifelse(QM_p < 0.05, "Significant", "Non-significant")
        )
    )
  }
  
  # Add bivariate R² values
  if(!is.null(bivariate_results) && nrow(bivariate_results) > 0) {
    result <- bind_rows(result,
      bivariate_results %>%
        transmute(
          model_type = "Bivariate",
          variable = predictors,
          R2 = R2,
          QM_p = QM_p,
          R2_formatted = sprintf("%.1f%%", R2 * 100),
          significance = ifelse(QM_p < 0.05, "Significant", "Non-significant")
        )
    )
  }
  
  # Add multivariate R² values
  if(!is.null(multivariate_results) && nrow(multivariate_results) > 0) {
    result <- bind_rows(result,
      multivariate_results %>%
        transmute(
          model_type = "Multivariate",
          variable = predictors,
          R2 = R2,
          QM_p = QM_p,
          R2_formatted = sprintf("%.1f%%", R2 * 100),
          significance = ifelse(QM_p < 0.05, "Significant", "Non-significant")
        )
    )
  }
  
  # Add temporal trend R² values
  if(!is.null(temporal_results) && nrow(temporal_results) > 0) {
    result <- bind_rows(result,
      temporal_results %>%
        transmute(
          model_type = "Temporal",
          variable = model,
          R2 = R2,
          QM_p = QM_p,
          R2_formatted = sprintf("%.1f%%", R2 * 100),
          significance = ifelse(QM_p < 0.05, "Significant", "Non-significant")
        )
    )
  }
  
  # Sort by R² if we have any results
  if(nrow(result) > 0) {
    result <- result %>% arrange(desc(R2))
  }
  
  return(result)
}

bivariate_test <- function(data) {
  # Focus on one specific model combination 
  var1 <- "method_type"
  var2 <- "year"
  
  # Try fitting the model
  model <- tryCatch({
    rma(logit_prop ~ factor(method_type) + year, vi = var_logit, data = data, method = "REML")
  }, error = function(e) {
    message("Model fitting error: ", e$message)
    return(NULL)
  })
  
  # If model fitting succeeded, examine its structure
  if(!is.null(model)) {
    # Print key components that might be causing issues
    message("Model components:")
    message("- class: ", paste(class(model), collapse=", "))
    message("- length of b: ", length(model$b))
    message("- p value: ", model$p)
    message("- QM value: ", model$QM)
    message("- QMp value: ", model$QMp)
    
    # Try individually creating each component that goes into the data frame
    message("\nTesting individual components:")
    n_pred <- try(length(model$b) - 1, silent = TRUE)
    message("- n_predictors: ", if(inherits(n_pred, "try-error")) "ERROR" else n_pred)
    
    qm_df <- try(model$p - 1, silent = TRUE)
    message("- QM_df: ", if(inherits(qm_df, "try-error")) "ERROR" else qm_df)
    
    aic_val <- try(-2 * model$ll + 2 * (length(model$b) + 1), silent = TRUE)
    message("- AIC: ", if(inherits(aic_val, "try-error")) "ERROR" else aic_val)
    
    # Try creating data frame with individual values
    message("\nTry creating data frame with scalars:")
    result <- try({
      data.frame(
        predictors = "method_type + year",
        n_predictors = as.integer(length(model$b) - 1),
        k = as.integer(model$k),
        QM = as.numeric(model$QM),
        QM_df = as.integer(model$p - 1),
        QM_p = as.numeric(model$QMp),
        R2 = as.numeric(0.5),  # placeholder
        tau2 = as.numeric(model$tau2),
        I2 = as.numeric(model$I2),
        AIC = as.numeric(calculate_safe_aic(model)),
        is_significant = as.logical(model$QMp < 0.05)
      )
      "SUCCESS"
    }, silent = TRUE)
    message("- Data frame creation: ", if(inherits(result, "try-error")) paste("ERROR:", result) else result)
  }
}


# Main Execution ---------------------------------------------------------

main <- function() {
  message("\nRunning meta-regression analysis...")
  
  # Load data
  data <- read_meta_data()

  message("\nRunning diagnostic test...")
  bivariate_test(data)
  
  # Create placeholder files if insufficient data
  if(is.null(data) || nrow(data) < 10) {
    message("Insufficient data for meta-regression (minimum 10 studies required)")
    
    # Create all necessary output files with appropriate messages
    ensure_output(tibble(), "output/r_results/meta_regression/01_univariate_results.csv", 
                 "Insufficient data for meta-regression")
    ensure_output(tibble(), "output/r_results/meta_regression/02_bivariate_results.csv", 
                 "Insufficient data for meta-regression")
    ensure_output(tibble(), "output/r_results/meta_regression/03_multivariate_results.csv", 
                 "Insufficient data for meta-regression")
    ensure_output(tibble(), "output/r_results/meta_regression/04_temporal_trends.csv", 
                 "Insufficient data for meta-regression")
    ensure_output(tibble(), "output/r_results/meta_regression/05_model_comparison.csv", 
                 "Insufficient data for meta-regression")
    ensure_output(tibble(), "output/r_results/meta_regression/06_effect_size_summary.csv", 
                 "Insufficient data for meta-regression")
    
    return(invisible(NULL))
  }
  
  # Define potential predictor variables
  categorical_vars <- c(
    "sample_source", "sample_recruitment", "sample_platform", 
    "sample_method", "journal", "sample_level", "sample_incentive", 
    "sample_country", "design_method", "design_location", "method_type", "cr_method", "method_timing"
  )
  
  continuous_vars <- c("year", "sample_size", "sample_age", "total_items")
  
  # Filter to available predictors in dataset
  categorical_vars <- categorical_vars[categorical_vars %in% names(data)]
  continuous_vars <- continuous_vars[continuous_vars %in% names(data)]
  
  message("\nUNIVARIATE PREDICTORS ANALYSIS")
  message("===========================")
  message("Analyzing predictors of careless responding rates:")
  message("  Categorical predictors: ", paste(categorical_vars, collapse = ", "))
  message("  Continuous predictors: ", paste(continuous_vars, collapse = ", "))
  
  # 1. Run univariate meta-regressions
  all_univariate_results <- run_all_univariate(data, categorical_vars, continuous_vars)
  
  # Add debugging to check univariate results
  message("\nDEBUG - Univariate results check:")
  if(!is.null(all_univariate_results) && nrow(all_univariate_results) > 0) {
    message(glue("  Found {nrow(all_univariate_results)} univariate models"))
    message(glue("  Models with p < 0.20: {sum(all_univariate_results$QM_p < 0.20, na.rm = TRUE)}"))
    
    # Show the significant models
    significant_models <- all_univariate_results %>% 
      filter(QM_p < 0.20) %>% 
      arrange(QM_p)
    
    if(nrow(significant_models) > 0) {
      message("  Significant models (p < 0.20):")
      for(i in 1:nrow(significant_models)) {
        message(glue("    {significant_models$moderator[i]}: QM_p = {format.pval(significant_models$QM_p[i])}, R² = {round(significant_models$R2[i]*100, 1)}%"))
      }
    }
  } else {
    message("  No valid univariate results found")
  }
  
  # Save univariate results - using actual results instead of empty tibble
  ensure_output(
    all_univariate_results, 
    "output/r_results/meta_regression/01_univariate_results.csv",
    "No significant univariate moderators found"
  )
  
  # Combine all coefficient files
  all_coefs <- map_dfr(
    list.files("output/r_results/meta_regression", pattern = "^coefs_.*\\.csv$", full.names = TRUE),
    ~read_csv(.x, show_col_types = FALSE)
  )
  
  if(nrow(all_coefs) > 0) {
    write_csv(all_coefs, "output/r_results/meta_regression/01_all_univariate_coefficients.csv")
  }
  
  # 2. Run focused bivariate meta-regressions
  message("\nBIVARIATE META-REGRESSION ANALYSIS")
  message("================================")
  
  # Get all predictors with at least 10 non-missing values
  valid_predictors <- c(categorical_vars, continuous_vars)[
    map_lgl(c(categorical_vars, continuous_vars), ~sum(!is.na(data[[.x]])) >= 10)
  ]
  
  # Use focused approach with promising predictors
  bivariate_results <- run_all_bivariate(
    data = data, 
    predictors = valid_predictors, 
    univariate_results = all_univariate_results,
    categorical_vars = categorical_vars
  )
  
  # Save bivariate results
  ensure_output(
    bivariate_results, 
    "output/r_results/meta_regression/02_bivariate_results.csv",
    "No valid bivariate meta-regression models"
  )
  
  # Combine all bivariate coefficient files
  bivar_coefs <- map_dfr(
    list.files("output/r_results/meta_regression", pattern = "^bivar_coefs_.*\\.csv$", full.names = TRUE),
    ~read_csv(.x, show_col_types = FALSE)
  )
  
  if(nrow(bivar_coefs) > 0) {
    write_csv(bivar_coefs, "output/r_results/meta_regression/02_all_bivariate_coefficients.csv")
  }
  
  # 3. Run multivariate model with best predictors
  message("\nMULTIVARIATE META-REGRESSION")
  message("==========================")
  
  # Use improved multivariate approach
  multivariate_results <- run_multivariate_model(
    data = data, 
    univariate_results = all_univariate_results,
    categorical_vars = categorical_vars,
    max_predictors = 5
  )
  
  # Save multivariate results
  ensure_output(
    multivariate_results, 
    "output/r_results/meta_regression/03_multivariate_results.csv",
    "No valid multivariate meta-regression model"
  )
  
  # 4. Run temporal trend analysis
  message("\nTEMPORAL TREND ANALYSIS")
  message("======================")
  
  temporal_results <- analyze_temporal_trend(data)
  
  # Save temporal results
  ensure_output(
    temporal_results, 
    "output/r_results/meta_regression/04_temporal_trends.csv",
    "No valid temporal trend model"
  )
  
  # 5. Create model comparison
  message("\nMODEL COMPARISON")
  message("================")
  
  model_comparison <- create_model_comparison(
    all_univariate_results,
    bivariate_results,
    multivariate_results,
    temporal_results
  )
  
  # Save model comparison
  ensure_output(
    model_comparison, 
    "output/r_results/meta_regression/05_model_comparison.csv",
    "No valid models for comparison"
  )
  
  # 6. Create effect size summary
  message("\nEFFECT SIZE SUMMARY")
  message("==================")
  
  effect_summary <- create_effect_size_summary(
    all_univariate_results,
    bivariate_results,
    multivariate_results,
    temporal_results
  )
  
  # Save effect size summary
  ensure_output(
    effect_summary, 
    "output/r_results/meta_regression/06_effect_size_summary.csv",
    "No valid effect sizes to summarize"
  )
  
  message("\nMeta-regression analysis complete! Results saved to output/r_results/meta_regression/")
}

# Run main function
main()