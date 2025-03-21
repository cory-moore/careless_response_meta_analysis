# 11_multilevel_meta.R
#
# Implementation of multilevel meta-analysis for careless responding
# Maintains all methodological decisions while using succinct code
# With enhanced error handling for robust execution

library(tidyverse)
library(metafor)
library(ggplot2)
library(viridis)

# Create output directories
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/r_results/multilevel", showWarnings = FALSE, recursive = TRUE)

# 1. DATA PREPARATION ----

message("Loading data...")
# Safely load data with error handling
safely_read_csv <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(NULL)
  }
  
  tryCatch({
    read_csv(path, show_col_types = FALSE)
  }, error = function(e) {
    warning("Error reading file: ", path, "\n", e$message)
    return(NULL)
  })
}

# Load datasets
first_method_data <- safely_read_csv("data/for_r_meta/first_method_data.csv")
sequential_data <- safely_read_csv("data/for_r_meta/sequential_data.csv")

if (is.null(first_method_data)) {
  stop("First-method dataset is required but could not be loaded.")
}

# Create balanced method groupings while preserving original decisions
prepare_multilevel_data <- function(data) {
  if (is.null(data)) return(NULL)
  
  # Create copy to avoid modifying the original
  result <- data
  
  # Check which columns exist
  has_cr_method <- "cr_method" %in% names(data)
  has_method_code <- "method_code" %in% names(data)
  has_method_type <- "method_type" %in% names(data)
  has_sample_source <- "sample_source" %in% names(data)
  
  # Add method grouping based on available columns
  if (has_cr_method) {
    # Using cr_method for grouping
    result <- result %>%
      mutate(method_group = case_when(
        cr_method %in% c(0, 12, 13) ~ "attention_checks",
        cr_method %in% c(1, 2, 3, 14) ~ "response_patterns",
        cr_method %in% c(4, 5, 15, 16) ~ "statistical_methods",
        cr_method %in% c(6, 7, 8) ~ "self_reported",
        cr_method %in% c(9, 10, 11) ~ "consistency_indices",
        TRUE ~ "other"
      ))
  } else if (has_method_code) {
    # Using method_code for grouping (likely in sequential data)
    result <- result %>%
      mutate(method_group = case_when(
        method_code %in% c(0, 12, 13) ~ "attention_checks",
        method_code %in% c(1, 2, 3, 14) ~ "response_patterns",
        method_code %in% c(4, 5, 15, 16) ~ "statistical_methods",
        method_code %in% c(6, 7, 8) ~ "self_reported",
        method_code %in% c(9, 10, 11) ~ "consistency_indices",
        TRUE ~ "other"
      ))
  } else if (has_method_type) {
    # If method_type is available but not cr_method or method_code
    result <- result %>%
      mutate(method_group = method_type)
  } else {
    warning("No method classification columns found in dataset")
  }
  
  # Add sample grouping if sample_source exists
  if (has_sample_source) {
    result <- result %>%
      mutate(sample_group = case_when(
        sample_source %in% c("student", "student_employee") ~ "student",
        sample_source == "employee_working_adult" ~ "employee",
        TRUE ~ "mixed_other"
      ))
  } else {
    # Default sample group if no proper column exists
    result$sample_group <- "mixed_other"
    message("No sample_source column found, using default 'mixed_other' for all")
  }
  
  return(result)
}

# Process datasets
first_method_ml <- prepare_multilevel_data(first_method_data)
sequential_ml <- prepare_multilevel_data(sequential_data)

# 2. ASSESS CLASS IMBALANCE ----

# Method distribution
method_counts <- first_method_ml %>%
  count(method_group) %>%
  mutate(percentage = n / sum(n) * 100)
print(method_counts)

# Sample distribution
sample_counts <- first_method_ml %>%
  count(sample_group) %>%
  mutate(percentage = n / sum(n) * 100)
print(sample_counts)

# Method × sample cross-tabulation
if (length(unique(first_method_ml$sample_group)) > 1) {
  chi_test <- chisq.test(table(first_method_ml$method_group, first_method_ml$sample_group))
  print(chi_test)
} else {
  message("Cannot perform chi-square test: only one sample group exists")
}

# 3. MULTILEVEL META-ANALYSIS FUNCTION ----

run_multilevel_meta <- function(data, formula, random_structure = ~ 1 | ID, method = "REML") {
  # Check if data is valid
  if (is.null(data) || nrow(data) < 3) {
    warning("Insufficient data for meta-analysis (NULL or <3 rows)")
    return(NULL)
  }
  
  # Map column names - sequential data uses raw_logit_prop while first_method uses logit_prop
  # This handles differences in column naming between datasets
  if ("raw_logit_prop" %in% names(data) && !"logit_prop" %in% names(data)) {
    data$logit_prop <- data$raw_logit_prop
  }
  
  if ("raw_var_logit" %in% names(data) && !"var_logit" %in% names(data)) {
    data$var_logit <- data$raw_var_logit
  }
  
  # Check if required columns exist
  required_cols <- c("logit_prop", "var_logit", "ID")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  # Fit model with error handling
  model <- tryCatch({
    rma.mv(
      yi = logit_prop, V = var_logit, 
      mods = formula, random = random_structure,
      data = data, method = method
    )
  }, error = function(e) {
    warning("Error fitting model: ", e$message)
    return(NULL)
  }, warning = function(w) {
    warning("Warning from model fitting: ", w$message)
    # Continue despite warning
    return(NULL)
  })
    
  # If model failed, return NULL
  if (is.null(model)) return(NULL)
  
  # Extract results
  tryCatch({
    fixed_effects <- data.frame(
      term = rownames(coef(summary(model))),
      estimate_logit = coef(model),
      ci_lb_logit = model$ci.lb,
      ci_ub_logit = model$ci.ub,
      p_value = model$pval,
      # Back-transform to proportion scale
      estimate_prop = transf.ilogit(coef(model)),
      ci_lb_prop = transf.ilogit(model$ci.lb),
      ci_ub_prop = transf.ilogit(model$ci.ub)
    ) %>%
      # Add percentage columns for easier reporting
      mutate(
        detection_rate = estimate_prop * 100,
        ci_lower = ci_lb_prop * 100,
        ci_upper = ci_ub_prop * 100
      )
    
    # Get model stats and variance components
    model_stats <- list(
      model = model,
      fixed_effects = fixed_effects,
      variance_components = model$sigma2,
      k = model$k,
      I2 = round(model$I2 * 100, 1),
      QE = model$QE,
      QEp = model$QEp,
      AIC = model$fit.stats["AIC", "REML"],
      BIC = model$fit.stats["BIC", "REML"]
    )
    
    return(model_stats)
  }, error = function(e) {
    warning("Error extracting results from model: ", e$message)
    return(list(model = model, error = e$message))
  })
}

# 4. POSITION EFFECTS ANALYSIS ----

message("\nRunning position effects analysis...")

# Check if sequential data is valid
has_valid_sequential <- !is.null(sequential_ml) && 
                       nrow(sequential_ml) >= 3 && 
                       "method_position" %in% names(sequential_ml)

if (has_valid_sequential) {
  # Position effects model
  tryCatch({
    position_model <- run_multilevel_meta(
      sequential_ml,
      formula = ~ factor(method_position) - 1
    )
    
    # Extract position effects if model ran successfully
    if (!is.null(position_model) && !is.null(position_model$fixed_effects)) {
      position_effects <- position_model$fixed_effects %>%
        filter(str_detect(term, "position")) %>%
        mutate(
          position = as.integer(str_extract(term, "\\d+")),
          # Calculate position-to-position changes
          prev_rate = lag(detection_rate),
          change = detection_rate - lag(detection_rate),
          percent_change = (detection_rate - lag(detection_rate)) / lag(detection_rate) * 100
        ) %>%
        select(position, detection_rate, ci_lower, ci_upper, change, percent_change) %>%
        arrange(position)
      
      # Create and save position effects plot
      position_plot <- ggplot(position_effects, aes(x = factor(position), y = detection_rate)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
        labs(title = "Careless Responding Detection Rates by Position",
             x = "Screening Position", y = "Detection Rate (%)") +
        theme_minimal()
      
      ggsave("output/figures/position_effects.png", position_plot, 
             width = 8, height = 6, dpi = 300)
      
      # Save position effects results
      write_csv(position_effects, "output/r_results/multilevel/position_effects.csv")
      
      # Position with method control if method_group exists
      if ("method_group" %in% names(sequential_ml)) {
        position_method_model <- run_multilevel_meta(
          sequential_ml,
          formula = ~ factor(method_position) + method_group
        )
      } else {
        position_method_model <- NULL
        message("Skipping position+method model - method_group variable not available")
      }
    } else {
      message("Position model did not produce valid fixed effects")
      position_effects <- NULL
      position_method_model <- NULL
    }
  }, error = function(e) {
    message("Error in position effects analysis: ", e$message)
    position_model <- NULL
    position_effects <- NULL
    position_method_model <- NULL
  })
} else {
  message("Insufficient sequential data for position effects analysis")
  position_model <- NULL
  position_effects <- NULL
  position_method_model <- NULL
}

# 5. METHOD EFFECTIVENESS ANALYSIS ----

message("\nRunning method effectiveness analysis...")

# Method group model
method_model <- tryCatch({
  # Check if method_group exists and has multiple values
  if ("method_group" %in% names(first_method_ml) && 
      length(unique(first_method_ml$method_group)) > 1) {
    
    # Run model (with no intercept for direct group estimates)
    method_result <- run_multilevel_meta(
      first_method_ml,
      formula = ~ 0 + method_group
    )
    
    # Extract and process method effects if successful
    if (!is.null(method_result) && !is.null(method_result$fixed_effects)) {
      method_effects <- method_result$fixed_effects %>%
        filter(str_detect(term, "method_group")) %>%
        mutate(method = str_replace(term, "method_group", "")) %>%
        select(method, detection_rate, ci_lower, ci_upper, p_value) %>%
        arrange(desc(detection_rate))
      
      # Create method effects visualization
      method_plot <- ggplot(method_effects, 
                           aes(x = reorder(method, detection_rate), y = detection_rate)) +
        geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
        coord_flip() +
        labs(title = "Careless Responding Detection Rates by Method Type",
             x = "", y = "Detection Rate (%)") +
        theme_minimal()
      
      ggsave("output/figures/method_effects_ml.png", method_plot, 
             width = 8, height = 6, dpi = 300)
      
      # Save method effects results
      write_csv(method_effects, "output/r_results/multilevel/method_effects.csv")
      
      # Return the model result
      method_result
    } else {
      message("Method model did not produce valid fixed effects")
      NULL
    }
  } else {
    message("Skipping method model - insufficient method group variation")
    NULL
  }
}, error = function(e) {
  message("Error in method effectiveness analysis: ", e$message)
  NULL
})

# 6. SAMPLE CHARACTERISTICS ANALYSIS ----

message("\nRunning sample characteristics analysis...")

# Sample group model
sample_model <- tryCatch({
  # Check if sample_group has multiple values
  if ("sample_group" %in% names(first_method_ml) && 
      length(unique(first_method_ml$sample_group)) > 1) {
    
    # Run model (no intercept for direct estimates)
    sample_result <- run_multilevel_meta(
      first_method_ml,
      formula = ~ 0 + sample_group
    )
    
    # Extract and process sample effects if successful
    if (!is.null(sample_result) && !is.null(sample_result$fixed_effects)) {
      sample_effects <- sample_result$fixed_effects %>%
        filter(str_detect(term, "sample_group")) %>%
        mutate(sample = str_replace(term, "sample_group", "")) %>%
        select(sample, detection_rate, ci_lower, ci_upper, p_value) %>%
        arrange(desc(detection_rate))
      
      # Save sample effects results
      write_csv(sample_effects, "output/r_results/multilevel/sample_effects.csv")
      
      # Return the model result
      sample_result
    } else {
      message("Sample model did not produce valid fixed effects")
      NULL
    }
  } else {
    message("Skipping sample model - insufficient sample group variation")
    NULL
  }
}, error = function(e) {
  message("Error in sample characteristics analysis: ", e$message)
  NULL
})

# 7. VARIANCE COMPONENT ANALYSIS ----

message("\nRunning variance component analysis...")

# Variance component model
variance_model <- tryCatch({
  # Determine which random effects to include based on available variables
  has_multiple_methods <- "method_group" %in% names(first_method_ml) && 
                         length(unique(first_method_ml$method_group)) > 1
  
  has_multiple_samples <- "sample_group" %in% names(first_method_ml) && 
                         length(unique(first_method_ml$sample_group)) > 1
  
  # Build appropriate random effects structure
  if (has_multiple_methods && has_multiple_samples) {
    # Full model with method and sample random effects
    random_structure <- list(~ 1 | method_group, ~ 1 | sample_group, ~ 1 | ID)
  } else if (has_multiple_methods) {
    # Only method random effects
    random_structure <- list(~ 1 | method_group, ~ 1 | ID)
  } else if (has_multiple_samples) {
    # Only sample random effects
    random_structure <- list(~ 1 | sample_group, ~ 1 | ID)
  } else {
    # Just study random effects
    random_structure <- ~ 1 | ID
  }
  
  # Run variance component model
  var_result <- run_multilevel_meta(
    first_method_ml,
    formula = ~ 1,
    random_structure = random_structure
  )
  
  # Process variance components if successful
  if (!is.null(var_result) && !is.null(var_result$variance_components)) {
    # Calculate total variance and proportions
    var_components <- var_result$variance_components
    sampling_var <- mean(first_method_ml$var_logit, na.rm = TRUE)
    total_var <- sum(var_components) + sampling_var
    
    var_props <- c(var_components, sampling_var) / total_var * 100
    names(var_props) <- c(names(var_components), "Sampling Error")
    
    # Create variance components table
    var_components_df <- data.frame(
      source = names(var_props),
      variance = c(var_components, sampling_var),
      percentage = var_props
    )
    
    # Create visualization if at least two components
    if (length(var_props) >= 2) {
      var_plot <- ggplot(var_components_df, 
                        aes(x = "", y = percentage, fill = reorder(source, percentage))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        scale_fill_brewer(palette = "Blues", direction = -1) +
        theme_minimal() +
        labs(title = "Sources of Variance in Careless Responding Rates",
             fill = "Variance Source") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank())
      
      ggsave("output/figures/variance_components.png", var_plot, 
             width = 7, height = 6, dpi = 300)
    }
    
    # Save variance components results
    write_csv(var_components_df, "output/r_results/multilevel/variance_components.csv")
    
    # Return variance model
    var_result
  } else {
    message("Variance component model did not produce valid results")
    NULL
  }
}, error = function(e) {
  message("Error in variance component analysis: ", e$message)
  NULL
})

# 8. MODEL COMPARISON ----

message("\nGenerating model comparison...")

# Create a list of valid models
valid_models <- list()

# Only add models that are valid and have all required fields
if (!is.null(method_model) && !is.null(method_model$k) && !is.null(method_model$AIC)) {
  valid_models[["Method"]] <- method_model
}

if (!is.null(position_model) && !is.null(position_model$k) && !is.null(position_model$AIC)) {
  valid_models[["Position"]] <- position_model
}

if (!is.null(position_method_model) && !is.null(position_method_model$k) && !is.null(position_method_model$AIC)) {
  valid_models[["Position+Method"]] <- position_method_model
}

if (!is.null(sample_model) && !is.null(sample_model$k) && !is.null(sample_model$AIC)) {
  valid_models[["Sample"]] <- sample_model
}

if (!is.null(variance_model) && !is.null(variance_model$k) && !is.null(variance_model$AIC)) {
  valid_models[["Variance Components"]] <- variance_model
}

# Only proceed if we have at least one valid model
if (length(valid_models) > 0) {
  message("  Creating comparison with ", length(valid_models), " valid models")
  
  # Create data frame with one row per model
  model_rows <- list()
  
  for (model_name in names(valid_models)) {
    model <- valid_models[[model_name]]
    model_rows[[model_name]] <- data.frame(
      Model = model_name,
      k = ifelse(!is.null(model$k), model$k, NA),
      AIC = ifelse(!is.null(model$AIC), model$AIC, NA),
      BIC = ifelse(!is.null(model$BIC), model$BIC, NA),
      QE = ifelse(!is.null(model$QE), model$QE, NA),
      I2 = ifelse(!is.null(model$I2), model$I2, NA)
    )
  }
  
  # Combine all rows
  comparison_df <- do.call(rbind, model_rows)
  
  # Save model comparison
  write_csv(comparison_df, "output/r_results/multilevel/model_comparison.csv")
  message("  Saved model comparison with ", nrow(comparison_df), " models")
} else {
  message("  No valid models available for comparison")
}

# 9. SENSITIVITY ANALYSIS ----

message("\nRunning sensitivity comparison with standard meta-regression...")

# Run standard meta-regression if method model ran successfully
if (!is.null(method_model) && "method_group" %in% names(first_method_ml)) {
  tryCatch({
    std_method_reg <- rma(
      yi = logit_prop,
      vi = var_logit,
      mods = ~ 0 + method_group,
      data = first_method_ml,
      method = "DL"
    )
    
    # Create method comparison table if successful
    if (exists("method_effects")) {
      method_comparison <- data.frame(
        method = method_effects$method,
        ml_estimate = method_effects$detection_rate,
        ml_ci_lower = method_effects$ci_lower,
        ml_ci_upper = method_effects$ci_upper,
        std_estimate = transf.ilogit(coef(std_method_reg)) * 100,
        std_ci_lower = transf.ilogit(std_method_reg$ci.lb) * 100,
        std_ci_upper = transf.ilogit(std_method_reg$ci.ub) * 100,
        difference = method_effects$detection_rate - (transf.ilogit(coef(std_method_reg)) * 100)
      )
      
      # Save method comparison
      write_csv(method_comparison, "output/r_results/multilevel/method_comparison.csv")
    }
  }, error = function(e) {
    message("Error in sensitivity analysis: ", e$message)
  })
}

message("\nMultilevel meta-analysis complete. Results saved to output/r_results/multilevel/")