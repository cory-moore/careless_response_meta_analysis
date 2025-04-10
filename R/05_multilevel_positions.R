###############################################################################
# 07_multilevel_positional.R
#
# This script implements comprehensive multilevel meta-analysis for careless 
# responding data with special focus on positional effects in sequential 
# screening. It addresses both the hierarchical data structure and the 
# methodological challenge of position effects.
#
# Key analyses include:
# 1. Method effectiveness - comparing detection approaches while controlling for dependencies
# 2. Sample characteristics - analyzing differences across sample types
# 3. Positional effects - examining how screening position impacts effectiveness
# 4. Method x Position interactions - assessing method-specific sensitivity to position
# 5. Sequence patterns - analyzing common screening sequences and transitions
# 6. Variance decomposition - partitioning heterogeneity into components
#
# Both raw proportions (based on original sample) and adjusted proportions 
# (based on remaining sample) are analyzed to provide complementary insights.
###############################################################################

library(tidyverse)
library(metafor)
library(ggplot2)
library(patchwork)  # For combining plots
library(glue)

# Create output directories
dir.create("output/r_results/multilevel", recursive = TRUE, showWarnings = FALSE)
dir.create("output/r_results/positional", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/multilevel", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/positional", recursive = TRUE, showWarnings = FALSE)

# Helper Functions --------------------------------------------------------

#' Load data with error handling
read_data <- function(path) {
  if (!file.exists(path)) {
    stop("Data file not found: ", path)
  }
  
  data <- read_csv(path, show_col_types = FALSE)
  
  # Display basic dataset info
  message(glue("Loaded {basename(path)}: {nrow(data)} rows, {n_distinct(data$ID)} studies"))
  
  return(data)
}

#' Run multilevel meta-analysis with appropriate error handling
run_multilevel_meta <- function(data, formula, random_structure = ~ 1 | ID, 
                               method = "REML", yi_var = "yi", vi_var = "vi") {
  # Map column names if needed
  if (yi_var != "yi" && yi_var %in% names(data)) {
    data$yi <- data[[yi_var]]
  }
  
  if (vi_var != "vi" && vi_var %in% names(data)) {
    data$vi <- data[[vi_var]]
  }
  
  # Check for required columns
  required <- c("yi", "vi", "ID")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Fit model with error handling
  model <- tryCatch({
    rma.mv(yi = yi, V = vi, mods = formula, random = random_structure,
           data = data, method = method)
  }, error = function(e) {
    message("Error fitting model: ", e$message)
    return(NULL)
  })
  
  if (is.null(model)) return(NULL)
  
  # Extract results to standard format
  results <- list(
    model = model,
    fixed_effects = as.data.frame(coef(summary(model))) %>%
      rownames_to_column("term") %>%
      mutate(
        # Create proportion-scale transformations
        estimate_prop = transf.ilogit(estimate),
        ci_lb_prop = transf.ilogit(ci.lb),
        ci_ub_prop = transf.ilogit(ci.ub),
        # Add significance indicators
        significance = case_when(
          pval < 0.001 ~ "***",
          pval < 0.01 ~ "**",
          pval < 0.05 ~ "*",
          pval < 0.1 ~ ".",
          TRUE ~ ""
        )
      ),
    k = model$k,
    variance_components = model$sigma2,
    AIC = model$fit.stats["AIC", "REML"],
    BIC = model$fit.stats["BIC", "REML"],
    QE = model$QE,
    QEp = model$QEp
  )
  
  return(results)
}

#' Extract clean terms from model output
clean_term_names <- function(fixed_effects, term_pattern, replacement = "") {
  fixed_effects %>%
    filter(str_detect(term, term_pattern)) %>%
    mutate(
      clean_term = str_replace(term, term_pattern, replacement),
      # Convert proportions to percentages for easier interpretation
      detection_rate = estimate_prop * 100,
      ci_lower = ci_lb_prop * 100,
      ci_upper = ci_ub_prop * 100
    )
}

#' Create forest-style plot of effects
create_effect_plot <- function(effect_data, x_col, y_col, error_min, error_max,
                              title, subtitle = NULL, x_lab, y_lab, 
                              text_col = NULL, text_label = NULL,
                              color_col = NULL) {
  p <- ggplot(effect_data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(size = 3, aes(color = if(!is.null(color_col)) .data[[color_col]] else NULL)) +
    geom_errorbar(aes(ymin = .data[[error_min]], 
                      ymax = .data[[error_max]],
                      color = if(!is.null(color_col)) .data[[color_col]] else NULL), 
                  width = 0.2) +
    labs(title = title,
         subtitle = subtitle,
         x = x_lab,
         y = y_lab) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Add text annotation if specified
  if (!is.null(text_col) && !is.null(text_label)) {
    p <- p + geom_text(aes(label = .data[[text_col]]), 
                       nudge_x = 0.3, hjust = 0)
  }
  
  # Add color scale if needed
  if (!is.null(color_col)) {
    p <- p + scale_color_brewer(palette = "Set1")
  }
  
  return(p)
}

#' Prepare method and sample groupings
prepare_categorical_groupings <- function(data) {
  # Add method grouping based on available columns
  if ("cr_method" %in% names(data)) {
    data <- data %>%
      mutate(method_group = case_when(
        cr_method == 1 ~ "response_time",
        cr_method %in% c(4, 5) ~ "outlier_analysis",
        cr_method %in% c(0, 12, 13) ~ "attention_check_items",
        cr_method %in% c(9, 10, 11, 15, 16) ~ "consistency_indices",
        cr_method %in% c(2, 3, 14) ~ "response_pattern",
        cr_method %in% c(6, 7, 8) ~ "self_reported",
        TRUE ~ "other"
      ))
  } else if ("method_code" %in% names(data)) {
    data <- data %>%
      mutate(method_group = case_when(
        method_code == 1 ~ "response_time",
        method_code %in% c(4, 5) ~ "outlier_analysis",
        method_code %in% c(0, 12, 13) ~ "attention_check_items",
        method_code %in% c(9, 10, 11, 15, 16) ~ "consistency_indices",
        method_code %in% c(2, 3, 14) ~ "response_pattern",
        method_code %in% c(6, 7, 8) ~ "self_reported",
        TRUE ~ "other"
      ))
  } else if ("method_type" %in% names(data)) {
    data <- data %>%
      mutate(method_group = method_type)
  }
  
  # Add sample grouping if sample_source exists
  if ("sample_source" %in% names(data)) {
    data <- data %>%
      mutate(sample_group = case_when(
        sample_source %in% c(0, 2) ~ "student",  # 0=student, 2=student_employee
        sample_source == 1 ~ "employee",         # 1=employee_working_adult
        TRUE ~ "mixed_other"
      ))
  }
  
  return(data)
}

#' Save model results to disk with proper formatting
save_model_results <- function(results, file_prefix, readable_labels = TRUE) {
  if (is.null(results) || !("fixed_effects" %in% names(results))) {
    message("No valid results to save for ", file_prefix)
    return(NULL)
  }
  
  # Save fixed effects with percentage formatting
  fixed_effects <- results$fixed_effects %>%
    mutate(
      detection_rate = estimate_prop * 100,
      ci_lower = ci_lb_prop * 100,
      ci_upper = ci_ub_prop * 100
    )
  
  # Make term names more readable if requested
  if (readable_labels) {
    fixed_effects <- fixed_effects %>%
      mutate(term = str_remove_all(term, "factor\\(|\\)")) %>%
      mutate(term = str_replace(term, "method_group", "")) %>%
      mutate(term = str_replace(term, "sample_group", "")) %>%
      mutate(term = str_replace(term, "method_position", "Position "))
  }
  
  # Save to CSV
  write_csv(fixed_effects, paste0("output/r_results/multilevel/", file_prefix, "_fixed_effects.csv"))
  
  # Create model summary 
  model_summary <- tibble(
    k = results$k,
    AIC = results$AIC,
    BIC = results$BIC,
    QE = results$QE,
    QEp = results$QEp
  )
  
  # Add variance components if available
  if (!is.null(results$variance_components)) {
    variance_df <- as.data.frame(t(results$variance_components)) %>%
      set_names(paste0("var_", names(results$variance_components)))
    
    model_summary <- bind_cols(model_summary, variance_df)
  }
  
  # Save model summary
  write_csv(model_summary, paste0("output/r_results/multilevel/", file_prefix, "_summary.csv"))
  
  return(list(
    fixed_effects = fixed_effects,
    model_summary = model_summary
  ))
}

# Data Preparation --------------------------------------------------------

# Load main datasets
first_method_data <- read_data("data/for_r_meta/first_method_data.csv")
sequential_data <- read_data("data/for_r_meta/sequential_data.csv")

# Prepare data with method and sample groupings
first_method_ml <- prepare_categorical_groupings(first_method_data)
sequential_ml <- prepare_categorical_groupings(sequential_data)

# Examine distribution of key variables
method_distribution <- first_method_ml %>%
  count(method_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

message("\nMethod distribution:")
method_distribution %>%
  mutate(percent = sprintf("%.1f%%", percent)) %>%
  as.data.frame() %>%
  print()

sample_distribution <- first_method_ml %>%
  count(sample_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

message("\nSample distribution:")
sample_distribution %>%
  mutate(percent = sprintf("%.1f%%", percent)) %>%
  as.data.frame() %>%
  print()

# Examine position distribution in sequential data
position_distribution <- sequential_ml %>%
  count(method_position) %>%
  arrange(method_position) %>%
  mutate(percent = n / sum(n) * 100)

message("\nPosition distribution in sequential data:")
position_distribution %>%
  mutate(percent = sprintf("%.1f%%", percent)) %>%
  as.data.frame() %>%
  print()

# Examine sequence lengths (number of methods per study)
sequence_lengths <- sequential_ml %>%
  group_by(ID) %>%
  summarize(sequence_length = n(), .groups = "drop") %>%
  count(sequence_length) %>%
  arrange(sequence_length) %>%
  mutate(percent = n / sum(n) * 100)

message("\nSequence lengths:")
sequence_lengths %>%
  mutate(percent = sprintf("%.1f%%", percent)) %>%
  as.data.frame() %>%
  print()

# 1. Method Effectiveness Analysis ----------------------------------------

message("\nRUNNING METHOD EFFECTIVENESS ANALYSIS")
message("===================================")

# Method model (no-intercept model for direct method estimates)
method_model <- run_multilevel_meta(
  first_method_ml,
  formula = ~ 0 + method_group,
  random_structure = ~ 1 | ID,
  yi_var = "logit_prop",
  vi_var = "var_logit"
)

# Save and display method results
method_results <- save_model_results(method_model, "method_effectiveness")

if (!is.null(method_results)) {
  message("\nMethod Effectiveness Results:")
  method_results$fixed_effects %>%
    select(term, detection_rate, ci_lower, ci_upper, significance) %>%
    arrange(desc(detection_rate)) %>%
    as.data.frame() %>%
    print()
  
  # Create method effectiveness plot
  method_plot <- create_effect_plot(
    method_results$fixed_effects,
    x_col = "detection_rate",
    y_col = "term",
    error_min = "ci_lower",
    error_max = "ci_upper",
    title = "Careless Responding Rates by Method Type",
    subtitle = "Multilevel Analysis with Study Random Effects",
    x_lab = "Detection Rate (%)",
    y_lab = "Method Type",
    text_col = "significance",
    text_label = "Significance"
  )
  
  # Save plot
  ggsave("output/figures/multilevel/method_effectiveness.png", 
         method_plot, width = 9, height = 6, dpi = 300)
}

# 1b. Method Timing Analysis (A Priori vs Post Hoc) -----------------------

message("\nRUNNING METHOD TIMING ANALYSIS")
message("===========================")

# Method timing model
method_timing_model <- run_multilevel_meta(
  first_method_ml,
  formula = ~ 0 + method_timing,
  random_structure = ~ 1 | ID,
  yi_var = "logit_prop",
  vi_var = "var_logit"
)

# Save and display method timing results
method_timing_results <- save_model_results(method_timing_model, "method_timing_effectiveness")

if (!is.null(method_timing_results)) {
  message("\nMethod Timing Results:")
  method_timing_results$fixed_effects %>%
    select(term, detection_rate, ci_lower, ci_upper, significance) %>%
    arrange(desc(detection_rate)) %>%
    as.data.frame() %>%
    print()
  
  # Create method timing plot
  timing_plot <- create_effect_plot(
    method_timing_results$fixed_effects,
    x_col = "detection_rate",
    y_col = "term",
    error_min = "ci_lower",
    error_max = "ci_upper",
    title = "Careless Responding Rates by Method Timing",
    subtitle = "A Priori (embedded in survey) vs Post Hoc (after data collection)",
    x_lab = "Detection Rate (%)",
    y_lab = "Method Timing",
    text_col = "significance",
    text_label = "Significance"
  )
  
  # Save plot
  ggsave("output/figures/multilevel/method_timing_effectiveness.png", 
         timing_plot, width = 9, height = 5, dpi = 300)
}

# 2. Sample Characteristics Analysis --------------------------------------

message("\nRUNNING SAMPLE CHARACTERISTICS ANALYSIS")
message("=====================================")

# Sample model (no-intercept for direct estimates)
sample_model <- run_multilevel_meta(
  first_method_ml,
  formula = ~ 0 + sample_group,
  random_structure = ~ 1 | ID,
  yi_var = "logit_prop",
  vi_var = "var_logit"
)

# Save and display sample results
sample_results <- save_model_results(sample_model, "sample_characteristics")

if (!is.null(sample_results)) {
  message("\nSample Characteristics Results:")
  sample_results$fixed_effects %>%
    select(term, detection_rate, ci_lower, ci_upper, significance) %>%
    as.data.frame() %>%
    print()
  
  # Create sample characteristics plot
  sample_plot <- create_effect_plot(
    sample_results$fixed_effects,
    x_col = "detection_rate",
    y_col = "term",
    error_min = "ci_lower",
    error_max = "ci_upper",
    title = "Careless Responding Rates by Sample Type",
    x_lab = "Detection Rate (%)",
    y_lab = "Sample Type",
    text_col = "significance",
    text_label = "Significance"
  )
  
  # Save plot
  ggsave("output/figures/multilevel/sample_characteristics.png", 
         sample_plot, width = 8, height = 5, dpi = 300)
}

# 3. Variance Components Analysis ----------------------------------------

message("\nRUNNING VARIANCE COMPONENTS ANALYSIS")
message("=================================")

# Simplify the variance components model with direct approach
variance_model <- tryCatch({
  # Start with a simple model
  if ("method_group" %in% names(first_method_ml) && 
      "sample_group" %in% names(first_method_ml)) {
    # If we have both method and sample groups
    message("Running variance components model with method and sample random effects")
    rma.mv(
      yi = logit_prop, 
      V = var_logit,
      data = first_method_ml,
      random = list(~ 1 | method_group, ~ 1 | sample_group, ~ 1 | ID),
      method = "REML"
    )
  } else if ("method_group" %in% names(first_method_ml)) {
    # If we only have method group
    message("Running variance components model with method random effects")
    rma.mv(
      yi = logit_prop, 
      V = var_logit,
      data = first_method_ml,
      random = list(~ 1 | method_group, ~ 1 | ID),
      method = "REML"
    )
  } else if ("sample_group" %in% names(first_method_ml)) {
    # If we only have sample group
    message("Running variance components model with sample random effects")
    rma.mv(
      yi = logit_prop, 
      V = var_logit,
      data = first_method_ml,
      random = list(~ 1 | sample_group, ~ 1 | ID),
      method = "REML"
    )
  } else {
    # If we have neither, just use study random effect
    message("Running variance components model with only study random effects")
    rma.mv(
      yi = logit_prop, 
      V = var_logit,
      data = first_method_ml,
      random = ~ 1 | ID,
      method = "REML"
    )
  }
}, error = function(e) {
  message("Error in variance component model: ", e$message)
  NULL
})

if (!is.null(variance_model)) {
  # Calculate variance proportions
  var_components <- variance_model$sigma2
  sampling_var <- mean(first_method_ml$var_logit, na.rm = TRUE)
  total_var <- sum(var_components) + sampling_var
  
  var_proportions <- c(var_components, sampling_var) / total_var * 100
  names(var_proportions) <- c(names(var_components), "Sampling Error")
  
  # Create variance components table
  variance_df <- tibble(
    source = names(var_proportions),
    variance = c(var_components, sampling_var),
    percentage = var_proportions
  )
  
  # Save variance components
  write_csv(variance_df, "output/r_results/multilevel/variance_components.csv")
  
  message("\nVariance Components:")
  variance_df %>%
    mutate(percentage = sprintf("%.1f%%", percentage)) %>%
    as.data.frame() %>%
    print()
  
  # Create variance components visualization
  variance_plot <- ggplot(variance_df, 
                         aes(x = "", y = percentage, fill = reorder(source, percentage))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    theme_minimal() +
    labs(title = "Sources of Variance in Careless Responding Rates",
         fill = "Variance Source") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  # Save plot
  ggsave("output/figures/multilevel/variance_components.png", 
         variance_plot, width = 7, height = 6, dpi = 300)
}

# 4. Positional Effects Analysis -----------------------------------------

message("\nRUNNING POSITIONAL EFFECTS ANALYSIS")
message("================================")

# Check if sequential data is available and has position information
if (nrow(sequential_ml) > 0 && "method_position" %in% names(sequential_ml)) {
  message("Analyzing positional effects across screening sequences...")
  
  # For raw proportions
  raw_position_model <- run_multilevel_meta(
    sequential_ml,
    formula = ~ factor(method_position) - 1,  # No intercept for direct position estimates
    random_structure = ~ 1 | ID,
    yi_var = "raw_logit_prop",
    vi_var = "raw_var_logit"
  )
  
  # For adjusted proportions (based on remaining sample size)
  adj_position_model <- run_multilevel_meta(
    sequential_ml,
    formula = ~ factor(method_position) - 1,  # No intercept for direct position estimates
    random_structure = ~ 1 | ID,
    yi_var = "adj_logit_prop",
    vi_var = "adj_var_logit"
  )
  
  # Save position effects results
  raw_position_results <- save_model_results(raw_position_model, "raw_position_effects")
  adj_position_results <- save_model_results(adj_position_model, "adjusted_position_effects")
  
  # Combine raw and adjusted results for comparison
  if (!is.null(raw_position_results) && !is.null(adj_position_results)) {
    # Extract position-specific results
    raw_positions <- raw_position_results$fixed_effects %>%
      mutate(proportion_type = "Raw") %>%
      # Extract position number from term
      mutate(position = as.numeric(str_extract(term, "\\d+")))
    
    adj_positions <- adj_position_results$fixed_effects %>%
      mutate(proportion_type = "Adjusted") %>%
      # Extract position number from term
      mutate(position = as.numeric(str_extract(term, "\\d+")))
    
    # Combine and save comparison
    position_comparison <- bind_rows(raw_positions, adj_positions) %>%
      select(position, proportion_type, detection_rate, ci_lower, ci_upper)
    
    write_csv(position_comparison, "output/r_results/positional/position_comparison.csv")
    
    message("\nPosition Effects Comparison (Raw vs. Adjusted):")
    position_comparison %>%
      arrange(proportion_type, position) %>%
      as.data.frame() %>%
      print()
    
    # Create position effects plot
    position_plot <- ggplot(position_comparison, 
                          aes(x = position, y = detection_rate, 
                              color = proportion_type, group = proportion_type)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      labs(title = "Careless Responding Detection Rates by Position",
           subtitle = "Raw vs. Adjusted Proportions",
           x = "Position in Screening Sequence",
           y = "Detection Rate (%)",
           color = "Proportion Type") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1", 
                        labels = c("Adjusted (% of remaining sample)", 
                                  "Raw (% of original sample)")) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"))
    
    # Save plot
    ggsave("output/figures/positional/position_effects.png", 
           position_plot, width = 8, height = 6, dpi = 300)
    
    # Calculate position-to-position changes for both metrics
    position_changes <- position_comparison %>%
      group_by(proportion_type) %>%
      arrange(position) %>%
      mutate(
        previous_rate = lag(detection_rate),
        absolute_change = detection_rate - lag(detection_rate),
        percentage_change = (detection_rate - lag(detection_rate)) / lag(detection_rate) * 100
      ) %>%
      filter(!is.na(percentage_change))
    
    # Save position changes
    write_csv(position_changes, "output/r_results/positional/position_changes.csv")
    
    message("\nPosition-to-Position Changes:")
    position_changes %>%
      select(proportion_type, position, previous_rate, detection_rate, 
            absolute_change, percentage_change) %>%
      as.data.frame() %>%
      print()
  }
}

# 5. Method × Position Interaction Analysis ------------------------------

message("\nRUNNING METHOD × POSITION INTERACTION ANALYSIS")
message("==========================================")

# Check if sequential data has both method and position information
if (nrow(sequential_ml) > 0 && 
    "method_position" %in% names(sequential_ml) && 
    "method_group" %in% names(sequential_ml)) {
  
  # For raw proportions
  raw_interaction_model <- run_multilevel_meta(
    sequential_ml,
    formula = ~ method_group * factor(method_position),
    random_structure = ~ 1 | ID,
    yi_var = "raw_logit_prop",
    vi_var = "raw_var_logit"
  )
  
  # For adjusted proportions
  adj_interaction_model <- run_multilevel_meta(
    sequential_ml,
    formula = ~ method_group * factor(method_position),
    random_structure = ~ 1 | ID,
    yi_var = "adj_logit_prop",
    vi_var = "adj_var_logit"
  )
  
  # Save interaction model results
  save_model_results(raw_interaction_model, "raw_method_position_interaction")
  save_model_results(adj_interaction_model, "adjusted_method_position_interaction")
  
  # Add method timing x position interaction if data available
  if ("method_timing" %in% names(sequential_ml)) {
    message("\nRunning Method Timing × Position Interaction Analysis...")
    
    # For raw proportions
    raw_timing_position_model <- run_multilevel_meta(
      sequential_ml,
      formula = ~ method_timing * factor(method_position),
      random_structure = ~ 1 | ID,
      yi_var = "raw_logit_prop",
      vi_var = "raw_var_logit"
    )
    
    # For adjusted proportions
    adj_timing_position_model <- run_multilevel_meta(
      sequential_ml,
      formula = ~ method_timing * factor(method_position),
      random_structure = ~ 1 | ID,
      yi_var = "adj_logit_prop",
      vi_var = "adj_var_logit"
    )
    
    # Save timing x position interaction results
    save_model_results(raw_timing_position_model, "raw_timing_position_interaction")
    save_model_results(adj_timing_position_model, "adjusted_timing_position_interaction")
  }
  
  # Improved prediction function for interaction effects
  generate_improved_predictions <- function(model_results, proportion_type = "Raw") {
    if (is.null(model_results) || is.null(model_results$model)) return(NULL)
    
    # Get the actual model
    model <- model_results$model
    
    # Get observed combinations from data
    observed_combos <- sequential_ml %>%
      select(method_group, method_position) %>%
      distinct()
    
    # Generate predictions for each observed combination
    predictions <- observed_combos %>%
      mutate(
        # Calculate predictions directly from model
        predicted_values = map2(method_group, method_position, function(m, p) {
          # Create newmods data frame
          newdata <- data.frame(
            method_group = m,
            method_position = p
          )
          
          # Safely predict
          pred_result <- tryCatch({
            predict(model, newmods = model.matrix(~ method_group * factor(method_position), 
                                                 data = newdata)[, -1, drop = FALSE])
          }, error = function(e) {
            message("Prediction error for ", m, " at position ", p, ": ", e$message)
            return(list(pred = NA, ci.lb = NA, ci.ub = NA))
          })
          
          # Return as tibble
          tibble(
            logit_pred = pred_result$pred,
            logit_ci_lb = pred_result$ci.lb,
            logit_ci_ub = pred_result$ci.ub
          )
        }, .dir = "first")
      ) %>%
      unnest(predicted_values) %>%
      # Transform to proportion scale
      mutate(
        pred_rate = transf.ilogit(logit_pred) * 100,
        ci_lower = transf.ilogit(logit_ci_lb) * 100,
        ci_upper = transf.ilogit(logit_ci_ub) * 100,
        proportion_type = proportion_type
      )
    
    return(predictions)
  }
  
  # Generate predictions with improved function
  message("Generating predictions for method × position combinations...")
  raw_predictions <- tryCatch({
    generate_improved_predictions(raw_interaction_model, "Raw")
  }, error = function(e) {
    message("Could not generate raw predictions: ", e$message)
    NULL
  })
  
  adj_predictions <- tryCatch({
    generate_improved_predictions(adj_interaction_model, "Adjusted")
  }, error = function(e) {
    message("Could not generate adjusted predictions: ", e$message)
    NULL
  })
  
  # Combine and save predictions if available
  if (!is.null(raw_predictions) || !is.null(adj_predictions)) {
    combined_predictions <- bind_rows(
      if(!is.null(raw_predictions)) raw_predictions else NULL,
      if(!is.null(adj_predictions)) adj_predictions else NULL
    )
    
    if(nrow(combined_predictions) > 0) {
      write_csv(combined_predictions, "output/r_results/positional/method_position_predictions.csv")
      message("Saved method × position predictions")
      
      # Create plots only if we have valid predictions
      if(nrow(combined_predictions) > 0) {
        # Create separate plots for raw and adjusted proportions
        create_method_position_plot <- function(predictions, proportion_type) {
          if(is.null(predictions) || nrow(predictions) == 0) return(NULL)
          
          ggplot(predictions, 
                aes(x = method_position, y = pred_rate, 
                    color = method_group, group = method_group)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
            labs(
              title = paste0("Position Effects by Method Type (", proportion_type, " Proportions)"),
              x = "Position in Screening Sequence",
              y = "Detection Rate (%)",
              color = "Method Type"
            ) +
            theme_minimal() +
            scale_color_brewer(palette = "Dark2") +
            theme(
              plot.title = element_text(face = "bold"),
              legend.position = "bottom"
            )
        }
        
        # Create plots for available predictions
        if(!is.null(raw_predictions) && nrow(raw_predictions) > 0) {
          raw_plot <- create_method_position_plot(raw_predictions, "Raw")
          if(!is.null(raw_plot)) {
            ggsave("output/figures/positional/raw_method_position.png", 
                  raw_plot, width = 9, height = 6, dpi = 300)
          }
        }
        
        if(!is.null(adj_predictions) && nrow(adj_predictions) > 0) {
          adj_plot <- create_method_position_plot(adj_predictions, "Adjusted")
          if(!is.null(adj_plot)) {
            ggsave("output/figures/positional/adjusted_method_position.png", 
                  adj_plot, width = 9, height = 6, dpi = 300)
          }
        }
        
        # Create combined plot if both available
        if(!is.null(raw_predictions) && !is.null(adj_predictions) && 
           nrow(raw_predictions) > 0 && nrow(adj_predictions) > 0) {
          if(exists("raw_plot") && exists("adj_plot") && 
             !is.null(raw_plot) && !is.null(adj_plot)) {
            combined_plot <- raw_plot / adj_plot + 
              plot_layout(guides = "collect") & 
              theme(legend.position = "bottom")
            
            ggsave("output/figures/positional/combined_method_position.png", 
                  combined_plot, width = 10, height = 12, dpi = 300)
          }
        }
      }
    }
  } else {
    message("Could not generate valid predictions for method × position interactions")
  }
} else {
  message("Insufficient data for method × position interaction analysis")
}

# 6. Sequence Path Analysis ----------------------------------------------

message("\nRUNNING SEQUENCE PATH ANALYSIS")
message("===========================")

# Check if sequential data is appropriate for sequence analysis
if (nrow(sequential_ml) > 0 && "method_group" %in% names(sequential_ml)) {
  # Add sequence information
  sequence_data <- sequential_ml %>%
    group_by(ID) %>%
    mutate(
      # Previous method type (for path analysis)
      prev_method_group = lag(method_group),
      
      # Create path variable (needed for studies with position > 1)
      method_path = case_when(
        method_position == 1 ~ "first",
        !is.na(prev_method_group) ~ paste(prev_method_group, method_group, sep = "-to-"),
        TRUE ~ NA_character_
      ),
      
      # Study sequence information
      sequence_length = n(),
      method_sequence = paste(method_group, collapse = "-")
    ) %>%
    ungroup()
  
  # Identify common sequences
  common_sequences <- sequence_data %>%
    distinct(ID, method_sequence) %>%
    count(method_sequence) %>%
    filter(n >= 3) %>%  # At least 3 studies with this sequence
    arrange(desc(n))
  
  if (nrow(common_sequences) > 0) {
    message(glue("\nFound {nrow(common_sequences)} common method sequences (n ≥ 3):"))
    print(head(common_sequences, 5))
    
    # Save common sequences
    write_csv(common_sequences, "output/r_results/positional/common_sequences.csv")
    
    # Analyze most common sequence
    if (nrow(common_sequences) >= 1) {
      top_sequence <- common_sequences$method_sequence[1]
      message(glue("\nAnalyzing the most common sequence: {top_sequence}"))
      
      # Get studies with this sequence
      top_sequence_data <- sequence_data %>%
        filter(method_sequence == top_sequence)
      
      # Generate position averages for this sequence
      top_sequence_summary <- top_sequence_data %>%
        group_by(method_position) %>%
        summarize(
          n_studies = n_distinct(ID),
          raw_rate = mean(raw_proportion, na.rm = TRUE) * 100,
          raw_sd = sd(raw_proportion, na.rm = TRUE) * 100,
          adj_rate = mean(adjusted_proportion, na.rm = TRUE) * 100,
          adj_sd = sd(adjusted_proportion, na.rm = TRUE) * 100,
          method_group = first(method_group),
          .groups = "drop"
        )
      
      # Save summary
      write_csv(top_sequence_summary, "output/r_results/positional/top_sequence_summary.csv")
      
      message("\nTop sequence effectiveness by position:")
      print(top_sequence_summary)
      
      # Create sequence visualization
      sequence_plot <- ggplot(top_sequence_summary, aes(x = method_position)) +
        geom_line(aes(y = raw_rate, color = "Raw Proportion"), size = 1) +
        geom_errorbar(aes(ymin = raw_rate - raw_sd,
                          ymax = raw_rate + raw_sd,
                          color = "Raw Proportion"), width = 0.2, alpha = 0.7) +
        geom_line(aes(y = adj_rate, color = "Adjusted Proportion"), size = 1) +
        geom_errorbar(aes(ymin = adj_rate - adj_sd,
                          ymax = adj_rate + adj_sd,
                          color = "Adjusted Proportion"), width = 0.2, alpha = 0.7) +
        geom_text(aes(y = 5, label = method_group), vjust = -1) +
        labs(
          title = glue("Detection Rates for Most Common Sequence: {top_sequence}"),
          subtitle = glue("Based on {top_sequence_summary$n_studies[1]} studies"),
          x = "Position in Screening Sequence",
          y = "Detection Rate (%)",
          color = "Proportion Type"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Set1") +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      ggsave("output/figures/positional/top_sequence.png", 
             sequence_plot, width = 8, height = 6, dpi = 300)
    }
    
    # Analyze method transitions (paths)
    transitions <- sequence_data %>%
      filter(method_position > 1) %>%
      count(prev_method_group, method_group) %>%
      filter(n >= 3) %>%
      arrange(desc(n))
    
    if (nrow(transitions) > 0) {
      message(glue("\nFound {nrow(transitions)} common method transitions (n ≥ 3):"))
      print(head(transitions, 5))
      
      # Save transitions
      write_csv(transitions, "output/r_results/positional/method_transitions.csv")
      
      # Analyze effectiveness of transitions
      transition_effectiveness <- sequence_data %>%
        filter(method_position > 1) %>%
        group_by(prev_method_group, method_group) %>%
        filter(n() >= 3) %>%  # At least 3 instances of this transition
        summarize(
          n_transitions = n(),
          raw_rate = mean(raw_proportion, na.rm = TRUE) * 100,
          raw_sd = sd(raw_proportion, na.rm = TRUE) * 100,
          adj_rate = mean(adjusted_proportion, na.rm = TRUE) * 100,
          adj_sd = sd(adjusted_proportion, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        mutate(transition = paste(prev_method_group, "→", method_group))
      
      # Save transition effectiveness
      write_csv(transition_effectiveness, "output/r_results/positional/transition_effectiveness.csv")
      
      # Create visualization
      if (nrow(transition_effectiveness) > 0) {
        # Prepare for plotting - reshape to long format
        transition_plot_data <- transition_effectiveness %>%
          pivot_longer(
            cols = c(raw_rate, adj_rate),
            names_to = "proportion_type",
            values_to = "rate"
          ) %>%
          mutate(
            sd = ifelse(proportion_type == "raw_rate", 
                       raw_sd, adj_sd),
            proportion_type = ifelse(proportion_type == "raw_rate", 
                                    "Raw", "Adjusted"),
            transition = reorder(transition, rate)
          )
        
        # Create transition effectiveness plot
        transition_plot <- ggplot(
          transition_plot_data, 
          aes(x = reorder(transition, rate), y = rate, fill = proportion_type)
        ) +
          geom_bar(stat = "identity", position = position_dodge()) +
          geom_errorbar(
            aes(ymin = rate - sd, ymax = rate + sd),
            position = position_dodge(width = 0.9),
            width = 0.25
          ) +
          coord_flip() +
          labs(
            title = "Effectiveness of Method Transitions",
            subtitle = "For transitions appearing in 3+ studies",
            x = "Method Transition",
            y = "Detection Rate (%)",
            fill = "Proportion Type"
          ) +
          theme_minimal() +
          scale_fill_brewer(palette = "Set1") +
          theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold")
          )
        
        ggsave("output/figures/positional/transition_effectiveness.png", 
               transition_plot, width = 9, height = 7, dpi = 300)
      }
    }
  }
}

# 7. Model Comparison ----------------------------------------------------

message("\nGENERATING MODEL COMPARISON")
message("========================")

# Compile all models for comparison
models_list <- list()

# Add models if they were successfully run
if (exists("method_model") && !is.null(method_model)) {
  models_list[["Method"]] <- method_model
}

if (exists("sample_model") && !is.null(sample_model)) {
  models_list[["Sample"]] <- sample_model
}

if (exists("raw_position_model") && !is.null(raw_position_model)) {
  models_list[["Position (Raw)"]] <- raw_position_model
}

if (exists("adj_position_model") && !is.null(adj_position_model)) {
  models_list[["Position (Adjusted)"]] <- adj_position_model
}

if (exists("raw_interaction_model") && !is.null(raw_interaction_model)) {
  models_list[["Method × Position (Raw)"]] <- raw_interaction_model
}

if (exists("adj_interaction_model") && !is.null(adj_interaction_model)) {
  models_list[["Method × Position (Adjusted)"]] <- adj_interaction_model
}

if (exists("variance_model") && !is.null(variance_model)) {
  models_list[["Variance Components"]] <- list(
    model = variance_model,
    AIC = variance_model$fit.stats["AIC", "REML"],
    BIC = variance_model$fit.stats["BIC", "REML"],
    k = variance_model$k
  )
}

# Create comparison table if we have models
if (length(models_list) > 0) {
  # Extract key comparison metrics
  model_comparison <- tibble(
    Model = names(models_list),
    k = map_dbl(models_list, ~ifelse(is.null(.x$k), NA_real_, .x$k)),
    AIC = map_dbl(models_list, ~ifelse(is.null(.x$AIC), NA_real_, .x$AIC)),
    BIC = map_dbl(models_list, ~ifelse(is.null(.x$BIC), NA_real_, .x$BIC)),
    QE = map_dbl(models_list, ~ifelse(is.null(.x$QE), NA_real_, .x$QE)),
    QEp = map_dbl(models_list, ~ifelse(is.null(.x$QEp), NA_real_, .x$QEp))
  )
  
  # Sort by AIC
  model_comparison <- model_comparison %>%
    arrange(AIC)
  
  # Save comparison
  write_csv(model_comparison, "output/r_results/multilevel/model_comparison.csv")
  
  message("\nModel Comparison:")
  model_comparison %>%
    mutate(QEp = format.pval(QEp, digits = 3)) %>%
    as.data.frame() %>%
    print()
}

# Final message
message("\nMultilevel and positional effects analysis complete! Results saved to output directories.")