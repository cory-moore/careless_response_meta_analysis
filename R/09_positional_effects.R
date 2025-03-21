###############################################################################
# 07_positional_effects.R
# 
# This script implements a comprehensive analysis of position effects in 
# sequential careless responding screening. It examines how the position of
# a detection method in a screening sequence affects its effectiveness.
#
# The analysis includes:
# 1. Basic positional analysis (standard meta-regression approach)
# 2. Multi-level analysis controlling for study-level dependencies
# 3. Method-specific position effects
# 4. Path analysis examining screening sequences
# 5. Direct comparative analysis of position effectiveness
#
# For each analysis, both raw proportions (based on original sample size) and
# adjusted proportions (based on remaining sample size) are examined to provide
# complementary perspectives on detection method effectiveness.
###############################################################################

library(tidyverse)
library(metafor)
library(glue)
library(purrr)
library(ggplot2)
library(cowplot)

# Create output directory
dir.create("output/r_results/positional_effects", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/positional_effects", recursive = TRUE, showWarnings = FALSE)

# Load sequential data
sequential_data <- read_csv("data/for_r_meta/sequential_data.csv", show_col_types = FALSE)
cat(glue("Loaded sequential dataset: {nrow(sequential_data)} method-position combinations\n"))
cat(glue("Dataset contains {sequential_data %>% distinct(ID) %>% nrow()} unique studies\n"))

# Display position distribution
position_counts <- sequential_data %>% 
  count(method_position) %>% 
  arrange(method_position)

cat("\nPosition distribution:\n")
for (i in 1:nrow(position_counts)) {
  cat(glue("  Position {position_counts$method_position[i]}: {position_counts$n[i]} methods\n"))
}

# Study sequences information
sequence_counts <- sequential_data %>%
  group_by(ID) %>%
  summarize(
    sequence_length = n(),
    .groups = "drop"
  ) %>%
  count(sequence_length) %>%
  arrange(sequence_length)

cat("\nSequence lengths:\n")
for (i in 1:nrow(sequence_counts)) {
  cat(glue("  {sequence_counts$sequence_length[i]} methods: {sequence_counts$n[i]} studies\n"))
}

###############################################################################
# 1. BASIC POSITIONAL ANALYSIS
###############################################################################

cat("\n\nBASIC POSITIONAL ANALYSIS\n")
cat("=========================\n")

# Function to run basic position meta-regression
run_position_model <- function(data, yi_var, vi_var, proportion_type = "raw") {
  # Verify variables exist
  if (!all(c(yi_var, vi_var) %in% colnames(data))) {
    cat(glue("  Error: Required variables {yi_var} or {vi_var} not found\n"))
    return(NULL)
  }
  
  # Create formula
  formula <- as.formula(paste(yi_var, "~", "factor(method_position)"))
  
  # Fit model
  model <- try(rma(formula, vi = data[[vi_var]], data = data, method = "REML"))
  
  if (inherits(model, "try-error")) {
    cat(glue("  Error fitting {proportion_type} position model\n"))
    return(NULL)
  }
  
  # Calculate R² compared to null model
  null_model <- rma(data[[yi_var]], vi = data[[vi_var]], data = data, method = "REML")
  r_squared <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
  
  # Report results
  cat(glue("\nPosition effects ({proportion_type} proportions):\n"))
  cat(glue("  QM = {round(model$QM, 2)} (df = {model$p-1}), p = {format.pval(model$QMp, digits=3)}\n"))
  cat(glue("  R² = {round(r_squared * 100, 1)}% of heterogeneity explained\n"))
  
  # Extract coefficients and transform to proportion scale
  coefs <- coef(summary(model)) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(
      # Clean up term names
      position = case_when(
        term == "intrcpt" ~ 1,
        str_detect(term, "factor\\(method_position\\)") ~ 
          as.numeric(str_replace(term, "factor\\(method_position\\)", "")),
        TRUE ~ NA_real_
      ),
      # Transform to proportion scale
      estimate_raw = estimate,
      proportion = transf.ilogit(estimate),
      ci_lower = transf.ilogit(estimate - 1.96 * se),
      ci_upper = transf.ilogit(estimate + 1.96 * se),
      
      # Add significance indicators
      sig = case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**", 
        pval < 0.05 ~ "*",
        pval < 0.1 ~ ".",
        TRUE ~ ""
      ),
      
      # Add data for plotting
      proportion_type = proportion_type
    )
  
  # Create summary data
  summary_data <- data.frame(
    model = paste("Position effects -", proportion_type),
    k = model$k,
    QM = model$QM,
    QM_df = model$p - 1,
    QM_p = model$QMp,
    R2 = r_squared,
    tau2 = model$tau2,
    I2 = model$I2,
    proportion_type = proportion_type
  )
  
  return(list(
    model = model,
    coefficients = coefs,
    summary = summary_data,
    null_model = null_model
  ))
}

# Run models for raw and adjusted proportions
raw_model <- run_position_model(sequential_data, "raw_logit_prop", "raw_var_logit", "raw")
adj_model <- run_position_model(sequential_data, "adj_logit_prop", "adj_var_logit", "adjusted")

# Combine results and save
if (!is.null(raw_model) && !is.null(adj_model)) {
  # Combined summary
  all_summary <- bind_rows(raw_model$summary, adj_model$summary)
  write_csv(all_summary, "output/r_results/positional_effects/01_basic_position_effects_summary.csv")
  
  # Combined coefficients
  all_coefs <- bind_rows(raw_model$coefficients, adj_model$coefficients)
  write_csv(all_coefs, "output/r_results/positional_effects/01_basic_position_effects_coefficients.csv")
  
  # Create position effects plot
  position_plot <- ggplot(all_coefs, aes(x = position, y = proportion, color = proportion_type, group = proportion_type)) +
    geom_point(size = 3) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Careless Responding Detection Rates by Position",
      subtitle = "Raw vs. Adjusted Proportions",
      x = "Position in Screening Sequence",
      y = "Proportion of Careless Responses",
      color = "Proportion Type"
    ) +
    theme_classic() +
    scale_color_brewer(palette = "Set1", 
                      labels = c("Adjusted (% of remaining sample)", 
                                "Raw (% of original sample)")) +
    theme(legend.position = "bottom")
  
  ggsave("output/figures/positional_effects/01_position_effects.png", position_plot, width = 8, height = 6, dpi = 300)
}

###############################################################################
# 2. MULTI-LEVEL ANALYSIS
###############################################################################

cat("\n\nMULTI-LEVEL POSITIONAL ANALYSIS\n")
cat("==============================\n")

# Function to run multi-level position model
run_multilevel_position_model <- function(data, yi_var, vi_var, proportion_type = "raw") {
  # Verify variables exist
  if (!all(c(yi_var, vi_var) %in% colnames(data))) {
    cat(glue("  Error: Required variables {yi_var} or {vi_var} not found\n"))
    return(NULL)
  }
  
  # Run model with study as a random effect
  model <- try(rma.mv(
    yi = data[[yi_var]], 
    V = data[[vi_var]],
    mods = ~ factor(method_position),
    random = ~ 1 | ID,
    data = data,
    method = "REML"
  ))
  
  if (inherits(model, "try-error")) {
    cat(glue("  Error fitting multilevel {proportion_type} position model\n"))
    return(NULL)
  }
  
  # Extract coefficients
  coefs <- coef(summary(model)) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(
      position = case_when(
        term == "intrcpt" ~ 1,
        str_detect(term, "factor\\(method_position\\)") ~ 
          as.numeric(str_replace(term, "factor\\(method_position\\)", "")),
        TRUE ~ NA_real_
      ),
      # Transform to proportion scale
      estimate_raw = estimate,
      proportion = transf.ilogit(estimate),
      ci_lower = transf.ilogit(estimate - 1.96 * se),
      ci_upper = transf.ilogit(estimate + 1.96 * se),
      proportion_type = proportion_type
    )
  
  # Report results
  cat(glue("\nMultilevel position model ({proportion_type} proportions):\n"))
  cat(glue("  QM = {round(model$QM, 2)} (df = {model$p-1}), p = {format.pval(model$QMp, digits=3)}\n"))
  cat(glue("  Study-level variance: {round(model$sigma2, 4)}\n"))
  
  # Create summary data
  summary_data <- data.frame(
    model = paste("Multilevel position -", proportion_type),
    k = model$k,
    QM = model$QM,
    QM_df = model$p - 1,
    QM_p = model$QMp,
    study_variance = model$sigma2,
    proportion_type = proportion_type
  )
  
  return(list(
    model = model,
    coefficients = coefs,
    summary = summary_data
  ))
}

# Run multi-level models
multilevel_raw <- run_multilevel_position_model(sequential_data, "raw_logit_prop", "raw_var_logit", "raw")
multilevel_adj <- run_multilevel_position_model(sequential_data, "adj_logit_prop", "adj_var_logit", "adjusted")

# Save results
if (!is.null(multilevel_raw) && !is.null(multilevel_adj)) {
  # Combined summary
  multilevel_summary <- bind_rows(multilevel_raw$summary, multilevel_adj$summary)
  write_csv(multilevel_summary, "output/r_results/positional_effects/02_multilevel_position_summary.csv")
  
  # Combined coefficients
  multilevel_coefs <- bind_rows(multilevel_raw$coefficients, multilevel_adj$coefficients)
  write_csv(multilevel_coefs, "output/r_results/positional_effects/02_multilevel_position_coefficients.csv")
  
  # Create comparison plot (basic vs. multilevel)
  if (exists("all_coefs")) {
    # Prepare data for comparison
    basic_coefs <- all_coefs %>%
      select(position, proportion, proportion_type) %>%
      mutate(model_type = "Basic")
    
    ml_coefs <- multilevel_coefs %>%
      select(position, proportion, proportion_type) %>%
      mutate(model_type = "Multilevel")
    
    comparison_data <- bind_rows(basic_coefs, ml_coefs)
    
    # Create comparison plot
    comparison_plot <- ggplot(
      comparison_data, 
      aes(x = position, y = proportion, 
          color = proportion_type, 
          linetype = model_type,
          group = interaction(proportion_type, model_type))
    ) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Comparison of Basic vs. Multilevel Position Models",
        x = "Position in Screening Sequence",
        y = "Proportion of Careless Responses",
        color = "Proportion Type",
        linetype = "Model Type"
      ) +
      theme_classic() +
      scale_color_brewer(palette = "Set1", 
                        labels = c("Adjusted", "Raw")) +
      theme(legend.position = "bottom")
    
    ggsave("output/figures/positional_effects/02_model_comparison.png", comparison_plot, width = 8, height = 6, dpi = 300)
  }
}

###############################################################################
# 3. METHOD-SPECIFIC POSITION EFFECTS
###############################################################################

cat("\n\nMETHOD-SPECIFIC POSITION EFFECTS\n")
cat("===============================\n")

# Check if method type is available
if ("method_type" %in% colnames(sequential_data)) {
  # Get method types
  method_types <- unique(sequential_data$method_type)
  cat(glue("  Found {length(method_types)} method types\n"))
  
  # Function to run method x position interaction model
  run_interaction_model <- function(data, yi_var, vi_var, proportion_type = "raw") {
    # Verify variables exist
    if (!all(c(yi_var, vi_var, "method_type") %in% colnames(data))) {
      cat(glue("  Error: Required variables not found\n"))
      return(NULL)
    }
    
    # Run interaction model
    model <- try(rma(
      yi = data[[yi_var]], 
      vi = data[[vi_var]],
      mods = ~ factor(method_position) * factor(method_type),
      data = data,
      method = "REML"
    ))
    
    if (inherits(model, "try-error")) {
      cat(glue("  Error fitting interaction model for {proportion_type} proportions\n"))
      return(NULL)
    }
    
    # Calculate R² compared to position-only model
    position_model <- rma(
      yi = data[[yi_var]], 
      vi = data[[vi_var]],
      mods = ~ factor(method_position),
      data = data,
      method = "REML"
    )
    
    r_squared_vs_position <- max(0, (position_model$tau2 - model$tau2) / position_model$tau2)
    
    # Calculate R² compared to null model
    null_model <- rma(data[[yi_var]], vi = data[[vi_var]], data = data, method = "REML")
    r_squared_total <- max(0, (null_model$tau2 - model$tau2) / null_model$tau2)
    
    # Report results
    cat(glue("\nPosition × Method Type interaction ({proportion_type} proportions):\n"))
    cat(glue("  QM = {round(model$QM, 2)} (df = {model$p-1}), p = {format.pval(model$QMp, digits=3)}\n"))
    cat(glue("  R² vs. null model: {round(r_squared_total * 100, 1)}%\n"))
    cat(glue("  Additional R² beyond position-only: {round(r_squared_vs_position * 100, 1)}%\n"))
    
    # Summary data
    summary_data <- data.frame(
      model = paste("Position × Method interaction -", proportion_type),
      k = model$k,
      QM = model$QM,
      QM_df = model$p - 1,
      QM_p = model$QMp,
      R2_vs_null = r_squared_total,
      R2_vs_position = r_squared_vs_position,
      proportion_type = proportion_type
    )
    
    return(list(
      model = model,
      summary = summary_data,
      position_model = position_model,
      null_model = null_model
    ))
  }
  
  # Run interaction models
  interaction_raw <- run_interaction_model(sequential_data, "raw_logit_prop", "raw_var_logit", "raw")
  interaction_adj <- run_interaction_model(sequential_data, "adj_logit_prop", "adj_var_logit", "adjusted")
  
  # Save results
  if (!is.null(interaction_raw) && !is.null(interaction_adj)) {
    interaction_summary <- bind_rows(interaction_raw$summary, interaction_adj$summary)
    write_csv(interaction_summary, "output/r_results/positional_effects/03_method_position_interaction.csv")
  }
  
  # Method-specific position trends (by method type)
  cat("\nAnalyzing method-specific position trends...\n")
  
  # Function to generate predicted values for each method type and position
  generate_method_predictions <- function(data, model, proportion_type = "raw") {
    # Create grid of method types and positions
    method_types <- unique(data$method_type)
    positions <- sort(unique(data$method_position))
    
    grid <- expand.grid(
      method_type = method_types,
      method_position = positions
    )
    
    # Generate predictions
    preds <- predict(model, newmods = model.matrix(~ factor(method_position) * factor(method_type), data = grid)[,-1])
    
    # Combine with grid and transform
    result <- grid %>%
      mutate(
        logit_pred = preds$pred,
        logit_ci_lb = preds$ci.lb,
        logit_ci.ub = preds$ci.ub,
        pred = transf.ilogit(logit_pred),
        ci_lb = transf.ilogit(logit_ci_lb),
        ci_ub = transf.ilogit(logit_ci.ub),
        proportion_type = proportion_type
      )
    
    return(result)
  }
  
  # Generate predictions
  if (!is.null(interaction_raw$model) && !is.null(interaction_adj$model)) {
    raw_preds <- generate_method_predictions(sequential_data, interaction_raw$model, "raw")
    adj_preds <- generate_method_predictions(sequential_data, interaction_adj$model, "adjusted")
    
    all_preds <- bind_rows(raw_preds, adj_preds)
    write_csv(all_preds, "output/r_results/positional_effects/03_method_position_predictions.csv")
    
    # Create method-specific position plots
    raw_plot <- ggplot(raw_preds, aes(x = method_position, y = pred, color = method_type, group = method_type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = method_type), alpha = 0.2, color = NA) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Position Effects by Method Type (Raw Proportions)",
        subtitle = "Based on original sample size",
        x = "Position in Screening Sequence",
        y = "Proportion of Careless Responses",
        color = "Method Type",
        fill = "Method Type"
      ) +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2")
    
    adj_plot <- ggplot(adj_preds, aes(x = method_position, y = pred, color = method_type, group = method_type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = method_type), alpha = 0.2, color = NA) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Position Effects by Method Type (Adjusted Proportions)",
        subtitle = "Based on remaining sample at each position",
        x = "Position in Screening Sequence",
        y = "Proportion of Careless Responses",
        color = "Method Type",
        fill = "Method Type"
      ) +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2")
    
    # Save plots
    ggsave("output/figures/positional_effects/03a_raw_method_position.png", raw_plot, width = 9, height = 6, dpi = 300)
    ggsave("output/figures/positional_effects/03b_adj_method_position.png", adj_plot, width = 9, height = 6, dpi = 300)
    
    # Combined plot
    combined_plot <- plot_grid(raw_plot, adj_plot, ncol = 1, labels = "AUTO")
    ggsave("output/figures/positional_effects/03c_combined_method_position.png", combined_plot, width = 10, height = 12, dpi = 300)
  }
}

###############################################################################
# 4. SEQUENCE PATH ANALYSIS
###############################################################################

cat("\n\nSEQUENCE PATH ANALYSIS\n")
cat("=====================\n")

# Add sequence information
sequential_data <- sequential_data %>%
  group_by(ID) %>%
  mutate(
    # Previous method type (for path analysis)
    prev_method_type = lag(method_type),
    
    # Create path variable (needed for studies with position > 1)
    method_path = case_when(
      method_position == 1 ~ "first",
      !is.na(prev_method_type) ~ paste(prev_method_type, method_type, sep="-to-"),
      TRUE ~ NA_character_
    ),
    
    # Study sequence information
    sequence_length = n(),
    method_sequence = paste(method_type, collapse="-")
  ) %>%
  ungroup()

# Identify common sequences
common_sequences <- sequential_data %>%
  distinct(ID, method_sequence) %>%
  count(method_sequence) %>%
  filter(n >= 3) %>%  # At least 3 studies with this sequence
  arrange(desc(n))

if (nrow(common_sequences) > 0) {
  cat(glue("\nFound {nrow(common_sequences)} common method sequences (n ≥ 3):\n"))
  for (i in 1:min(5, nrow(common_sequences))) {
    cat(glue("  {common_sequences$method_sequence[i]} (n = {common_sequences$n[i]})\n"))
  }
  if (nrow(common_sequences) > 5) {
    cat(glue("  ... and {nrow(common_sequences) - 5} more\n"))
  }
  
  # Save common sequences
  write_csv(common_sequences, "output/r_results/positional_effects/04_common_sequences.csv")
  
  # Analyze most common sequences
  if (nrow(common_sequences) >= 1) {
    top_sequence <- common_sequences$method_sequence[1]
    cat(glue("\nAnalyzing the most common sequence: {top_sequence}\n"))
    
    # Get studies with this sequence
    top_sequence_data <- sequential_data %>%
      filter(method_sequence == top_sequence)
    
    # Generate position averages for this sequence
    top_sequence_summary <- top_sequence_data %>%
      group_by(method_position) %>%
      summarize(
        n_studies = n_distinct(ID),
        raw_proportion_mean = mean(raw_proportion, na.rm = TRUE),
        raw_proportion_sd = sd(raw_proportion, na.rm = TRUE),
        adj_proportion_mean = mean(adjusted_proportion, na.rm = TRUE),
        adj_proportion_sd = sd(adjusted_proportion, na.rm = TRUE),
        method_type = first(method_type),
        .groups = "drop"
      )
    
    # Save summary
    write_csv(top_sequence_summary, "output/r_results/positional_effects/04_top_sequence_summary.csv")
    
    # Create sequence visualization
    sequence_plot <- ggplot(top_sequence_summary, aes(x = method_position)) +
      geom_line(aes(y = raw_proportion_mean, color = "Raw Proportion"), size = 1) +
      geom_errorbar(aes(ymin = raw_proportion_mean - raw_proportion_sd,
                        ymax = raw_proportion_mean + raw_proportion_sd,
                        color = "Raw Proportion"), width = 0.2, alpha = 0.7) +
      geom_line(aes(y = adj_proportion_mean, color = "Adjusted Proportion"), size = 1) +
      geom_errorbar(aes(ymin = adj_proportion_mean - adj_proportion_sd,
                        ymax = adj_proportion_mean + adj_proportion_sd,
                        color = "Adjusted Proportion"), width = 0.2, alpha = 0.7) +
      geom_text(aes(y = 0.05, label = method_type), vjust = -1) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = glue("Detection Rates for Most Common Sequence: {top_sequence}"),
        subtitle = glue("Based on {top_sequence_summary$n_studies[1]} studies"),
        x = "Position in Screening Sequence",
        y = "Proportion of Careless Responses",
        color = "Proportion Type"
      ) +
      theme_classic() +
      scale_color_brewer(palette = "Set1") +
      theme(legend.position = "bottom")
    
    ggsave("output/figures/positional_effects/04_top_sequence.png", sequence_plot, width = 8, height = 6, dpi = 300)
  }
  
  # Analyze method transitions (paths)
  cat("\nAnalyzing method transitions...\n")
  
  # Find common transitions
  transitions <- sequential_data %>%
    filter(method_position > 1) %>%
    count(prev_method_type, method_type) %>%
    filter(n >= 3) %>%
    arrange(desc(n))
  
  if (nrow(transitions) > 0) {
    cat(glue("  Found {nrow(transitions)} common method transitions (n ≥ 3):\n"))
    for (i in 1:min(5, nrow(transitions))) {
      cat(glue("  {transitions$prev_method_type[i]} → {transitions$method_type[i]} (n = {transitions$n[i]})\n"))
    }
    if (nrow(transitions) > 5) {
      cat(glue("  ... and {nrow(transitions) - 5} more\n"))
    }
    
    # Save transitions
    write_csv(transitions, "output/r_results/positional_effects/04_method_transitions.csv")
    
    # Analyze effectiveness of transitions
    transition_effectiveness <- sequential_data %>%
      filter(method_position > 1) %>%
      group_by(prev_method_type, method_type) %>%
      filter(n() >= 3) %>%  # At least 3 instances of this transition
      summarize(
        n_transitions = n(),
        raw_proportion_mean = mean(raw_proportion, na.rm = TRUE),
        raw_proportion_sd = sd(raw_proportion, na.rm = TRUE),
        adj_proportion_mean = mean(adjusted_proportion, na.rm = TRUE),
        adj_proportion_sd = sd(adjusted_proportion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(transition = paste(prev_method_type, "→", method_type))
    
    # Save transition effectiveness
    write_csv(transition_effectiveness, "output/r_results/positional_effects/04_transition_effectiveness.csv")
    
    # Create transition effectiveness plot
    if (nrow(transition_effectiveness) > 0) {
      # Prepare for plotting - reshape to long format
      transition_plot_data <- transition_effectiveness %>%
        pivot_longer(
          cols = c(raw_proportion_mean, adj_proportion_mean),
          names_to = "proportion_type",
          values_to = "mean"
        ) %>%
        mutate(
          sd = ifelse(proportion_type == "raw_proportion_mean", 
                     raw_proportion_sd, adj_proportion_sd),
          proportion_type = ifelse(proportion_type == "raw_proportion_mean", 
                                  "Raw", "Adjusted"),
          transition = reorder(transition, mean)
        )
      
      # Create plot
      transition_plot <- ggplot(
        transition_plot_data, 
        aes(x = reorder(transition, mean), y = mean, fill = proportion_type)
      ) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(
          aes(ymin = mean - sd, ymax = mean + sd),
          position = position_dodge(width = 0.9),
          width = 0.25
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        coord_flip() +
        labs(
          title = "Effectiveness of Method Transitions",
          subtitle = "For transitions appearing in 3+ studies",
          x = "Method Transition",
          y = "Proportion of Careless Responses",
          fill = "Proportion Type"
        ) +
        theme_classic() +
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "bottom")
      
      ggsave("output/figures/positional_effects/04_transition_effectiveness.png", 
             transition_plot, width = 9, height = 7, dpi = 300)
    }
  }
}

###############################################################################
# 5. DIRECT COMPARATIVE ANALYSIS
###############################################################################

cat("\n\nDIRECT COMPARATIVE ANALYSIS\n")
cat("=========================\n")

# Calculate method effectiveness by position
method_position_effectiveness <- sequential_data %>%
  group_by(method_type, method_position) %>%
  filter(