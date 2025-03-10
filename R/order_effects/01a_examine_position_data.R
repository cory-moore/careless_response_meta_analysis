# 03a_explore_position_data.R
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

explore_position_data <- function(data) {
  cat("Exploratory analysis of position regression data\n")
  cat("===============================================\n\n")
  
  # Overall counts
  cat("Dataset overview:\n")
  cat("  Total observations:", nrow(data), "\n")
  cat("  Unique methods:", n_distinct(data$method_code), "\n")
  cat("  Unique method types:", n_distinct(data$method_type), "\n")
  cat("  Position range:", min(data$method_position), "to", max(data$method_position), "\n\n")
  
  # Strategy counts
  if ("strategy" %in% colnames(data)) {
    cat("Strategy distribution:\n")
    strategy_summary <- data %>%
      group_by(strategy) %>%
      summarize(
        count = n(),
        percent = round(n() / nrow(data) * 100, 1),
        .groups = "drop"
      )
    
    for (i in 1:nrow(strategy_summary)) {
      cat("  ", strategy_summary$strategy[i], ":", 
          strategy_summary$count[i], "observations",
          "(", strategy_summary$percent[i], "%)\n")
    }
    cat("\n")
  }
  
  # Position distribution
  cat("Position distribution:\n")
  position_summary <- data %>%
    group_by(method_position) %>%
    summarize(
      count = n(),
      percent = round(n() / nrow(data) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(method_position)
  
  for (i in 1:nrow(position_summary)) {
    cat("  Position", position_summary$method_position[i], ":", 
        position_summary$count[i], "observations",
        "(", position_summary$percent[i], "%)\n")
  }
  cat("\n")
  
  # Cross-tabulation of position and strategy
  if ("strategy" %in% colnames(data)) {
    cat("Position by strategy:\n")
    position_strategy <- table(data$method_position, data$strategy)
    print(position_strategy)
    cat("\n")
  }
  
  # Method-level analysis
  cat("Method-level analysis:\n")
  method_summary <- data %>%
    group_by(method_code, method_name, method_type) %>%
    summarize(
      n_observations = n(),
      n_positions = n_distinct(method_position),
      positions = paste(sort(unique(method_position)), collapse=", "),
      mean_proportion = mean(proportion, na.rm = TRUE),
      mean_adjusted = mean(adjusted_proportion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_observations))
  
  cat("Top 10 methods by observation count:\n")
  top_methods <- head(method_summary, 10)
  for (i in 1:nrow(top_methods)) {
    cat("  Method", top_methods$method_code[i], 
        "(", top_methods$method_name[i], "):", 
        top_methods$n_observations[i], "observations across", 
        top_methods$n_positions[i], "positions (",
        top_methods$positions[i], ")\n")
  }
  cat("\n")
  
  # Methods with position diversity
  multi_position_methods <- method_summary %>%
    filter(n_positions > 1) %>%
    arrange(desc(n_positions), desc(n_observations))
  
  cat("Methods appearing in multiple positions:\n")
  if (nrow(multi_position_methods) > 0) {
    for (i in 1:nrow(multi_position_methods)) {
      cat("  Method", multi_position_methods$method_code[i], 
          "(", multi_position_methods$method_name[i], "):", 
          multi_position_methods$n_observations[i], "observations across", 
          multi_position_methods$n_positions[i], "positions (",
          multi_position_methods$positions[i], ")\n")
    }
  } else {
    cat("  None found!\n")
  }
  cat("\n")
  
  # Check for meta-regression viability
  cat("Meta-regression viability analysis:\n")
  
  # Methods with sufficient data per position
  method_position_counts <- data %>%
    group_by(method_code, method_name, method_position) %>%
    summarize(count = n(), .groups = "drop")
  
  viable_method_positions <- method_position_counts %>%
    filter(count >= 3) # At least 3 observations per method-position combination
  
  methods_for_regression <- viable_method_positions %>%
    group_by(method_code, method_name) %>%
    summarize(
      positions = n_distinct(method_position),
      total_obs = sum(count),
      .groups = "drop"
    ) %>%
    filter(positions >= 2) # At least 2 positions with sufficient data
  
  cat("Methods viable for meta-regression (at least 2 positions with 3+ observations):\n")
  if (nrow(methods_for_regression) > 0) {
    for (i in 1:nrow(methods_for_regression)) {
      cat("  Method", methods_for_regression$method_code[i], 
          "(", methods_for_regression$method_name[i], "):", 
          methods_for_regression$positions[i], "positions,",
          methods_for_regression$total_obs[i], "total observations\n")
    }
  } else {
    cat("  None found!\n")
  }
  cat("\n")
  
  # Method type analysis
  cat("Method type analysis:\n")
  type_summary <- data %>%
    group_by(method_type) %>%
    summarize(
      n_observations = n(),
      n_methods = n_distinct(method_code),
      n_positions = n_distinct(method_position),
      positions = paste(sort(unique(method_position)), collapse=", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_observations))
  
  for (i in 1:nrow(type_summary)) {
    cat("  Type", type_summary$method_type[i], ":", 
        type_summary$n_observations[i], "observations,",
        type_summary$n_methods[i], "unique methods,",
        type_summary$n_positions[i], "positions (",
        type_summary$positions[i], ")\n")
  }
  cat("\n")
  
  # Check method type viability for meta-regression
  type_position_counts <- data %>%
    group_by(method_type, method_position) %>%
    summarize(count = n(), .groups = "drop")
  
  viable_type_positions <- type_position_counts %>%
    filter(count >= 3) # At least 3 observations per type-position
  
  types_for_regression <- viable_type_positions %>%
    group_by(method_type) %>%
    summarize(
      positions = n_distinct(method_position),
      total_obs = sum(count),
      .groups = "drop"
    ) %>%
    filter(positions >= 2) # At least 2 positions
  
  cat("Method types viable for meta-regression (at least 2 positions with 3+ observations):\n")
  if (nrow(types_for_regression) > 0) {
    for (i in 1:nrow(types_for_regression)) {
      cat("  Type", types_for_regression$method_type[i], ":", 
          types_for_regression$positions[i], "positions,",
          types_for_regression$total_obs[i], "total observations\n")
    }
  } else {
    cat("  None found!\n")
  }
  
  # Create simple visualization of position effects
  cat("\nCreating exploratory visualizations...\n")
  
  # Overall position effect
  p1 <- ggplot(data, aes(x = method_position, y = adjusted_proportion)) +
    geom_jitter(alpha = 0.3, width = 0.2, height = 0) +
    geom_boxplot(alpha = 0.5) +
    labs(title = "Distribution of Careless Responding Rates by Position",
         x = "Position in Screening Sequence",
         y = "Adjusted Careless Responding Rate") +
    theme_minimal()
  
  ggsave("output/order_effects/exploratory_position_effect.png", p1, 
         width = 8, height = 6, dpi = 300)
  
  # Position effect by method type
  if (n_distinct(data$method_type) > 1) {
    p2 <- ggplot(data, aes(x = method_position, y = adjusted_proportion, fill = method_type)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "Careless Responding Rates by Position and Method Type",
           x = "Position in Screening Sequence",
           y = "Adjusted Careless Responding Rate",
           fill = "Method Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave("output/order_effects/exploratory_position_by_type.png", p2, 
           width = 10, height = 7, dpi = 300)
  }
  
  # Return key information
  return(list(
    method_summary = method_summary,
    type_summary = type_summary,
    methods_for_regression = methods_for_regression,
    types_for_regression = types_for_regression
  ))
}

# Main execution
cat("Loading position regression dataset...\n")
position_data <- read_csv("output/order_effects/data/position_regression.csv", 
                         show_col_types = FALSE)

# Run exploratory analysis
exploratory_results <- explore_position_data(position_data)

# Save detailed results
write_csv(exploratory_results$method_summary, 
         "output/order_effects/exploratory_method_summary.csv")
write_csv(exploratory_results$type_summary, 
         "output/order_effects/exploratory_type_summary.csv")

cat("\nExploratory analysis complete!\n")
cat("Results saved to output/order_effects/exploratory_*.csv\n")
cat("Visualizations saved to output/order_effects/exploratory_*.png\n")