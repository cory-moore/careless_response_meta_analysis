###############################################################################
# 05_compare_metas.R
# 
# This script compares the results of the three meta-analytical approaches:
# 1. First-Method Approach (PRIMARY): Uses single-method studies and only the 
#    first method from sequential screening studies to eliminate order effects.
# 2. Single-Method Approach (SECONDARY): Uses only studies with a single CR 
#    detection method for maximum internal validity.
# 3. Overall/Total Approach (TERTIARY): Uses total CR amounts from all studies 
#    regardless of method configuration to maximize data utilization.
#
# The script performs four main types of analyses:
# - Comparison of overall pooled proportions across approaches
# - Formal statistical tests of differences between approaches
# - Tests for heterogeneity between approaches (overall and within subgroups)
# - Comparison of temporal trends across approaches
#
# Method-related subgroups are only compared between primary and secondary
# approaches, as they are conceptually problematic for the Overall approach.
# Sample-related subgroups are compared across all three approaches.
###############################################################################

library(tidyverse)
library(metafor)
library(glue)

approaches <- c("primary", "secondary", "overall")
all_approaches <- map_dfr(approaches, ~read_csv(glue("output/r_results/{.x}/overall_results.csv"), show_col_types = FALSE))

cat("\nCOMPARING META-ANALYSIS APPROACHES:\n")

# Format for display and report overall proportions
all_approaches <- all_approaches %>%
  mutate(
    Prop_Formatted = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]"),
    # Create logit-scale values for statistical testing
    yi = log(Pooled_Proportion / (1 - Pooled_Proportion)),
    vi = (CI_Upper - CI_Lower)^2 / (2 * 1.96)^2,
    se = sqrt(vi)
  )

cat("\nCalculating overall pooled proportions...\n")

# Calculate pairwise differences with formal z-tests
cat("\nCalculating pairwise differences...\n")
pairwise_stats <- combn(1:nrow(all_approaches), 2, simplify = FALSE) %>%
  map_dfr(~{
    x <- all_approaches[.x,]
    
    # Calculate difference on proportion scale
    abs_diff <- abs(x$Pooled_Proportion[1] - x$Pooled_Proportion[2])
    perc_diff <- abs_diff / min(x$Pooled_Proportion) * 100
    
    # Check confidence interval overlap
    overlap_ci <- !(x$CI_Upper[1] < x$CI_Lower[2] || x$CI_Upper[2] < x$CI_Lower[1])
    
    # z-test on logit scale
    diff_logit <- x$yi[1] - x$yi[2]
    se_diff <- sqrt(x$vi[1] + x$vi[2])
    z_stat <- diff_logit / se_diff
    p_value <- 2 * (1 - pnorm(abs(z_stat)))
    
    tibble(
      Comparison = glue("{x$Analysis[1]} vs {x$Analysis[2]}"),
      Absolute_Diff = abs_diff,
      Percentage_Diff = perc_diff,
      CI_Overlap = overlap_ci,
      Z_statistic = z_stat,
      P_value = p_value,
      Significant = p_value < 0.05
    )
  })

write_csv(pairwise_stats, "output/r_results/comparisons/pairwise_statistical_tests.csv")
write_csv(all_approaches, "output/r_results/comparisons/approach_comparison.csv")

# Test for heterogeneity between approaches
cat("\nTesting for statistical heterogeneity between approaches...\n")
meta_test <- rma(yi = yi, vi = vi, data = all_approaches)

heterogeneity_results <- tibble(
  Comparison = "Overall approaches",
  Q_statistic = meta_test$QE,
  df = meta_test$k - 1,
  P_value = meta_test$QEp,
  Significant = meta_test$QEp < 0.05,
  I2 = meta_test$I2,
  H2 = meta_test$H2,
  tau2 = meta_test$tau2
)

# Define subgroup variables by type
method_vars <- c("method_type", "cr_method", "method_timing")
sample_vars <- c("sample_source", "sample_recruitment", "sample_platform", "sample_method",
                "journal", "sample_level", "sample_incentive", "sample_country", 
                "design_method", "design_location")

# Function to compare subgroups across approaches and test heterogeneity
compare_subgroups <- function(var, methods_only = FALSE) {
  cat(glue("\nComparing by {var}...\n"))
  
  # Select appropriate approaches
  apps <- if(methods_only) c("primary", "secondary") else approaches
  
  # Get data from each approach
  data_list <- map(apps, ~{
    path <- glue("output/r_results/{.x}/subgroup_{var}.csv")
    if(file.exists(path)) {
      read_csv(path, show_col_types = FALSE) %>% 
        mutate(Approach = .x,
               yi = log(Pooled_Proportion / (1 - Pooled_Proportion)),
               vi = (CI_Upper - CI_Lower)^2 / (2 * 1.96)^2)
    }
  }) %>% 
    compact()
  
  if(length(data_list) < 2) return(NULL)
  
  # Process and write comparison
  combined <- bind_rows(data_list) %>%
    mutate(Prop_CI = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]"))
  
  # Create and save wider format comparison
  comparison_wide <- combined %>%
    select(Subgroup, Approach, Prop_CI, k) %>%
    pivot_wider(id_cols = Subgroup, names_from = Approach, 
                values_from = c(Prop_CI, k), names_sep = "_")
  
  write_csv(comparison_wide, glue("output/r_results/comparisons/subgroup_{var}_comparison.csv"))
  
  # Test heterogeneity within each subgroup across approaches
  subgroup_heterogeneity <- combined %>%
    group_by(Subgroup) %>%
    filter(n_distinct(Approach) >= 2) %>%
    group_map(~{
      # Only test if we have at least 2 approaches for this subgroup
      if(n_distinct(.x$Approach) >= 2) {
        test <- rma(yi = yi, vi = vi, data = .x)
        
        tibble(
          Variable = var,
          Subgroup = .y$Subgroup,
          Q_statistic = test$QE,
          df = test$k - 1,
          P_value = test$QEp,
          Significant = test$QEp < 0.05,
          I2 = test$I2,
          H2 = test$H2,
          tau2 = test$tau2,
          Approaches = paste(.x$Approach, collapse = ", ")
        )
      }
    }) %>%
    bind_rows()
  
  return(subgroup_heterogeneity)
}

# Compare method-related subgroups (primary vs secondary only)
method_heterogeneity <- map_dfr(method_vars, ~compare_subgroups(.x, methods_only = TRUE))

# Compare sample-related subgroups across all approaches
sample_heterogeneity <- map_dfr(sample_vars, ~compare_subgroups(.x))

# Combine all heterogeneity results
all_heterogeneity <- bind_rows(
  heterogeneity_results,
  method_heterogeneity,
  sample_heterogeneity
)

write_csv(all_heterogeneity, "output/r_results/comparisons/heterogeneity_tests.csv")


# Compare temporal trends
cat("\nComparing temporal trends...\n")
temporal_data <- map(approaches, ~{
  path <- glue("output/r_results/{.x}/temporal_trends.csv")
  if(file.exists(path)) {
    read_csv(path, show_col_types = FALSE) %>% 
      mutate(Approach = .x,
             yi = log(Pooled_Proportion / (1 - Pooled_Proportion)),
             vi = (CI_Upper - CI_Lower)^2 / (2 * 1.96)^2)
  }
}) %>% 
  compact()

if(length(temporal_data) >= 2) {
  # Create comparison table
  combined_temporal <- bind_rows(temporal_data) %>%
    mutate(Year = as.numeric(Subgroup),
           Prop_CI = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]"))
  
  # Create and save wide format
  temporal_wide <- combined_temporal %>%  
    select(Year, Approach, Prop_CI, k) %>%
    pivot_wider(id_cols = Year, names_from = Approach, 
                values_from = c(Prop_CI, k), names_sep = "_") %>%
    arrange(Year)
  
  write_csv(temporal_wide, "output/r_results/comparisons/temporal_comparison.csv")
  
  # Analyze temporal trends for each approach
  temporal_results <- map_dfr(temporal_data, function(data) {
    trend_model <- data %>%
      mutate(Year = as.numeric(Subgroup) - min(as.numeric(Subgroup))) %>%
      {rma(yi = yi, vi = vi, mods = ~ Year, data = ., method = "REML")}
    
    tibble(
      Approach = data$Approach[1],
      Intercept = trend_model$b[1],
      Slope = trend_model$b[2],
      SE_slope = trend_model$se[2],
      Z_value = trend_model$zval[2],
      P_value = trend_model$pval[2],
      Direction = if_else(trend_model$b[2] > 0, "increasing", "decreasing"),
      Significant = trend_model$pval[2] < 0.05
    )
  })
  
  write_csv(temporal_results, "output/r_results/comparisons/temporal_trend_analysis.csv")
  
  # Test for heterogeneity in temporal trends across approaches
  year_heterogeneity <- combined_temporal %>%
    group_by(Year) %>%
    filter(n_distinct(Approach) >= 2) %>%
    group_map(~{
      if(n_distinct(.x$Approach) >= 2) {
        test <- rma(yi = yi, vi = vi, data = .x)
        
        tibble(
          Variable = "year",
          Subgroup = as.character(.y$Year),
          Q_statistic = test$QE,
          df = test$k - 1,
          P_value = test$QEp,
          Significant = test$QEp < 0.05,
          I2 = test$I2,
          H2 = test$H2,
          tau2 = test$tau2,
          Approaches = paste(.x$Approach, collapse = ", ")
        )
      }
    }) %>%
    bind_rows()
  
  # Add year heterogeneity to the comprehensive results
  all_heterogeneity <- bind_rows(all_heterogeneity, year_heterogeneity)
  write_csv(all_heterogeneity, "output/r_results/comparisons/heterogeneity_tests.csv")
}

cat("\nComparison of meta-analysis approaches complete. Results saved to output/r_results/comparisons/\n")
cat("  - approach_comparison.csv: Overall approach statistics\n")
cat("  - pairwise_statistical_tests.csv: Formal statistical tests between approaches\n")
cat("  - heterogeneity_tests.csv: Tests for heterogeneity between approaches\n")
if(length(temporal_data) >= 2) {
  cat("  - temporal_trend_analysis.csv: Analysis of time trends by approach\n")
}
cat("  - Various subgroup comparison files in the comparisons directory\n")