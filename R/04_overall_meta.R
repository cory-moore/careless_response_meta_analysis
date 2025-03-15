###############################################################################
# 04_overall_meta.R
# 
# This script implements the meta-analysis using the Overall/Total 
# approach. It performs:
# 1. Overall random-effects meta-analysis using total CR amounts
# 2. Subgroup analyses by various dimensions
#
# The Overall approach captures all available data regardless of method 
# configuration (single method, sequential, non-sequential) by using the total 
# careless responding amount from each study. This approach maximizes data 
# utilization but may conflate method effectiveness with position effects.
#
# Results are saved in CSV format for further analysis and visualization.
###############################################################################

library(tidyverse)
library(metafor)
library(glue)

overall_data <- read_csv("data/for_r_meta/overall_data.csv", show_col_types = FALSE)
cat(glue("Loaded Overall dataset: {nrow(overall_data)} studies\n"))

# Add method_type if it doesn't exist
if (!"method_type" %in% colnames(overall_data)) {
  # First, check which column might contain method information
  method_column <- NULL
  for (col in c("cr_method", "method_code")) {
    if (col %in% colnames(overall_data)) {
      method_column <- col
      break
    }
  }
  
  if (!is.null(method_column)) {
    # Create method type mapping based on the codebook
    cat(glue("Creating method_type from {method_column}\n"))
    
    # Function to determine method type for each code
    overall_data <- overall_data %>%
      mutate(method_type = case_when(
        !!sym(method_column) == 1 ~ "response_time",
        !!sym(method_column) %in% c(4, 5) ~ "outlier_analysis",
        !!sym(method_column) %in% c(0, 12, 13) ~ "attention_check_items",
        !!sym(method_column) %in% c(9, 10, 11, 15, 16) ~ "consistency_indices",
        !!sym(method_column) %in% c(2, 3, 14) ~ "response_pattern",
        !!sym(method_column) %in% c(6, 7, 8) ~ "self_reported",
        TRUE ~ "other"
      ))
    
    cat("Created method_type variable\n")
  } else {
    cat("Could not find method column to create method_type\n")
  }
}

run_meta_analysis <- function(data, method = "DL") {
  # Random-effects meta-analysis using metafor
  meta_result <- rma(yi = logit_prop, 
                    vi = var_logit, 
                    data = data, 
                    method = method)
  
  # Back-transform to proportion scale
  pooled_prop <- transf.ilogit(meta_result$b)
  ci_lb <- transf.ilogit(meta_result$ci.lb)
  ci_ub <- transf.ilogit(meta_result$ci.ub)
  
  return(list(
    pooled_prop = pooled_prop,
    ci_lb = ci_lb,
    ci_ub = ci_ub,
    k = meta_result$k,
    n = sum(data$sample_size, na.rm = TRUE),
    tau2 = meta_result$tau2,
    i2 = meta_result$I2,
    h2 = meta_result$H2,
    q = meta_result$QE,
    p_q = meta_result$QEp
  ))
}

run_subgroup_analysis <- function(data, grouping_var) {
  unique_values <- data %>% 
    pull(!!sym(grouping_var)) %>% 
    unique() %>% 
    na.omit()
  
  cat(glue("  Running subgroup analysis for {grouping_var} with {length(unique_values)} levels\n"))
  
  results_list <- list()
  
  for (val in unique_values) {
    subset_data <- data %>% filter(!!sym(grouping_var) == val)
    
    if (nrow(subset_data) < 2) {
      cat(glue("    Skipping {grouping_var} = {val} with only {nrow(subset_data)} studies\n"))
      next
    }
    
    cat(glue("    Analyzing {grouping_var} = {val} with {nrow(subset_data)} studies\n"))
    
    meta_results <- run_meta_analysis(subset_data)
    
    results_list[[length(results_list) + 1]] <- tibble(
      Subgroup = as.character(val),
      Pooled_Proportion = as.numeric(meta_results$pooled_prop),
      CI_Lower = as.numeric(meta_results$ci_lb),
      CI_Upper = as.numeric(meta_results$ci_ub),
      k = as.integer(meta_results$k),
      N = as.integer(meta_results$n),
      tau2 = as.numeric(meta_results$tau2),
      I2 = as.numeric(meta_results$i2),
      H2 = as.numeric(meta_results$h2),
      Q = as.numeric(meta_results$q),
      p_Q = as.numeric(meta_results$p_q)
    )
  }
  
  if (length(results_list) > 0) {
    return(bind_rows(results_list) %>% arrange(desc(k)))
  } else {
    return(tibble(
      Subgroup = character(),
      Pooled_Proportion = numeric(),
      CI_Lower = numeric(),
      CI_Upper = numeric(),
      k = integer(),
      N = integer(),
      tau2 = numeric(),
      I2 = numeric(),
      H2 = numeric(),
      Q = numeric(),
      p_Q = numeric()
    ))
  }
}

cat("\nRUNNING META-ANALYSIS (OVERALL APPROACH):\n")
cat("-------------------------------------------\n")
overall_results <- run_meta_analysis(overall_data)

cat("\nOverall Results (Overall/Total Approach):\n")
cat(glue("  Pooled Proportion: {round(overall_results$pooled_prop * 100, 2)}% (95% CI: {round(overall_results$ci_lb * 100, 2)}%-{round(overall_results$ci_ub * 100, 2)}%)\n"))
cat(glue("  Based on {overall_results$k} studies with {overall_results$n} participants\n"))
cat(glue("  Heterogeneity: I² = {round(overall_results$i2, 1)}%, Q = {round(overall_results$q, 2)} (p{ifelse(overall_results$p_q < 0.001, '< 0.001', paste0('= ', round(overall_results$p_q, 3)))})\n"))

# Create the overall results dataframe with only atomic columns
overall_results_df <- tibble(
  Analysis = "Overall",
  Pooled_Proportion = as.numeric(overall_results$pooled_prop),
  CI_Lower = as.numeric(overall_results$ci_lb),
  CI_Upper = as.numeric(overall_results$ci_ub),
  k = as.integer(overall_results$k),
  N = as.integer(overall_results$n),
  tau2 = as.numeric(overall_results$tau2),
  I2 = as.numeric(overall_results$i2),
  H2 = as.numeric(overall_results$h2),
  Q = as.numeric(overall_results$q),
  p_Q = as.numeric(overall_results$p_q)
)

write_csv(overall_results_df, "output/r_results/overall/overall_results.csv")

cat("\nRunning subgroup analyses...\n")
subgroup_variables <- c(
  "method_type", 
  "sample_source_name", 
  "sample_platform_name", 
  "sample_method_name",
  "journal_name"
)

subgroup_results <- subgroup_variables %>%
  set_names() %>%
  map(~{
    if (.x %in% colnames(overall_data)) {
      cat(glue("\nAnalyzing by {.x}:\n"))
      results <- run_subgroup_analysis(overall_data, .x)
      write_csv(results, glue("output/r_results/overall/subgroup_{.x}.csv"))
      results
    } else {
      cat(glue("Skipping {.x} - not found in dataset\n"))
      NULL
    }
  }) %>%
  compact()

if ("year" %in% colnames(overall_data)) {
  cat("\nAnalyzing temporal trends...\n")
  year_results <- run_subgroup_analysis(overall_data, "year")
  write_csv(year_results, "output/r_results/overall/temporal_trends.csv")
}

cat("\nOverall meta-analysis complete. Results saved to output/r_results/overall/\n")