# combined_meta_analysis.R
#
# This script performs meta-analyses using three analytical approaches:
# 1. First-Method Approach: Single-method studies + first methods from sequential screening
# 2. Single-Method Approach: Only single-method studies (maximum internal validity)
# 3. Overall Approach: Total CR amounts regardless of method configuration
#
# Across all approaches, the same workflow is used.
# 1. Run overall meta-analysis
# 2. Run subgroup analyses
# 3. Run temporal trends analysis
#

library(tidyverse)
library(metafor)
library(glue)

# Define approach-specific parameters
approaches <- list(
  first_method = list(
    name = "First-Method",
    description = "Using single-method studies and first methods from sequential screening",
    data_path = "data/for_r_meta/first_method_data.csv",
    output_dir = "output/r_results/primary",
    include_method_subgroups = TRUE,
    primary = TRUE
  ),
  single_method = list(
    name = "Single-Method",
    description = "Using only single-method studies for maximum internal validity",
    data_path = "data/for_r_meta/single_method_data.csv",
    output_dir = "output/r_results/secondary",
    include_method_subgroups = TRUE,
    primary = FALSE
  ),
  overall = list(
    name = "Overall",
    description = "Using total CR amounts regardless of method configuration",
    data_path = "data/for_r_meta/overall_data.csv",
    output_dir = "output/r_results/overall",
    include_method_subgroups = FALSE,
    primary = FALSE
  )
)

load_codebook <- \(path = "codebook.json") jsonlite::read_json(path)

run_meta_analysis <- function(data, method = "REML") {
  result <- rma(yi = logit_prop, vi = var_logit, data = data, method = method)
  list(
    pooled_prop = as.numeric(transf.ilogit(result$b)),
    ci_lb = as.numeric(transf.ilogit(result$ci.lb)), 
    ci_ub = as.numeric(transf.ilogit(result$ci.ub)),
    k = as.integer(result$k), 
    n = as.integer(sum(data$sample_size, na.rm = TRUE)),
    tau2 = as.numeric(result$tau2), 
    i2 = as.numeric(result$I2), 
    h2 = as.numeric(result$H2), 
    q = as.numeric(result$QE), 
    p_q = as.numeric(result$QEp)
  )
}

run_subgroup_analysis <- function(data, var, codebook = NULL) {
  if (is.null(codebook)) codebook <- load_codebook()
  
  unique_vals <- data %>% 
    pull(!!sym(var)) %>% 
    unique() %>% 
    na.omit()
    
  base_var <- if(str_ends(var, "_name")) str_replace(var, "_name$", "") else var
  mapping <- if(base_var %in% names(codebook)) codebook[[base_var]] else NULL
  
  results <- map(unique_vals, function(val) {
    subset <- data %>% filter(!!sym(var) == val)
    if (nrow(subset) < 2) return(NULL)
    
    display_name <- val
    if (!is.null(mapping) && (is.numeric(val) || val == "-1" || 
        (is.character(val) && grepl("^-?\\d+$", val)))) {
      val_str <- as.character(val)
      if (val_str %in% names(mapping)) display_name <- mapping[[val_str]]
    }
    
    meta <- run_meta_analysis(subset)
    tibble(
      Subgroup = as.character(display_name),
      Pooled_Proportion = meta$pooled_prop, CI_Lower = meta$ci_lb, CI_Upper = meta$ci_ub,
      k = meta$k, N = meta$n, tau2 = meta$tau2, I2 = meta$i2, H2 = meta$h2, 
      Q = meta$q, p_Q = meta$p_q
    )
  }) %>% compact() %>% bind_rows() %>% arrange(desc(k))
  
  if (nrow(results) == 0) {
    return(tibble(Subgroup = character(), Pooled_Proportion = numeric(), 
                 CI_Lower = numeric(), CI_Upper = numeric(), k = integer()))
  }
  
  return(results)
}

# Process a single approach
process_approach <- function(approach_params) {
  cat("\nPROCESSING", approach_params$name, "APPROACH:\n")
  cat(approach_params$description, "\n")
  dir.create(approach_params$output_dir, recursive = TRUE, showWarnings = FALSE)
  
  data <- read_csv(approach_params$data_path, show_col_types = FALSE)
  cat(glue("  Loaded {nrow(data)} studies with {sum(data$sample_size)} participants\n"))
  
  overall_results <- run_meta_analysis(data)
  cat("\nOverall Results:\n")
  cat(glue("  Pooled Proportion: {round(overall_results$pooled_prop * 100, 2)}% (95% CI: {round(overall_results$ci_lb * 100, 2)}%-{round(overall_results$ci_ub * 100, 2)}%)\n"))
  cat(glue("  Based on {overall_results$k} studies with {overall_results$n} participants\n"))
  cat(glue("  Heterogeneity: I² = {round(overall_results$i2, 1)}%, Q = {round(overall_results$q, 2)} (p{ifelse(overall_results$p_q < 0.001, '< 0.001', paste0('= ', round(overall_results$p_q, 3)))})\n"))
  
  tibble(
    Analysis = approach_params$name,
    Pooled_Proportion = overall_results$pooled_prop, CI_Lower = overall_results$ci_lb,
    CI_Upper = overall_results$ci_ub, k = overall_results$k, N = overall_results$n,
    tau2 = overall_results$tau2, I2 = overall_results$i2, H2 = overall_results$h2,
    Q = overall_results$q, p_Q = overall_results$p_q
  ) %>% write_csv(file.path(approach_params$output_dir, "overall_results.csv"))
  
  method_vars <- c("method_type", "cr_method", "method_timing")
  sample_vars <- c("sample_source", "sample_recruitment", "sample_platform", "sample_method",
                  "journal", "sample_level", "sample_incentive", "sample_country", 
                  "design_method", "design_location")
  
  subgroup_vars <- if(approach_params$include_method_subgroups) {
    c(method_vars, sample_vars)
  } else {
    sample_vars
  }
  
  cat("\nRunning subgroup analyses...\n")
  codebook <- load_codebook()
  
  walk(subgroup_vars, ~{
    if (.x %in% colnames(data)) {
      cat(glue("\n  Analyzing by {.x}:\n"))
      results <- run_subgroup_analysis(data, .x, codebook)
      write_csv(results, file.path(approach_params$output_dir, glue("subgroup_{.x}.csv")))
    }
  })
  
  cat("\nAnalyzing temporal trends...\n")
  year_results <- run_subgroup_analysis(data, "year")
  write_csv(year_results, file.path(approach_params$output_dir, "temporal_trends.csv"))
  
  cat(glue("\n{approach_params$name} analysis complete. Results saved to {approach_params$output_dir}/\n"))
  
  return(list(
    name = approach_params$name,
    overall = overall_results,
    data = data
  ))
}

results <- map(approaches, process_approach)

cat("\nAll meta-analyses complete!\n")