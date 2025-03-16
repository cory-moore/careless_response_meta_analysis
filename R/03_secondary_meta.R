###############################################################################
# 03_secondary_meta.R
# 
# This script implements the SECONDARY meta-analysis using the Single-Method 
# approach. It performs:
# 1. Overall random-effects meta-analysis for Single-Method studies only
# 2. Subgroup analyses by method type, sample source, etc.
#
# The Single-Method approach is a sensitivity analysis that includes:
# - Only studies using a single CR detection method (cr_multiple = 0)
# 
# This provides the highest internal validity by completely eliminating 
# order effects concerns, though at the cost of a smaller sample.
#
# Results are saved in CSV format for further analysis and visualization.
###############################################################################

library(tidyverse)
library(metafor)
library(glue)

single_method_data <- read_csv("data/for_r_meta/single_method_data.csv", show_col_types = FALSE)
cat(glue("Loaded Single-Method dataset: {nrow(single_method_data)} studies\n"))

load_codebook <- \(path = "codebook.json") jsonlite::read_json(path)

run_meta_analysis <- function(data, method = "DL") {
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
  cat(glue("  Running subgroup analysis for {var} with {length(unique_vals)} levels\n"))
  
  base_var <- if(str_ends(var, "_name")) str_replace(var, "_name$", "") else var
  mapping <- if(base_var %in% names(codebook)) codebook[[base_var]] else NULL
  
  results <- map(unique_vals, function(val) {
    subset <- data %>% 
      filter(!!sym(var) == val)
    if (nrow(subset) < 2) {
      cat(glue("    Skipping {var} = {val} with only {nrow(subset)} studies\n"))
      return(NULL)
    }
    cat(glue("    Analyzing {var} = {val} with {nrow(subset)} studies\n"))
    
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
  }) %>% 
    compact()
  
  if (length(results) > 0) {
    bind_rows(results) %>% 
      arrange(desc(k))
  } else {
    tibble(Subgroup = character(), Pooled_Proportion = numeric(), 
           CI_Lower = numeric(), CI_Upper = numeric(), k = integer())
  }
}

# Run overall meta-analysis
cat("\nRUNNING SECONDARY META-ANALYSIS (SINGLE-METHOD APPROACH):\n")
overall_results <- run_meta_analysis(single_method_data)

cat("\nOverall Results (Single-Method Approach):\n")
cat(glue("  Pooled Proportion: {round(overall_results$pooled_prop * 100, 2)}% (95% CI: {round(overall_results$ci_lb * 100, 2)}%-{round(overall_results$ci_ub * 100, 2)}%)\n"))
cat(glue("  Based on {overall_results$k} studies with {overall_results$n} participants\n"))
cat(glue("  Heterogeneity: I² = {round(overall_results$i2, 1)}%, Q = {round(overall_results$q, 2)} (p{ifelse(overall_results$p_q < 0.001, '< 0.001', paste0('= ', round(overall_results$p_q, 3)))})\n"))

# Save overall results
tibble(
  Analysis = "Single-Method",
  Pooled_Proportion = overall_results$pooled_prop, CI_Lower = overall_results$ci_lb,
  CI_Upper = overall_results$ci_ub, k = overall_results$k, N = overall_results$n,
  tau2 = overall_results$tau2, I2 = overall_results$i2, H2 = overall_results$h2,
  Q = overall_results$q, p_Q = overall_results$p_q
) %>% 
  write_csv("output/r_results/secondary/overall_results.csv")

# Define and run subgroup analyses
cat("\nRunning subgroup analyses...\n")
codebook <- load_codebook()

subgroup_variables <- c("method_type", "sample_source", "sample_recruitment", 
                        "sample_platform", "sample_method", "journal", "sample_level", 
                        "sample_incentive", "sample_country", "design_method", 
                        "design_location")

walk(subgroup_variables, ~{
  if (.x %in% colnames(single_method_data)) {
    cat(glue("\nAnalyzing by {.x}:\n"))
    results <- run_subgroup_analysis(single_method_data, .x, codebook)
    write_csv(results, glue("output/r_results/secondary/subgroup_{.x}.csv"))
  } else {
    cat(glue("Skipping {.x} - not found in dataset\n"))
  }
})

# Run temporal trends analysis
cat("\nAnalyzing temporal trends...\n")
year_results <- run_subgroup_analysis(single_method_data, "year")
write_csv(year_results, "output/r_results/secondary/temporal_trends.csv")

cat("\nSecondary meta-analysis complete. Results saved to output/r_results/secondary/\n")