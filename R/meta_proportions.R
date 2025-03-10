###############################################################################
# R script for random-effects meta-analysis on proportion data
#
# This script performs a meta-analysis on proportions of careless 
# responding in survey data using random-effects models. It implements the 
# DerSimonian-Laird method for pooling proportions across studies while 
# accounting for between-study heterogeneity.
#
# Workflow:
# 1. Load required packages:
#    - dplyr: For data manipulation and transformation
#    - openxlsx: For reading/writing Excel files
#    - metafor: For meta-analysis functions (rma)
#    - meta: For additional meta-analysis utilities
#
# 2. Define `meta_analysis` function that:
#    - Takes proportion values and sample sizes as inputs
#    - Performs data validation and cleaning
#    - Transforms proportions using logit transformation for analysis
#    - Calculates study-specific variances based on sample size and proportion
#    - Fits random-effects model using DerSimonian-Laird estimator
#    - Back-transforms results to proportion scale
#    - Returns pooled proportion, standard error, confidence intervals,
#      heterogeneity statistics (I² and Q), and sample information
#
# 3. Define `analyze_group` function that:
#    - Reads data from Excel sheets for specific categories
#    - Calculates overall pooled proportions for each group
#    - Performs subgroup analyses within each category
#    - Handles errors gracefully with informative messages
#
# 4. Define `main` function that:
#    - Initializes results data frame
#    - Processes overall analysis
#    - Defines group configurations
#    - Processes all groups
#    - Saves results to CSV
#
###############################################################################

library(dplyr)
library(openxlsx)
library(metafor)
library(meta)

meta_analysis <- function(p_values, n_values) {
  if (length(p_values) == 0 || all(is.na(p_values))) {
    return(list(
      pooled_p = NA, pooled_se = NA, lower_ci = NA, upper_ci = NA,
      n_sum = sum(n_values, na.rm = TRUE), i_squared = NA, q = NA,
      n_studies = length(p_values)
    ))
  }
  
  meta_data <- data.frame(p = p_values, n = n_values)
  meta_data <- meta_data[!is.na(meta_data$p) & !is.na(meta_data$n) & meta_data$n > 0, ]
  
  if (nrow(meta_data) == 0) {
    return(list(
      pooled_p = NA, pooled_se = NA, lower_ci = NA, upper_ci = NA,
      n_sum = sum(n_values, na.rm = TRUE), i_squared = NA, q = NA,
      n_studies = length(p_values)
    ))
  }
  
  meta_data$p <- pmin(pmax(meta_data$p, 0.00001), 0.99999)
  
  meta_data$logit_p <- log(meta_data$p / (1 - meta_data$p))
  meta_data$var <- 1 / (meta_data$n * meta_data$p * (1 - meta_data$p))
  
  tryCatch({
    rma_result <- rma(yi = logit_p, vi = var, data = meta_data, method = "DL")
    
    pooled_logit <- rma_result$b[1]
    pooled_p <- exp(pooled_logit) / (1 + exp(pooled_logit))
    
    logit_ci <- c(rma_result$ci.lb, rma_result$ci.ub)
    ci <- exp(logit_ci) / (1 + exp(logit_ci))
    
    i_squared <- rma_result$I2
    q_stat <- rma_result$QE
    
    return(list(
      pooled_p = round(pooled_p, 4),
      pooled_se = round(rma_result$se, 4),
      lower_ci = round(ci[1], 4),
      upper_ci = round(ci[2], 4),
      n_sum = sum(meta_data$n),
      i_squared = round(i_squared, 2),
      q = round(q_stat, 2),
      n_studies = nrow(meta_data)
    ))
  }, error = function(e) {
    cat("Error in primary meta-analysis:", e$message, "\n")
    return(list(
      pooled_p = NA, pooled_se = NA, lower_ci = NA, upper_ci = NA,
      n_sum = sum(meta_data$n), i_squared = NA, q = NA,
      n_studies = nrow(meta_data)
    ))
  })
}

analyze_group <- function(sheet_name, group_name, subgroup_col, proportion_col, results_df) {
  tryCatch({
    df <- read.xlsx("output/proportions_r.xlsx", sheet = sheet_name)
    
    if (nrow(df) > 0) {
      p <- df[[proportion_col]]
      n <- df$sample_size
      
      meta_results <- meta_analysis(p, n)
      
      results_df <- rbind(results_df, data.frame(
        Group = group_name,
        Subgroup = "Total",
        'Pooled Prevalence' = meta_results$pooled_p,
        'Pooled Standard Error' = meta_results$pooled_se,
        'Lower CI' = meta_results$lower_ci,
        'Upper CI' = meta_results$upper_ci,
        'Pooled Sample Size' = meta_results$n_sum,
        'Number of Studies' = meta_results$n_studies,
        'I-squared (%)' = meta_results$i_squared,
        'Q Statistic' = meta_results$q,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.null(subgroup_col) && subgroup_col %in% colnames(df)) {
      for (subgroup in unique(df[[subgroup_col]])) {
        if (!is.na(subgroup)) {
          subset_df <- df[df[[subgroup_col]] == subgroup, ]
          
          if (nrow(subset_df) > 0) {
            p <- subset_df[[proportion_col]]
            n <- subset_df$sample_size
            
            meta_results <- meta_analysis(p, n)
            
            results_df <- rbind(results_df, data.frame(
              Group = group_name,
              Subgroup = as.character(subgroup),
              'Pooled Prevalence' = meta_results$pooled_p,
              'Pooled Standard Error' = meta_results$pooled_se,
              'Lower CI' = meta_results$lower_ci,
              'Upper CI' = meta_results$upper_ci,
              'Pooled Sample Size' = meta_results$n_sum,
              'Number of Studies' = meta_results$n_studies,
              'I-squared (%)' = meta_results$i_squared,
              'Q Statistic' = meta_results$q,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
    
    return(results_df)
  }, error = function(e) {
    cat("Error analyzing group", group_name, ":", e$message, "\n")
    return(results_df)
  })
}

main <- function() {
  columns <- c(
    'Group', 'Subgroup', 'Pooled Prevalence', 'Pooled Standard Error',
    'Lower CI', 'Upper CI', 'Pooled Sample Size', 'Number of Studies',
    'I-squared (%)', 'Q Statistic'
  )
  pooled_results <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(pooled_results) <- columns
  
  total_df <- read.xlsx("output/proportions_r.xlsx", sheet = "proportions_total")
  p <- total_df$proportions_total
  n <- total_df$sample_size
  
  meta_results <- meta_analysis(p, n)
  
  pooled_results <- rbind(pooled_results, data.frame(
    Group = "Overall",
    Subgroup = "",
    'Pooled Prevalence' = meta_results$pooled_p,
    'Pooled Standard Error' = meta_results$pooled_se,
    'Lower CI' = meta_results$lower_ci,
    'Upper CI' = meta_results$upper_ci,
    'Pooled Sample Size' = meta_results$n_sum,
    'Number of Studies' = meta_results$n_studies,
    'I-squared (%)' = meta_results$i_squared,
    'Q Statistic' = meta_results$q,
    stringsAsFactors = FALSE
  ))
  
  groups <- list(
    list(sheet = "proportions_year", name = "Year", col = "year", prop_col = "proportions_year"),
    list(sheet = "proportions_journal", name = "Journal", col = "journal_name", prop_col = "proportions_journal"),
    list(sheet = "proportions_sample_source", name = "Sample Source", col = "sample_source_name", prop_col = "proportions_sample_source"),
    list(sheet = "proportions_sample_method", name = "Sample Method", col = "sample_method_name", prop_col = "proportions_sample_method"),
    list(sheet = "proportions_platform", name = "Sample Platform", col = "sample_platform_name", prop_col = "proportions_platform"),
    list(sheet = "proportions_cr_method", name = "Careless Response Method", col = "cr_method_name", prop_col = "proportions_cr_method")
  )
  
  for (group in groups) {
    pooled_results <- analyze_group(group$sheet, group$name, group$col, group$prop_col, pooled_results)
  }
  
  write.csv(pooled_results, "output/random_effects_pooled_proportions_R.csv", row.names = FALSE)
}

main()