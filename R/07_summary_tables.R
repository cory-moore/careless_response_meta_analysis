# comprehensive_summary.R
#
# This script compiles all meta-analysis results into a comprehensive
# Excel workbook for reporting. It includes:
# 
# - Overall results for all analytical approaches
# - Subgroup analyses by method, sample, and study characteristics
# - Meta-regression moderator analyses 
# - Multilevel modeling and variance components
# - Position effects in sequential screening
# - Publication bias assessments
# - Sensitivity analyses
#
# Results are organized into thematic worksheets with consistent formatting,
# ready for inclusion in the dissertation.

library(tidyverse)
library(openxlsx)
library(glue)

# Create output directory
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Helper Functions --------------------------------------------------------

# Read results with robust error handling
read_result <- function(path, default = NULL) {
  if(!file.exists(path)) {
    message(glue("File not found: {path}"))
    return(default)
  }
  
  tryCatch({
    read_csv(path, show_col_types = FALSE)
  }, error = function(e) {
    message(glue("Error reading {path}: {e$message}"))
    return(default)
  })
}

# Format numeric columns for readability
format_percentage <- function(x, digits = 2) {
  # Vectorized approach to handle both proportions and percentages
  result <- rep(NA_character_, length(x))
  
  # For NA values
  result[is.na(x)] <- "NA%"
  
  # For values that appear to be already in percentage form (>1)
  idx_pct <- !is.na(x) & x > 1
  result[idx_pct] <- paste0(round(x[idx_pct], digits), "%")
  
  # For proportion values
  idx_prop <- !is.na(x) & x <= 1
  result[idx_prop] <- paste0(round(x[idx_prop] * 100, digits), "%")
  
  return(result)
}

format_ci <- function(lower, upper, digits = 2) {
  paste0("[", format_percentage(lower, digits), " - ", 
         format_percentage(upper, digits), "]")
}

format_pvalue <- function(p) {
  ifelse(p < 0.001, "< 0.001", round(p, 3))
}

# Add formatted worksheet with consistent styling
add_formatted_worksheet <- function(wb, sheet_name, data, title = NULL,
                                   start_row = 1, notes = NULL) {
  # Create worksheet
  addWorksheet(wb, sheet_name)
  
  # Add title if provided
  if(!is.null(title)) {
    writeData(wb, sheet_name, title, startRow = start_row)
    start_row <- start_row + 2
  }
  
  # Write data
  writeData(wb, sheet_name, data, startRow = start_row)
  
  # Add notes if provided
  if(!is.null(notes)) {
    writeData(wb, sheet_name, notes, 
              startRow = start_row + nrow(data) + 2)
  }
  
  # Apply styling
  addStyle(wb, sheet_name, 
           style = createStyle(textDecoration = "bold", fontSize = 14),
           rows = 1, cols = 1)
  
  # Style header row
  header_row <- if(is.null(title)) start_row else start_row
  addStyle(wb, sheet_name,
           style = createStyle(fgFill = "#D9E1F2", border = "Bottom",
                              borderColour = "#4472C4", 
                              borderStyle = "medium",
                              textDecoration = "bold"),
           rows = header_row, cols = 1:ncol(data), gridExpand = TRUE)
  
  # Auto-fit columns
  setColWidths(wb, sheet_name, cols = 1:ncol(data), widths = "auto")
  
  return(start_row + nrow(data))
}

# Section Processors ------------------------------------------------------

process_overall_results <- function(wb) {
  # Load results from each approach
  approaches <- c("primary" = "First-Method", 
                 "secondary" = "Single-Method", 
                 "overall" = "Overall")
  
  results <- map_dfr(names(approaches), ~{
    path <- glue("output/r_results/{.x}/overall_results.csv")
    result <- read_result(path)
    if(!is.null(result)) result$Approach <- approaches[.x]
    result
  })
  
  # Load comparison results
  comparison <- read_result("output/r_results/comparisons/approach_comparison.csv")
  pairwise_tests <- read_result("output/r_results/comparisons/pairwise_statistical_tests.csv")
  
  # Format for readability
  if(!is.null(results) && nrow(results) > 0) {
    formatted_results <- results %>%
      mutate(
        `Pooled Proportion` = format_percentage(Pooled_Proportion),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `Studies (k)` = k,
        `Sample Size (N)` = N,
        `I²` = format_percentage(I2, 1),
        `τ²` = round(tau2, 5),
        `Q Statistic` = round(Q, 2),
        `p-value` = format_pvalue(p_Q)
      ) %>%
      select(Approach, `Pooled Proportion`, `95% CI`, `Studies (k)`, 
             `Sample Size (N)`, `I²`, `τ²`, `Q Statistic`, `p-value`) %>%
      arrange(match(Approach, approaches))
    
    # Format pairwise tests if available
    formatted_tests <- NULL
    if(!is.null(pairwise_tests) && nrow(pairwise_tests) > 0) {
      formatted_tests <- pairwise_tests %>%
        mutate(
          `Absolute Difference` = format_percentage(Absolute_Diff),
          `Relative Difference` = paste0(round(Percentage_Diff, 1), "%"),
          `CI Overlap` = ifelse(CI_Overlap, "Yes", "No"),
          `Z-statistic` = round(Z_statistic, 2),
          `p-value` = format_pvalue(P_value),
          `Significant` = ifelse(Significant, "Yes", "No")
        ) %>%
        select(Comparison, `Absolute Difference`, `Relative Difference`,
               `CI Overlap`, `Z-statistic`, `p-value`, `Significant`)
    }
    
    # Add to workbook
    add_formatted_worksheet(
      wb, "Overall Results", formatted_results,
      title = "Table 1: Overall Results by Analytical Approach",
      notes = paste0(
        "Notes: Pooled proportions represent the estimated prevalence of careless responding. ",
        "I² indicates the percentage of variation due to true heterogeneity rather than sampling error. ",
        "The Q statistic tests the null hypothesis that all studies share a common effect size."
      )
    )
    
    # Add pairwise tests if available
    if(!is.null(formatted_tests) && nrow(formatted_tests) > 0) {
      add_formatted_worksheet(
        wb, "Approach Comparisons", formatted_tests,
        title = "Table 1b: Statistical Comparisons Between Approaches",
        notes = paste0(
          "Notes: The Z-statistic tests whether the difference between approaches is statistically significant. ",
          "Absolute difference represents the raw percentage point difference, while relative difference ",
          "indicates this as a percentage of the smaller estimate."
        )
      )
    }
  }
}

process_method_results <- function(wb) {
  # Load method results from primary and secondary approaches
  approaches <- c("primary" = "First-Method", "secondary" = "Single-Method")
  
  results <- map_dfr(names(approaches), ~{
    path <- glue("output/r_results/{.x}/subgroup_method_type.csv")
    result <- read_result(path)
    if(!is.null(result) && nrow(result) > 0) {
      result$Approach <- approaches[.x]
    }
    result
  })
  
  if(!is.null(results) && nrow(results) > 0) {
    formatted_results <- results %>%
      mutate(
        `Method Type` = Subgroup,
        `Pooled Proportion` = format_percentage(Pooled_Proportion),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `Studies (k)` = k,
        `Sample Size (N)` = N,
        `I²` = format_percentage(I2, 1),
        `τ²` = round(tau2, 5)
      ) %>%
      select(Approach, `Method Type`, `Pooled Proportion`, `95% CI`, 
             `Studies (k)`, `Sample Size (N)`, `I²`, `τ²`) %>%
      arrange(Approach, desc(`Pooled Proportion`))
    
    add_formatted_worksheet(
      wb, "Method Analysis", formatted_results,
      title = "Table 2: Careless Responding Rates by Detection Method Type",
      notes = paste0(
        "Notes: This table presents careless responding rates by detection method type. ",
        "Only First-Method and Single-Method approaches are included because method-level analysis ",
        "is not conceptually appropriate for the Overall approach, which aggregates across methods."
      )
    )
  }
}

process_sample_results <- function(wb) {
  # Define sample characteristic variables
  sample_vars <- c("sample_source", "sample_platform", "sample_recruitment")
  var_labels <- c("Sample Source", "Sample Platform", "Sample Recruitment")
  
  # Load results from all approaches
  approaches <- c("primary" = "First-Method", 
                 "secondary" = "Single-Method", 
                 "overall" = "Overall")
  
  results <- map_dfr(names(approaches), function(approach) {
    map2_dfr(sample_vars, var_labels, function(var, label) {
      path <- glue("output/r_results/{approach}/subgroup_{var}.csv")
      result <- read_result(path)
      if(!is.null(result) && nrow(result) > 0) {
        result$Approach <- approaches[approach]
        result$Characteristic <- label
      }
      result
    })
  })
  
  if(!is.null(results) && nrow(results) > 0) {
    formatted_results <- results %>%
      mutate(
        `Category` = Subgroup,
        `Pooled Proportion` = format_percentage(Pooled_Proportion),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `Studies (k)` = k,
        `Sample Size (N)` = N,
        `I²` = format_percentage(I2, 1)
      ) %>%
      select(Characteristic, Approach, `Category`, `Pooled Proportion`, `95% CI`, 
             `Studies (k)`, `Sample Size (N)`, `I²`) %>%
      arrange(Characteristic, Approach, desc(`Pooled Proportion`))
    
    add_formatted_worksheet(
      wb, "Sample Characteristics", formatted_results,
      title = "Table 3: Careless Responding Rates by Sample Characteristics",
      notes = paste0(
        "Notes: This table presents careless responding rates across different sample characteristics. ",
        "Results are shown for all three analytical approaches to provide a comprehensive view of ",
        "how sample characteristics influence careless responding prevalence."
      )
    )
  }
}

process_journal_results <- function(wb) {
  # Load journal results from all approaches
  approaches <- c("primary" = "First-Method", 
                 "secondary" = "Single-Method", 
                 "overall" = "Overall")
  
  results <- map_dfr(names(approaches), ~{
    path <- glue("output/r_results/{.x}/subgroup_journal.csv")
    result <- read_result(path)
    if(!is.null(result) && nrow(result) > 0) {
      result$Approach <- approaches[.x]
    }
    result
  })
  
  if(!is.null(results) && nrow(results) > 0) {
    formatted_results <- results %>%
      mutate(
        `Journal` = Subgroup,
        `Pooled Proportion` = format_percentage(Pooled_Proportion),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `Studies (k)` = k,
        `Sample Size (N)` = N
      ) %>%
      select(Approach, `Journal`, `Pooled Proportion`, `95% CI`, 
             `Studies (k)`, `Sample Size (N)`) %>%
      arrange(Approach, desc(`Studies (k)`)) %>%
      filter(`Studies (k)` >= 3)  # Only include journals with sufficient studies
    
    if(nrow(formatted_results) > 0) {
      add_formatted_worksheet(
        wb, "Journal Analysis", formatted_results,
        title = "Table 4: Careless Responding Rates by Journal",
        notes = paste0(
          "Notes: This table presents careless responding rates across different journals. ",
          "Results are shown only for journals with at least 3 studies for meta-analysis."
        )
      )
    }
  }
}

process_temporal_results <- function(wb) {
  # Load temporal results from all approaches
  approaches <- c("primary" = "First-Method", 
                 "secondary" = "Single-Method", 
                 "overall" = "Overall")
  
  results <- map_dfr(names(approaches), ~{
    path <- glue("output/r_results/{.x}/temporal_trends.csv")
    result <- read_result(path)
    if(!is.null(result) && nrow(result) > 0) {
      result$Approach <- approaches[.x]
      result$Year <- as.numeric(as.character(result$Subgroup))
    }
    result
  })
  
  # Load trend analysis if available
  trend_analysis <- read_result("output/r_results/comparisons/temporal_trend_analysis.csv")
  
  if(!is.null(results) && nrow(results) > 0) {
    formatted_results <- results %>%
      mutate(
        `Pooled Proportion` = format_percentage(Pooled_Proportion),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `Studies (k)` = k,
        `Sample Size (N)` = N
      ) %>%
      select(Approach, Year, `Pooled Proportion`, `95% CI`, 
             `Studies (k)`, `Sample Size (N)`) %>%
      arrange(Approach, Year)
    
    # Format trend analysis if available
    formatted_trends <- NULL
    if(!is.null(trend_analysis) && nrow(trend_analysis) > 0) {
      formatted_trends <- trend_analysis %>%
        mutate(
          `Intercept` = round(Intercept, 4),
          `Slope` = round(Slope, 4),
          `SE of Slope` = round(SE_slope, 4),
          `Z-value` = round(Z_value, 2),
          `p-value` = format_pvalue(P_value),
          `Significant` = ifelse(Significant, "Yes", "No")
        ) %>%
        select(Approach, `Intercept`, `Slope`, `SE of Slope`, 
               `Z-value`, `p-value`, `Significant`, Direction)
    }
    
    # Add temporal results to workbook
    add_formatted_worksheet(
      wb, "Temporal Trends", formatted_results,
      title = "Table 5: Careless Responding Rates by Publication Year",
      notes = paste0(
        "Notes: This table presents careless responding rates by publication year. ",
        "The trend analysis examines whether there is a significant linear trend in rates over time."
      )
    )
    
    # Add trend analysis if available
    if(!is.null(formatted_trends) && nrow(formatted_trends) > 0) {
      add_formatted_worksheet(
        wb, "Temporal Trend Analysis", formatted_trends,
        title = "Table 5b: Analysis of Temporal Trends in Careless Responding",
        notes = paste0(
          "Notes: Slope represents the annual change in logit-transformed proportion. ",
          "Positive values indicate increasing rates over time, while negative values ",
          "indicate decreasing rates."
        )
      )
    }
  }
}

process_meta_regression <- function(wb) {
  # Load meta-regression results
  univariate_results <- read_result("output/r_results/meta_regression/01_univariate_results.csv")
  bivariate_results <- read_result("output/r_results/meta_regression/02_bivariate_results.csv")
  multivariate_results <- read_result("output/r_results/meta_regression/03_multivariate_results.csv")
  model_comparison <- read_result("output/r_results/meta_regression/05_model_comparison.csv")
  effect_summary <- read_result("output/r_results/meta_regression/06_effect_size_summary.csv")
  
  # Process univariate results
  if(!is.null(univariate_results) && nrow(univariate_results) > 0) {
    formatted_univariate <- univariate_results %>%
      mutate(
        `Moderator` = moderator,
        `QM` = round(QM, 2),
        `df` = QM_df,
        `p-value` = format_pvalue(QM_p),
        `R²` = format_percentage(R2, 1),
        `τ²` = round(tau2, 5),
        `I²` = format_percentage(I2, 1),
        `k` = k,
        `Significant` = ifelse(is_significant, "Yes", "No")
      ) %>%
      select(`Moderator`, `QM`, `df`, `p-value`, `R²`, `τ²`, `I²`, `k`, `Significant`) %>%
      arrange(as.numeric(`p-value`))
    
    add_formatted_worksheet(
      wb, "Meta-Regression Univariate", formatted_univariate,
      title = "Table 6: Univariate Meta-Regression Results",
      notes = paste0(
        "Notes: QM tests whether the moderator explains significant heterogeneity. ",
        "R² indicates the proportion of between-study variance explained by the moderator. ",
        "Results are from the First-Method approach."
      )
    )
  }
  
  # Process model comparison
  if(!is.null(model_comparison) && nrow(model_comparison) > 0) {
    formatted_comparison <- model_comparison %>%
      mutate(
        `Model Type` = model_type,
        `Model Name` = model_name,
        `Parameters` = n_parameters,
        `Studies (k)` = k,
        `QM` = round(QM, 2),
        `p-value` = format_pvalue(QM_p),
        `R²` = format_percentage(R2, 1),
        `AIC` = round(AIC, 2),
        `AIC Weight` = format_percentage(AIC_weight, 1)
      ) %>%
      select(`Model Type`, `Model Name`, `Parameters`, `Studies (k)`, 
             `QM`, `p-value`, `R²`, `AIC`, `AIC Weight`) %>%
      arrange(AIC)
    
    add_formatted_worksheet(
      wb, "Meta-Regression Models", formatted_comparison,
      title = "Table 7: Meta-Regression Model Comparison",
      notes = paste0(
        "Notes: Models are ordered by AIC (lower values indicate better fit). ",
        "AIC Weight indicates the relative probability that the model is the best among those compared. ",
        "R² represents the proportion of between-study variance explained."
      )
    )
  }
  
  # Process effect size summary
  if(!is.null(effect_summary) && nrow(effect_summary) > 0) {
    formatted_effects <- effect_summary %>%
      mutate(
        `Model Type` = model_type,
        `Variables` = variable,
        `R²` = R2_formatted,
        `Significance` = significance
      ) %>%
      select(`Model Type`, `Variables`, `R²`, `Significance`) %>%
      arrange(desc(`R²`))
    
    add_formatted_worksheet(
      wb, "Meta-Regression Effects", formatted_effects,
      title = "Table 8: Variance Explained by Meta-Regression Models",
      notes = paste0(
        "Notes: R² represents the proportion of between-study variance explained by each model. ",
        "Higher values indicate stronger moderating effects."
      )
    )
  }
}

process_multilevel_results <- function(wb) {
  # Load multilevel model results
  method_results <- read_result("output/r_results/multilevel/method_effectiveness_fixed_effects.csv")
  sample_results <- read_result("output/r_results/multilevel/sample_characteristics_fixed_effects.csv")
  variance_components <- read_result("output/r_results/multilevel/variance_components.csv")
  model_comparison <- read_result("output/r_results/multilevel/model_comparison.csv")
  
  # Process method effectiveness
  if(!is.null(method_results) && nrow(method_results) > 0) {
    formatted_methods <- method_results %>%
      mutate(
        `Method Type` = term,
        `Detection Rate` = format_percentage(detection_rate, 1),
        `95% CI` = paste0("[", format_percentage(ci_lower, 1), "-", 
                         format_percentage(ci_upper, 1), "]"),
        `Z-value` = round(zval, 2),
        `p-value` = format_pvalue(pval),
        `Significance` = significance
      ) %>%
      select(`Method Type`, `Detection Rate`, `95% CI`, 
             `Z-value`, `p-value`, `Significance`) %>%
      arrange(desc(`Detection Rate`))
    
    add_formatted_worksheet(
      wb, "Multilevel Method Effects", formatted_methods,
      title = "Table 9: Method Effectiveness from Multilevel Analysis",
      notes = paste0(
        "Notes: This table presents detection rates by method type from multilevel modeling. ",
        "Unlike subgroup meta-analysis, this approach directly accounts for dependencies between ",
        "methods from the same studies."
      )
    )
  }
  
  # Process variance components
  if(!is.null(variance_components) && nrow(variance_components) > 0) {
    formatted_variance <- variance_components %>%
      mutate(
        `Variance Source` = source,
        `Variance Component` = round(variance, 6),
        `Percentage` = paste0(round(percentage, 1), "%")
      ) %>%
      select(`Variance Source`, `Variance Component`, `Percentage`) %>%
      arrange(desc(`Percentage`))
    
    add_formatted_worksheet(
      wb, "Variance Components", formatted_variance,
      title = "Table 10: Decomposition of Variance in Careless Responding Rates",
      notes = paste0(
        "Notes: This table shows the proportion of total variance attributable to different sources. ",
        "Higher percentages indicate greater influence on variation in careless responding rates."
      )
    )
  }
}

process_position_effects <- function(wb) {
  # Load position effects results
  position_comparison <- read_result("output/r_results/positional/position_comparison.csv")
  position_changes <- read_result("output/r_results/positional/position_changes.csv")
  method_position <- read_result("output/r_results/positional/method_position_predictions.csv")
  
  # Process position comparison
  if(!is.null(position_comparison) && nrow(position_comparison) > 0) {
    formatted_positions <- position_comparison %>%
      mutate(
        `Position` = position,
        `Proportion Type` = proportion_type,
        `Detection Rate` = format_percentage(detection_rate, 1),
        `95% CI` = paste0("[", format_percentage(ci_lower, 1), "-", 
                         format_percentage(ci_upper, 1), "]")
      ) %>%
      select(`Position`, `Proportion Type`, `Detection Rate`, `95% CI`) %>%
      arrange(`Proportion Type`, `Position`)
    
    add_formatted_worksheet(
      wb, "Position Effects", formatted_positions,
      title = "Table 11: Detection Rates by Position in Screening Sequence",
      notes = paste0(
        "Notes: Raw proportions use the original sample size as denominator, while ",
        "adjusted proportions use the remaining sample size after previous screening steps. ",
        "Comparing across positions reveals the impact of sequential screening on detection rates."
      )
    )
  }
  
  # Process position changes
  if(!is.null(position_changes) && nrow(position_changes) > 0) {
    formatted_changes <- position_changes %>%
      mutate(
        `Position` = position,
        `Proportion Type` = proportion_type,
        `Previous Rate` = format_percentage(previous_rate, 1),
        `Current Rate` = format_percentage(detection_rate, 1),
        `Absolute Change` = format_percentage(absolute_change, 1),
        `Percentage Change` = paste0(round(percentage_change, 1), "%")
      ) %>%
      select(`Position`, `Proportion Type`, `Previous Rate`, 
             `Current Rate`, `Absolute Change`, `Percentage Change`) %>%
      arrange(`Proportion Type`, `Position`)
    
    add_formatted_worksheet(
      wb, "Position-to-Position Changes", formatted_changes,
      title = "Table 12: Position-to-Position Changes in Detection Rates",
      notes = paste0(
        "Notes: This table quantifies changes in detection rates from one position to the next. ",
        "Percentage change is calculated relative to the previous position's rate."
      )
    )
  }
  
  # Process method × position interaction
  if(!is.null(method_position) && nrow(method_position) > 0) {
    # Filter to a manageable subset focusing on key method groups
    key_methods <- c("attention_checks", "response_patterns", "statistical_methods", 
                    "self_reported", "consistency_indices")
    
    formatted_interaction <- method_position %>%
      filter(method_group %in% key_methods) %>%
      mutate(
        `Method Type` = method_group,
        `Position` = method_position,
        `Proportion Type` = proportion_type,
        `Detection Rate` = format_percentage(pred_rate, 1),
        `95% CI` = paste0("[", format_percentage(ci_lower, 1), "-", 
                         format_percentage(ci_upper, 1), "]")
      ) %>%
      select(`Method Type`, `Position`, `Proportion Type`, 
             `Detection Rate`, `95% CI`) %>%
      arrange(`Proportion Type`, `Method Type`, `Position`)
    
    if(nrow(formatted_interaction) > 0) {
      add_formatted_worksheet(
        wb, "Method-Position Interaction", formatted_interaction,
        title = "Table 13: Method × Position Interaction Effects",
        notes = paste0(
          "Notes: This table shows how different method types perform across screening positions. ",
          "It reveals which methods are most sensitive to position effects in sequential screening."
        )
      )
    }
  }
}

process_publication_bias <- function(wb) {
  # Load publication bias results
  pub_bias_summary <- read_result("output/r_results/pub_bias/summary_table.csv")
  
  # Process publication bias results
  if(!is.null(pub_bias_summary) && nrow(pub_bias_summary) > 0) {
    formatted_bias <- pub_bias_summary %>%
      mutate(
        `Original Estimate with CI` = Original_Est_CI,
        `Egger's Test Result` = Egger_Result
      ) %>%
      select(Approach, `Original Estimate with CI`, `Egger's Test Result`)
    
    add_formatted_worksheet(
      wb, "Publication Bias", formatted_bias,
      title = "Table 14: Publication Bias Assessment",
      notes = paste0(
        "Notes: Egger's test examines funnel plot asymmetry, where significant p-values ",
        "suggest potential publication bias or small-study effects. Non-significant results ",
        "indicate no strong evidence of publication bias."
      )
    )
  }
}

process_sensitivity_analyses <- function(wb) {
  # Load sensitivity analysis results
  estimator_sensitivity <- read_result("output/r_results/sensitivity/estimator_sensitivity_First-Method.csv")
  sample_threshold <- read_result("output/r_results/sensitivity/sample_size_threshold_First-Method.csv")
  
  # Process estimator sensitivity
  if(!is.null(estimator_sensitivity) && nrow(estimator_sensitivity) > 0) {
    formatted_estimators <- estimator_sensitivity %>%
      mutate(
        `Estimator` = Estimator,
        `Pooled Proportion` = format_percentage(Estimate),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `τ²` = round(tau2, 5),
        `I²` = format_percentage(I2, 1)
      ) %>%
      select(`Estimator`, `Pooled Proportion`, `95% CI`, `τ²`, `I²`) %>%
      arrange(`Estimator`)
    
    add_formatted_worksheet(
      wb, "Estimator Sensitivity", formatted_estimators,
      title = "Table 15: Sensitivity to Between-Study Variance Estimator",
      notes = paste0(
        "Notes: This table shows how the choice of between-study variance estimator affects results. ",
        "The First-Method approach is used for this analysis. DL = DerSimonian-Laird, ",
        "REML = Restricted Maximum Likelihood, SJ = Sidik-Jonkman, HS = Hunter-Schmidt."
      )
    )
  }
  
  # Process sample size threshold
  if(!is.null(sample_threshold) && nrow(sample_threshold) > 0) {
    formatted_thresholds <- sample_threshold %>%
      mutate(
        `Min Sample Size` = Threshold,
        `Studies Included` = Studies_Included,
        `% of Studies` = paste0(round(Proportion_of_Studies * 100, 1), "%"),
        `Participants` = Participants_Included,
        `% of Participants` = paste0(round(Proportion_of_Participants * 100, 1), "%"),
        `Pooled Proportion` = format_percentage(Estimate),
        `95% CI` = format_ci(CI_Lower, CI_Upper),
        `I²` = format_percentage(I2, 1)
      ) %>%
      select(`Min Sample Size`, `Studies Included`, `% of Studies`, 
             `Participants`, `% of Participants`, 
             `Pooled Proportion`, `95% CI`, `I²`) %>%
      arrange(`Min Sample Size`)
    
    add_formatted_worksheet(
      wb, "Sample Size Thresholds", formatted_thresholds,
      title = "Table 16: Sample Size Threshold Analysis",
      notes = paste0(
        "Notes: This table examines how minimum sample size thresholds affect the pooled estimate. ",
        "It helps assess whether smaller studies systematically differ from larger ones."
      )
    )
  }
}

# Main Execution ---------------------------------------------------------

main <- function() {
  cat("\nCompiling comprehensive results tables...\n")
  
  # Create workbook
  wb <- createWorkbook()
  
  # Process each section
  process_overall_results(wb)
  cat("✓ Compiled overall results\n")
  
  process_method_results(wb)
  cat("✓ Compiled method analysis results\n")
  
  process_sample_results(wb)
  cat("✓ Compiled sample characteristics results\n")
  
  process_journal_results(wb)
  cat("✓ Compiled journal analysis results\n")
  
  process_temporal_results(wb)
  cat("✓ Compiled temporal trend results\n")
  
  process_meta_regression(wb)
  cat("✓ Compiled meta-regression results\n")
  
  process_multilevel_results(wb)
  cat("✓ Compiled multilevel model results\n")
  
  process_position_effects(wb)
  cat("✓ Compiled position effects results\n")
  
  process_publication_bias(wb)
  cat("✓ Compiled publication bias results\n")
  
  process_sensitivity_analyses(wb)
  cat("✓ Compiled sensitivity analyses\n")
  
  # Save workbook
  output_file <- "output/tables/meta_analysis_results.xlsx"
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("\nComprehensive results tables compiled successfully!\n")
  cat("Excel workbook saved to:", output_file, "\n")
}

# Run the main function
main()