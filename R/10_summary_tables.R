###############################################################################
# summary_tables.R
# 
# This script compiles the core meta-analysis results from scripts 02-05 into
# a comprehensive Excel workbook. It includes:
# 
# - Overall results for all three analytical approaches (First-Method, 
#   Single-Method, and Overall)
# - Subgroup analyses by method type, sample characteristics, and study features
# - Heterogeneity measures
# - Approach comparisons and statistical tests
# - Temporal trends
# 
# The results are organized into separate worksheets with consistent formatting
# to facilitate interpretation and inclusion in the dissertation.
###############################################################################

library(tidyverse)
library(openxlsx)
library(glue)

# Create output directory if it doesn't exist
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Function to read CSV results with robust error handling
read_result <- function(file_path, default = NULL) {
  if(file.exists(file_path)) {
    tryCatch({
      read.csv(file_path, check.names = FALSE)
    }, error = function(e) {
      cat("Error reading", file_path, ":", e$message, "\n")
      return(default)
    })
  } else {
    cat("File not found:", file_path, "\n")
    return(default)
  }
}

# Load metafor for transformations if needed
if(!"metafor" %in% loadedNamespaces()) {
  library(metafor)
}

# Create a new workbook
wb <- createWorkbook()

#################################################
# OVERALL RESULTS
#################################################

cat("\nCompiling overall results...\n")

addWorksheet(wb, "Overall Results")

# Get the overall results for each approach
first_method <- read_result("output/r_results/primary/overall_results.csv")
single_method <- read_result("output/r_results/secondary/overall_results.csv") 
overall_method <- read_result("output/r_results/overall/overall_results.csv")

# Get comparison results if available
comparison_results <- read_result("output/r_results/comparisons/approach_comparison.csv")
pairwise_tests <- read_result("output/r_results/comparisons/pairwise_statistical_tests.csv")

# Combine overall results
if(!is.null(first_method) && !is.null(single_method) && !is.null(overall_method)) {
  combined_overall <- bind_rows(first_method, single_method, overall_method)
  
  # Format for readability
  formatted_overall <- combined_overall %>%
    mutate(
      Approach = factor(Analysis, levels = c("First-Method", "Single-Method", "Overall")),
      Estimate = paste0(round(Pooled_Proportion * 100, 2), "%"),
      CI = paste0("[", round(CI_Lower * 100, 2), "% - ", round(CI_Upper * 100, 2), "%]"),
      I2_value = paste0(round(I2, 1), "%"),
      Tau2_value = round(tau2, 5),
      Q_value = round(Q, 2),
      P_Q = ifelse(p_Q < 0.001, "< 0.001", round(p_Q, 3))
    ) %>%
    select(Approach, Estimate, CI, k, N, I2_value, Tau2_value, Q_value, P_Q) %>%
    rename(
      "Approach" = Approach,
      "Pooled Proportion" = Estimate,
      "95% CI" = CI,
      "Studies (k)" = k,
      "Sample Size (N)" = N,
      "I²" = I2_value,
      "τ²" = Tau2_value,
      "Q Statistic" = Q_value,
      "p-value" = P_Q
    ) %>%
    arrange(Approach)
  
  # Write the table title
  writeData(wb, "Overall Results", "Table 1: Overall Results by Analytical Approach", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Overall Results", formatted_overall, startRow = 3, startCol = 1)
  
  # Add statistical test results if available
  if(!is.null(pairwise_tests) && nrow(pairwise_tests) > 0) {
    # Format pairwise test results
    formatted_tests <- pairwise_tests %>%
      mutate(
        Absolute_Diff = paste0(round(Absolute_Diff * 100, 2), "%"),
        Percentage_Diff = paste0(round(Percentage_Diff, 1), "%"),
        CI_Overlap = ifelse(CI_Overlap, "Yes", "No"),
        Z_statistic = round(Z_statistic, 2),
        P_value = ifelse(P_value < 0.001, "< 0.001", round(P_value, 3)),
        Significant = ifelse(Significant, "Yes", "No")
      ) %>%
      rename(
        "Comparison" = Comparison,
        "Absolute Difference" = Absolute_Diff,
        "Relative Difference" = Percentage_Diff,
        "CI Overlap" = CI_Overlap,
        "Z-statistic" = Z_statistic,
        "p-value" = P_value,
        "Significant" = Significant
      )
    
    # Write the pairwise tests title
    writeData(wb, "Overall Results", "Pairwise Statistical Tests between Approaches", 
              startRow = nrow(formatted_overall) + 5, startCol = 1)
    
    # Write the pairwise tests
    writeData(wb, "Overall Results", formatted_tests, 
              startRow = nrow(formatted_overall) + 7, startCol = 1)
  }
  
  # Add explanation under the table
  writeData(wb, "Overall Results", paste0(
    "Notes: Pooled proportions represent the estimated prevalence of careless responding. ",
    "I² indicates the percentage of variation due to true heterogeneity rather than sampling error. ",
    "The Q statistic tests the null hypothesis that all studies share a common effect size."
  ), startRow = nrow(formatted_overall) + if(!is.null(pairwise_tests) && nrow(pairwise_tests) > 0) nrow(pairwise_tests) + 10 else 10, startCol = 1)
}

#################################################
# METHOD TYPE ANALYSIS
#################################################

cat("Compiling method type results...\n")

addWorksheet(wb, "Methods Analysis")

# Get method type results for First-Method and Single-Method
first_method_types <- read_result("output/r_results/primary/subgroup_method_type.csv")
single_method_types <- read_result("output/r_results/secondary/subgroup_method_type.csv")

# Combine method type results
combined_methods <- list()

if(!is.null(first_method_types) && nrow(first_method_types) > 0) {
  first_method_types$Approach <- "First-Method"
  combined_methods[[length(combined_methods) + 1]] <- first_method_types
}

if(!is.null(single_method_types) && nrow(single_method_types) > 0) {
  single_method_types$Approach <- "Single-Method"
  combined_methods[[length(combined_methods) + 1]] <- single_method_types
}

if(length(combined_methods) > 0) {
  combined_df <- bind_rows(combined_methods)
  
  # Format for readability
  formatted_methods <- combined_df %>%
    mutate(
      Estimate = paste0(round(Pooled_Proportion * 100, 2), "%"),
      CI = paste0("[", round(CI_Lower * 100, 2), "% - ", round(CI_Upper * 100, 2), "%]"),
      I2_value = paste0(round(I2, 1), "%"),
      Tau2_value = round(tau2, 5),
      Q_value = round(Q, 2),
      P_Q = ifelse(p_Q < 0.001, "< 0.001", round(p_Q, 3))
    ) %>%
    select(Approach, Subgroup, Estimate, CI, k, N, I2_value, Tau2_value) %>%
    rename(
      "Approach" = Approach,
      "Method Type" = Subgroup,
      "Pooled Proportion" = Estimate,
      "95% CI" = CI,
      "Studies (k)" = k,
      "Sample Size (N)" = N,
      "I²" = I2_value,
      "τ²" = Tau2_value
    ) %>%
    arrange(Approach, desc(`Pooled Proportion`))
  
  # Write the table title
  writeData(wb, "Methods Analysis", "Table 2: Careless Responding Rates by Detection Method Type", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Methods Analysis", formatted_methods, startRow = 3, startCol = 1)
  
  # Add explanation
  writeData(wb, "Methods Analysis", paste0(
    "Notes: This table presents careless responding rates by detection method type. ",
    "Only First-Method and Single-Method approaches are included because method-level subgroup analysis ",
    "is not conceptually appropriate for the Overall approach, which aggregates across methods."
  ), startRow = nrow(formatted_methods) + 5, startCol = 1)
}

#################################################
# SAMPLE CHARACTERISTICS
#################################################

cat("Compiling sample characteristic results...\n")

addWorksheet(wb, "Sample Characteristics")

# Define the sample characteristic variables to include
sample_vars <- c("sample_source_name", "sample_platform_name", "sample_recruitment_name")
var_names <- c("Sample Source", "Sample Platform", "Sample Recruitment")

# Create a list to store all sample results
sample_results <- list()

# Loop through each approach and sample variable
for(approach in c("primary", "secondary", "overall")) {
  approach_name <- switch(approach,
                        "primary" = "First-Method",
                        "secondary" = "Single-Method",
                        "overall" = "Overall")
  
  for(i in seq_along(sample_vars)) {
    var <- sample_vars[i]
    var_name <- var_names[i]
    
    # Get the results file
    file_path <- glue("output/r_results/{approach}/subgroup_{var}.csv")
    results <- read_result(file_path)
    
    if(!is.null(results) && nrow(results) > 0) {
      results$Approach <- approach_name
      results$Variable <- var_name
      sample_results[[length(sample_results) + 1]] <- results
    }
  }
}

# Combine all sample results if we have any
if(length(sample_results) > 0) {
  combined_samples <- bind_rows(sample_results)
  
  # Format for readability
  formatted_samples <- combined_samples %>%
    mutate(
      Estimate = paste0(round(Pooled_Proportion * 100, 2), "%"),
      CI = paste0("[", round(CI_Lower * 100, 2), "% - ", round(CI_Upper * 100, 2), "%]"),
      I2_value = paste0(round(I2, 1), "%")
    ) %>%
    select(Approach, Variable, Subgroup, Estimate, CI, k, N, I2_value) %>%
    rename(
      "Approach" = Approach,
      "Characteristic" = Variable,
      "Category" = Subgroup,
      "Pooled Proportion" = Estimate,
      "95% CI" = CI,
      "Studies (k)" = k,
      "Sample Size (N)" = N,
      "I²" = I2_value
    ) %>%
    arrange(Characteristic, Approach, desc(`Pooled Proportion`))
  
  # Write the table title
  writeData(wb, "Sample Characteristics", "Table 3: Careless Responding Rates by Sample Characteristics", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Sample Characteristics", formatted_samples, startRow = 3, startCol = 1)
  
  # Add explanation
  writeData(wb, "Sample Characteristics", paste0(
    "Notes: This table presents careless responding rates across different sample characteristics. ",
    "Results are shown for all three analytical approaches to provide a comprehensive view of ",
    "how sample characteristics influence careless responding prevalence."
  ), startRow = nrow(formatted_samples) + 5, startCol = 1)
}

#################################################
# JOURNAL ANALYSIS
#################################################

cat("Compiling journal results...\n")

addWorksheet(wb, "Journal Analysis")

# Get journal results for each approach
journal_results <- list()

for(approach in c("primary", "secondary", "overall")) {
  approach_name <- switch(approach,
                        "primary" = "First-Method",
                        "secondary" = "Single-Method",
                        "overall" = "Overall")
  
  file_path <- glue("output/r_results/{approach}/subgroup_journal_name.csv")
  results <- read_result(file_path)
  
  if(!is.null(results) && nrow(results) > 0) {
    results$Approach <- approach_name
    journal_results[[length(journal_results) + 1]] <- results
  }
}

# Combine all journal results if we have any
if(length(journal_results) > 0) {
  combined_journals <- bind_rows(journal_results)
  
  # Format for readability
  formatted_journals <- combined_journals %>%
    mutate(
      Estimate = paste0(round(Pooled_Proportion * 100, 2), "%"),
      CI = paste0("[", round(CI_Lower * 100, 2), "% - ", round(CI_Upper * 100, 2), "%]"),
      Journal = as.character(Subgroup)
    ) %>%
    select(Approach, Journal, Estimate, CI, k, N) %>%
    rename(
      "Approach" = Approach,
      "Journal" = Journal,
      "Pooled Proportion" = Estimate,
      "95% CI" = CI,
      "Studies (k)" = k,
      "Sample Size (N)" = N
    ) %>%
    arrange(Approach, desc(`Studies (k)`))
  
  # Write the table title
  writeData(wb, "Journal Analysis", "Table 4: Careless Responding Rates by Journal", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Journal Analysis", formatted_journals, startRow = 3, startCol = 1)
  
  # Add explanation
  writeData(wb, "Journal Analysis", paste0(
    "Notes: This table presents careless responding rates across different journals. ",
    "Results are shown only for journals with sufficient studies for meta-analysis."
  ), startRow = nrow(formatted_journals) + 5, startCol = 1)
}

#################################################
# TEMPORAL TRENDS
#################################################

cat("Compiling temporal trends results...\n")

addWorksheet(wb, "Temporal Trends")

# Get temporal trends for each approach
temporal_results <- list()

for(approach in c("primary", "secondary", "overall")) {
  approach_name <- switch(approach,
                        "primary" = "First-Method",
                        "secondary" = "Single-Method",
                        "overall" = "Overall")
  
  file_path <- glue("output/r_results/{approach}/temporal_trends.csv")
  results <- read_result(file_path)
  
  if(!is.null(results) && nrow(results) > 0) {
    results$Approach <- approach_name
    temporal_results[[length(temporal_results) + 1]] <- results
  }
}

# Get temporal trend analysis results if available
trend_analysis <- read_result("output/r_results/comparisons/temporal_trend_analysis.csv")

# Combine all temporal results if we have any
if(length(temporal_results) > 0) {
  combined_temporal <- bind_rows(temporal_results)
  
  # Format for readability
  formatted_temporal <- combined_temporal %>%
    mutate(
      Year = as.numeric(as.character(Subgroup)),
      Estimate = paste0(round(Pooled_Proportion * 100, 2), "%"),
      CI = paste0("[", round(CI_Lower * 100, 2), "% - ", round(CI_Upper * 100, 2), "%]")
    ) %>%
    select(Approach, Year, Estimate, CI, k, N) %>%
    rename(
      "Approach" = Approach,
      "Year" = Year,
      "Pooled Proportion" = Estimate,
      "95% CI" = CI,
      "Studies (k)" = k,
      "Sample Size (N)" = N
    ) %>%
    arrange(Approach, Year)
  
  # Write the table title
  writeData(wb, "Temporal Trends", "Table 5: Careless Responding Rates by Publication Year", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Temporal Trends", formatted_temporal, startRow = 3, startCol = 1)
  
  # Add trend analysis if available
  if(!is.null(trend_analysis) && nrow(trend_analysis) > 0) {
    formatted_trends <- trend_analysis %>%
      mutate(
        Intercept = round(Intercept, 4),
        Slope = round(Slope, 4),
        SE_slope = round(SE_slope, 4),
        Z_value = round(Z_value, 2),
        P_value = ifelse(P_value < 0.001, "< 0.001", round(P_value, 3)),
        Significant = ifelse(Significant, "Yes", "No")
      ) %>%
      rename(
        "Approach" = Approach,
        "Intercept" = Intercept,
        "Slope" = Slope,
        "SE of Slope" = SE_slope,
        "Z-value" = Z_value,
        "p-value" = P_value,
        "Significant" = Significant,
        "Direction" = Direction
      )
    
    # Write the trend analysis title
    writeData(wb, "Temporal Trends", "Temporal Trend Analysis", 
              startRow = nrow(formatted_temporal) + 5, startCol = 1)
    
    # Write the trend analysis
    writeData(wb, "Temporal Trends", formatted_trends, 
              startRow = nrow(formatted_temporal) + 7, startCol = 1)
  }
  
  # Add explanation
  writeData(wb, "Temporal Trends", paste0(
    "Notes: This table presents careless responding rates by publication year. ",
    "The trend analysis examines whether there is a significant linear trend in careless responding rates over time."
  ), startRow = nrow(formatted_temporal) + (if(!is.null(trend_analysis) && nrow(trend_analysis) > 0) nrow(trend_analysis) + 10 else 10), startCol = 1)
}

#################################################
# HETEROGENEITY TESTS
#################################################

cat("Compiling heterogeneity test results...\n")

addWorksheet(wb, "Heterogeneity Tests")

# Get heterogeneity test results
heterogeneity_tests <- read_result("output/r_results/comparisons/heterogeneity_tests.csv")

if(!is.null(heterogeneity_tests) && nrow(heterogeneity_tests) > 0) {
  # Format for readability
  formatted_heterogeneity <- heterogeneity_tests %>%
    mutate(
      Q_statistic = round(Q_statistic, 2),
      P_value = ifelse(P_value < 0.001, "< 0.001", round(P_value, 3)),
      Significant = ifelse(Significant, "Yes", "No"),
      I2 = paste0(round(I2, 1), "%"),
      H2 = round(H2, 2),
      tau2 = round(tau2, 5)
    ) %>%
    select(Variable, Subgroup, Q_statistic, df, P_value, Significant, I2, tau2) %>%
    rename(
      "Variable" = Variable,
      "Subgroup" = Subgroup,
      "Q Statistic" = Q_statistic,
      "df" = df,
      "p-value" = P_value,
      "Significant" = Significant,
      "I²" = I2,
      "τ²" = tau2
    )
  
  # Write the table title
  writeData(wb, "Heterogeneity Tests", "Table 6: Tests of Heterogeneity Between Approaches", 
            startRow = 1, startCol = 1)
  
  # Write the main results table
  writeData(wb, "Heterogeneity Tests", formatted_heterogeneity, startRow = 3, startCol = 1)
  
  # Add explanation
  writeData(wb, "Heterogeneity Tests", paste0(
    "Notes: This table presents tests of heterogeneity between the three analytical approaches. ",
    "Significant results indicate that careless responding rates differ systematically across approaches ",
    "for the given variable or subgroup."
  ), startRow = nrow(formatted_heterogeneity) + 5, startCol = 1)
}

#################################################
# APPLY FORMATTING
#################################################

# Apply consistent formatting to all worksheets
for(sheet in names(wb)) {
  # Auto-adjust column widths
  setColWidths(wb, sheet, cols = 1:50, widths = "auto")
  
  # Add styles
  addStyle(wb, sheet, createStyle(textDecoration = "bold", fontSize = 14), 
           rows = 1, cols = 1)
  
  addStyle(wb, sheet, createStyle(fgFill = "#D9E1F2", border = "Bottom", 
                                 borderColour = "#4472C4", borderStyle = "medium",
                                 textDecoration = "bold"),
           rows = 3, cols = 1:50, gridExpand = TRUE)
}

#################################################
# SAVE WORKBOOK
#################################################

# Save the workbook
output_file <- "output/tables/meta_analysis_results.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\nResults compiled successfully!\n")
cat("Excel workbook saved to:", output_file, "\n")