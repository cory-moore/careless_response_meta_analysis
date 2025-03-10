###############################################################################
# R script for meta-analysis of order effects in careless responding detection
#
# This script performs comprehensive meta-analyses on careless responding 
# detection methods across different screening strategies, examining how
# detection rates vary by position in screening sequences. It implements both
# standard and adjusted analyses to properly account for order effects.
#
# OVERVIEW AND RATIONALE:
#
# Careless responding detection methods can be applied in three distinct ways:
#
# 1. Single Method: Using just one detection method in isolation
#   - Provides a clean baseline of detection efficacy
#   - No influence from or interaction with other methods
#
# 2. Sequential Screening: Applying multiple methods in sequence
#   - Each method is only applied to participants who passed previous methods
#   - Position in the sequence affects the sample composition each method faces
#   - Later methods work with a "cleaner" sample (obvious cases already removed)
#   - Two different proportions are calculated:
#     a) Original: Uses full sample size as denominator (cr_amount/sample_size)
#     b) Adjusted: Uses remaining sample as denominator (cr_amount/remaining_sample)
#     The adjusted proportion better reflects a method's effectiveness on the
#     specific sample it was applied to
#
# 3. Non-Sequential Multiple Methods: Using multiple methods without sequence
#   - Studies report which methods were used but not method-specific detection
#   - Only the overall careless responding rate can be validly analyzed
#   - Individual method analyses would be conceptually inappropriate
#
# This script conducts three levels of analyses:
#
# 1. Overall Analysis: 
#   - Meta-analyzes overall careless responding rates for each strategy
#   - Provides comparisons of cumulative detection capability
#
# 2. Method-Specific Analysis: 
#   - Analyzes individual detection methods by position and strategy
#   - Only performed for single method and sequential strategies
#   - Demonstrates how specific methods perform at different positions
#
# 3. Method Type Analysis: 
#   - Groups similar methods (e.g., attention checks, response patterns)
#   - Provides broader patterns with increased statistical power
#   - Shows whether certain categories of methods are more position-sensitive
#
# All analyses use random-effects meta-analysis with logit transformation to
# properly handle proportion data. For sequential screening, both original and
# adjusted proportions are analyzed to illustrate the impact of changing
# denominators through the screening process.
#
# The primary findings reveal how detection rates vary by:
# - Screening strategy (single vs. sequential vs. non-sequential)
# - Method type (attention checks, response time, etc.)
# - Position in screening sequence (first, second, etc.)
# - Original vs. adjusted calculation approaches
#
# These results provide evidence-based guidance for designing optimal careless
# responding detection protocols in survey research.
###############################################################################

library(dplyr)
library(metafor)
library(meta)
library(readr)
library(jsonlite)

load_codebook <- function(filename) {
  return(fromJSON(filename))
}

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
    
    return(list(
      pooled_p = round(pooled_p, 4),
      pooled_se = round(rma_result$se, 4),
      lower_ci = round(ci[1], 4),
      upper_ci = round(ci[2], 4),
      n_sum = sum(meta_data$n),
      i_squared = round(rma_result$I2, 2),
      q = round(rma_result$QE, 2),
      n_studies = nrow(meta_data)
    ))
  }, error = function(e) {
    cat("Error in primary meta-analysis, trying alternate method:", e$message, "\n")
    
    tryCatch({
      meta_result <- metaprop(event = meta_data$p * meta_data$n, 
                             n = meta_data$n, 
                             method = "GLMM", 
                             sm = "PLOGIT",
                             random = TRUE)
      
      return(list(
        pooled_p = round(meta_result$TE.random, 4),
        pooled_se = round(meta_result$seTE.random, 4),
        lower_ci = round(meta_result$lower.random, 4),
        upper_ci = round(meta_result$upper.random, 4),
        n_sum = sum(meta_data$n),
        i_squared = round(meta_result$I2, 2),
        q = round(meta_result$Q, 2),
        n_studies = nrow(meta_data)
      ))
    }, error = function(e2) {
      cat("Both meta-analysis methods failed, using weighted average:", e2$message, "\n")
      
      weighted_sum <- sum(meta_data$p * meta_data$n)
      total_n <- sum(meta_data$n)
      weighted_avg <- weighted_sum / total_n
      
      return(list(
        pooled_p = round(weighted_avg, 4),
        pooled_se = NA,
        lower_ci = NA,
        upper_ci = NA,
        n_sum = total_n,
        i_squared = NA,
        q = NA,
        n_studies = nrow(meta_data)
      ))
    })
  })
}

find_datasets <- function(pattern) {
  files <- list.files("output/order_effects/data", pattern = pattern, full.names = TRUE)
  return(files)
}

load_method_dataset <- function(file_path) {
  tryCatch({
    dataset <- read_csv(file_path, show_col_types = FALSE)
    required_cols <- c("method_code", "method_name", "strategy", "method_position", 
                       "sample_size", "proportion", "adjusted_proportion")
    missing_cols <- required_cols[!required_cols %in% colnames(dataset)]
    
    if (length(missing_cols) > 0) {
      cat("Warning: Dataset", file_path, "is missing columns:", 
          paste(missing_cols, collapse = ", "), "\n")
      return(NULL)
    }
    
    return(dataset)
  }, error = function(e) {
    cat("Error loading dataset", file_path, ":", e$message, "\n")
    return(NULL)
  })
}

analyze_overall_cr <- function() {
  results <- data.frame(
    Analysis_Level = character(),
    Strategy = character(),
    Pooled_Prevalence = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    Sample_Size = integer(),
    N_Studies = integer(),
    I_Squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  single_data <- read_csv("output/order_effects/data/strategy_single.csv", show_col_types = FALSE)
  sequential_data <- read_csv("output/order_effects/data/strategy_sequential.csv", show_col_types = FALSE)
  non_sequential_data <- read_csv("output/order_effects/data/strategy_non_sequential.csv", show_col_types = FALSE)
  
  if (nrow(single_data) > 0) {
    if (!"proportions_total" %in% colnames(single_data)) {
      single_data$proportions_total <- single_data$cr_total_amount / single_data$sample_size
    }
    
    single_meta <- meta_analysis(single_data$proportions_total, single_data$sample_size)
    
    results <- rbind(results, data.frame(
      Analysis_Level = "Overall",
      Strategy = "Single Method",
      Pooled_Prevalence = single_meta$pooled_p,
      Lower_CI = single_meta$lower_ci,
      Upper_CI = single_meta$upper_ci,
      Sample_Size = single_meta$n_sum,
      N_Studies = single_meta$n_studies,
      I_Squared = single_meta$i_squared,
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(sequential_data) > 0) {
    if (!"proportions_total" %in% colnames(sequential_data)) {
      sequential_data$proportions_total <- sequential_data$cr_total_amount / sequential_data$sample_size
    }
    
    sequential_meta <- meta_analysis(sequential_data$proportions_total, sequential_data$sample_size)
    
    results <- rbind(results, data.frame(
      Analysis_Level = "Overall",
      Strategy = "Sequential",
      Pooled_Prevalence = sequential_meta$pooled_p,
      Lower_CI = sequential_meta$lower_ci,
      Upper_CI = sequential_meta$upper_ci,
      Sample_Size = sequential_meta$n_sum,
      N_Studies = sequential_meta$n_studies,
      I_Squared = sequential_meta$i_squared,
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(non_sequential_data) > 0) {
    if (!"proportions_total" %in% colnames(non_sequential_data)) {
      non_sequential_data$proportions_total <- non_sequential_data$cr_total_amount / non_sequential_data$sample_size
    }
    
    non_sequential_meta <- meta_analysis(non_sequential_data$proportions_total, non_sequential_data$sample_size)
    
    results <- rbind(results, data.frame(
      Analysis_Level = "Overall",
      Strategy = "Non-Sequential",
      Pooled_Prevalence = non_sequential_meta$pooled_p,
      Lower_CI = non_sequential_meta$lower_ci,
      Upper_CI = non_sequential_meta$upper_ci,
      Sample_Size = non_sequential_meta$n_sum,
      N_Studies = non_sequential_meta$n_studies,
      I_Squared = non_sequential_meta$i_squared,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

analyze_method_dataset <- function(dataset, method_code, method_name) {
  results <- data.frame(
    Method_Code = integer(),
    Method_Name = character(),
    Strategy = character(),
    Position = integer(),
    Proportion_Type = character(),
    Pooled_Prevalence = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    Sample_Size = integer(),
    N_Studies = integer(),
    I_Squared = numeric(),
    Method_Type = character(),
    stringsAsFactors = FALSE
  )
  
  method_type <- if (length(unique(dataset$method_type)) > 0) {
    unique(dataset$method_type)[1]
  } else {
    "unknown"
  }
  
  for (strategy in unique(dataset$strategy)) {
    if (strategy == "Non-Sequential") {
      next
    }
    
    strategy_data <- dataset %>% filter(strategy == !!strategy)
    
    for (position in unique(strategy_data$method_position)) {
      pos_data <- strategy_data %>% filter(method_position == position)
      
      if (nrow(pos_data) < 2) {
        next
      }
      
      orig_meta <- meta_analysis(pos_data$proportion, pos_data$sample_size)
      
      results <- rbind(results, data.frame(
        Method_Code = method_code,
        Method_Name = method_name,
        Strategy = strategy,
        Position = position,
        Proportion_Type = "Original",
        Pooled_Prevalence = orig_meta$pooled_p,
        Lower_CI = orig_meta$lower_ci,
        Upper_CI = orig_meta$upper_ci,
        Sample_Size = orig_meta$n_sum,
        N_Studies = orig_meta$n_studies,
        I_Squared = orig_meta$i_squared,
        Method_Type = method_type,
        stringsAsFactors = FALSE
      ))
      
      if (strategy == "Sequential") {
        adj_meta <- meta_analysis(pos_data$adjusted_proportion, pos_data$remaining_sample)
        
        results <- rbind(results, data.frame(
          Method_Code = method_code,
          Method_Name = method_name,
          Strategy = strategy,
          Position = position,
          Proportion_Type = "Adjusted",
          Pooled_Prevalence = adj_meta$pooled_p,
          Lower_CI = adj_meta$lower_ci,
          Upper_CI = adj_meta$upper_ci,
          Sample_Size = sum(pos_data$remaining_sample),
          N_Studies = adj_meta$n_studies,
          I_Squared = adj_meta$i_squared,
          Method_Type = method_type,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

analyze_method_type_dataset <- function(dataset, method_type) {
  results <- data.frame(
    Method_Type = character(),
    Strategy = character(),
    Position = integer(),
    Proportion_Type = character(),
    Pooled_Prevalence = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    Sample_Size = integer(),
    N_Studies = integer(),
    I_Squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  type_data <- dataset %>% filter(method_type == !!method_type)
  
  if (nrow(type_data) < 2) {
    return(results)
  }
  
  for (strategy in unique(type_data$strategy)) {
    if (strategy == "Non-Sequential") {
      next
    }
    
    strategy_data <- type_data %>% filter(strategy == !!strategy)
    
    for (position in unique(strategy_data$method_position)) {
      pos_data <- strategy_data %>% filter(method_position == position)
      
      if (nrow(pos_data) < 2) {
        next
      }
      
      orig_meta <- meta_analysis(pos_data$proportion, pos_data$sample_size)
      
      results <- rbind(results, data.frame(
        Method_Type = method_type,
        Strategy = strategy,
        Position = position,
        Proportion_Type = "Original",
        Pooled_Prevalence = orig_meta$pooled_p,
        Lower_CI = orig_meta$lower_ci,
        Upper_CI = orig_meta$upper_ci,
        Sample_Size = orig_meta$n_sum,
        N_Studies = orig_meta$n_studies,
        I_Squared = orig_meta$i_squared,
        stringsAsFactors = FALSE
      ))
      
      if (strategy == "Sequential") {
        adj_meta <- meta_analysis(pos_data$adjusted_proportion, pos_data$remaining_sample)
        
        results <- rbind(results, data.frame(
          Method_Type = method_type,
          Strategy = strategy,
          Position = position,
          Proportion_Type = "Adjusted",
          Pooled_Prevalence = adj_meta$pooled_p,
          Lower_CI = adj_meta$lower_ci,
          Upper_CI = adj_meta$upper_ci,
          Sample_Size = sum(pos_data$remaining_sample),
          N_Studies = adj_meta$n_studies,
          I_Squared = adj_meta$i_squared,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

combine_results <- function(overall_results, method_results, method_type_results) {
  if (nrow(method_results) > 0) {
    method_results$Analysis_Level <- "Method"
  }
  
  if (nrow(method_type_results) > 0) {
    method_type_results$Analysis_Level <- "Method_Type"
    method_type_results$Method_Code <- NA
    method_type_results$Method_Name <- NA
  }
  
  if (nrow(method_results) > 0) {
    method_results <- method_results %>%
      select(Analysis_Level, Method_Code, Method_Name, Method_Type, 
             Strategy, Position, Proportion_Type, everything())
  }
  
  if (nrow(method_type_results) > 0) {
    method_type_results <- method_type_results %>%
      select(Analysis_Level, Method_Code, Method_Name, Method_Type, 
             Strategy, Position, Proportion_Type, everything())
  }
  
  if (nrow(overall_results) > 0) {
    overall_results$Method_Code <- NA
    overall_results$Method_Name <- NA
    overall_results$Method_Type <- NA
    overall_results$Position <- NA
    overall_results$Proportion_Type <- NA
    
    overall_results <- overall_results %>%
      select(Analysis_Level, Method_Code, Method_Name, Method_Type, 
             Strategy, Position, Proportion_Type, everything())
  }
  
  combined_results <- bind_rows(overall_results, method_results, method_type_results)
  
  return(combined_results)
}

main <- function() {
  if (!dir.exists("output/order_effects/results")) {
    dir.create("output/order_effects/results", recursive = TRUE)
  }
  
  codebook <- load_codebook("codebook.json")
  
  cat("Analyzing overall careless responding by screening strategy...\n")
  overall_results <- analyze_overall_cr()
  
  write_csv(overall_results, "output/order_effects/results/overall_meta_results.csv")
  cat("Saved overall results with", nrow(overall_results), "rows\n")
  
  cat("Finding method datasets...\n")
  method_files <- find_datasets("method_[0-9]+\\.csv")
  cat("Found", length(method_files), "method datasets\n")
  
  method_results <- data.frame(
    Method_Code = integer(),
    Method_Name = character(),
    Strategy = character(),
    Position = integer(),
    Proportion_Type = character(),
    Pooled_Prevalence = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    Sample_Size = integer(),
    N_Studies = integer(),
    I_Squared = numeric(),
    Method_Type = character(),
    stringsAsFactors = FALSE
  )
  
  for (file in method_files) {
    method_code <- as.integer(gsub(".*method_([0-9]+)\\.csv", "\\1", file))
    method_name <- tryCatch(
      codebook$cr_method[[as.character(method_code)]],
      error = function(e) paste("Method", method_code)
    )
    
    cat("Analyzing method", method_code, ":", method_name, "\n")
    
    dataset <- load_method_dataset(file)
    if (!is.null(dataset)) {
      if (!"method_type" %in% colnames(dataset)) {
        cat("  Warning: method_type column missing, using 'unknown'\n")
        dataset$method_type <- "unknown"
      }
      
      results <- analyze_method_dataset(dataset, method_code, method_name)
      
      if (nrow(results) > 0) {
        method_results <- bind_rows(method_results, results)
      } else {
        cat("  No valid results found for method", method_code, "\n")
      }
    }
  }
  
  if (nrow(method_results) > 0) {
    write_csv(method_results, "output/order_effects/results/method_meta_results.csv")
    cat("Saved method results with", nrow(method_results), "rows\n")
  } else {
    cat("Warning: No valid method results found\n")
  }
  
  cat("Finding method type datasets...\n")
  method_type_files <- find_datasets("method_type_.*\\.csv")
  cat("Found", length(method_type_files), "method type datasets\n")
  
  method_type_results <- data.frame()
  
  for (file in method_type_files) {
    method_type <- gsub(".*method_type_(.*)\\.csv", "\\1", file)
    
    cat("Analyzing method type:", method_type, "\n")
    
    dataset <- load_method_dataset(file)
    if (!is.null(dataset)) {
      results <- analyze_method_type_dataset(dataset, method_type)
      
      if (nrow(results) > 0) {
        method_type_results <- bind_rows(method_type_results, results)
      } else {
        cat("  No valid results found for method type", method_type, "\n")
      }
    }
  }
  
  if (nrow(method_type_results) > 0) {
    write_csv(method_type_results, "output/order_effects/results/method_type_meta_results.csv")
    cat("Saved method type results with", nrow(method_type_results), "rows\n")
  } else {
    cat("Warning: No valid method type results found\n")
  }
  
  combined_results <- combine_results(overall_results, method_results, method_type_results)
  write_csv(combined_results, "output/order_effects/results/combined_meta_results.csv")
  cat("Saved combined results with", nrow(combined_results), "rows\n")
  
  return(list(
    overall_results = overall_results,
    method_results = method_results,
    method_type_results = method_type_results,
    combined_results = combined_results
  ))
}

main()