###############################################################################
# R script to calculate proportions with order effects adjustment
###############################################################################

# Set CRAN mirror first to avoid mirror selection error
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required packages if not already installed
required_packages <- c("dplyr", "tidyr", "openxlsx", "jsonlite")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

library(dplyr)
library(tidyr)
library(openxlsx)
library(jsonlite)

# Load codebook
load_codebook <- function(filename) {
  if (!file.exists(filename)) {
    warning("Codebook file not found: ", filename)
    return(list())
  }
  return(fromJSON(filename))
}

# Calculate standard errors for a given proportion and sample size
compute_standard_errors <- function(p, n) {
  if (is.na(p) || is.na(n) || n == 0) {
    return(NA)
  }
  p <- pmin(pmax(p, 0), 1)  # Clip between 0 and 1
  return(sqrt(p * (1 - p) / n))
}

# Compute confidence interval for a proportion
compute_confidence_interval <- function(p, n, alpha = 0.05) {
  if (is.na(p) || is.na(n) || n == 0) {
    return(c(NA, NA))
  }
  p <- pmin(pmax(p, 0), 1)  # Clip between 0 and 1
  z <- qnorm(1 - alpha/2)
  se <- compute_standard_errors(p, n)
  ci_lower <- p - z * se
  ci_upper <- p + z * se
  ci_lower <- pmin(pmax(ci_lower, 0), 1)  # Clip between 0 and 1
  ci_upper <- pmin(pmax(ci_upper, 0), 1)  # Clip between 0 and 1
  return(c(round(ci_lower, 4), round(ci_upper, 4)))
}

# Add statistics to a dataframe
add_stats_to_df <- function(df, proportion_col) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  df$se <- sapply(1:nrow(df), function(i) {
    compute_standard_errors(df[[proportion_col]][i], df$sample_size[i])
  })
  
  ci <- t(sapply(1:nrow(df), function(i) {
    compute_confidence_interval(df[[proportion_col]][i], df$sample_size[i])
  }))
  
  df$ci_lower <- ci[, 1]
  df$ci_upper <- ci[, 2]
  
  return(df)
}

# Calculate adjusted proportions that account for order effects
calculate_adjusted_proportions <- function(data) {
  # Create a copy of the data
  adjusted_data <- data
  
  # Initialize column for remaining sample after each CR method
  adjusted_data$remaining_sample <- adjusted_data$sample_size
  
  # Calculate adjusted proportions for each CR method (1-4)
  for (i in 1:4) {
    method_col <- paste0("cr_", i, "_method")
    amount_col <- paste0("cr_", i, "_amount")
    
    # Check if these columns exist
    if (method_col %in% colnames(adjusted_data) && amount_col %in% colnames(adjusted_data)) {
      # Create an adjusted proportion column
      adj_prop_col <- paste0("cr_", i, "_adj_proportion")
      
      # Calculate adjusted proportion based on remaining sample
      adjusted_data[[adj_prop_col]] <- ifelse(
        !is.na(adjusted_data[[method_col]]) & !is.na(adjusted_data[[amount_col]]) & adjusted_data$remaining_sample > 0,
        adjusted_data[[amount_col]] / adjusted_data$remaining_sample,
        NA
      )
      
      # Update remaining sample for next method (subtract detected careless responders)
      adjusted_data$remaining_sample <- ifelse(
        !is.na(adjusted_data[[amount_col]]),
        adjusted_data$remaining_sample - adjusted_data[[amount_col]],
        adjusted_data$remaining_sample
      )
    }
  }
  
  # Calculate average adjusted proportion for each CR method type
  method_types <- c("attention_check", "consistency", "response_time", "outlier", "response_pattern", "self_report")
  
  for (type in method_types) {
    # Initialize columns
    type_col <- paste0(type, "_adj_proportion")
    adjusted_data[[type_col]] <- NA
    adjusted_data[[paste0(type, "_methods_used")]] <- 0
    adjusted_data[[paste0(type, "_total_identified")]] <- 0
    
    # Map methods to types based on codebook or known mapping
    # This is a simplified example - you'll need to adapt based on your codebook
    method_to_type <- list(
      "attention_check" = c(0, 12, 13),  # Example mapping
      "consistency" = c(9, 10, 11, 15, 16),
      "response_time" = c(1),
      "outlier" = c(4, 5),
      "response_pattern" = c(2, 3, 14),
      "self_report" = c(6, 7, 8)
    )
    
    # Find methods of this type
    methods_of_type <- method_to_type[[type]]
    
    # For each method position, check if it's this type and add to totals
    for (i in 1:4) {
      method_col <- paste0("cr_", i, "_method")
      amount_col <- paste0("cr_", i, "_amount")
      adj_prop_col <- paste0("cr_", i, "_adj_proportion")
      
      if (all(c(method_col, adj_prop_col) %in% colnames(adjusted_data))) {
        # Count methods of this type
        is_this_type <- !is.na(adjusted_data[[method_col]]) & adjusted_data[[method_col]] %in% methods_of_type
        adjusted_data[[paste0(type, "_methods_used")]] <- adjusted_data[[paste0(type, "_methods_used")]] + is_this_type
        
        # Add to total identified
        if (amount_col %in% colnames(adjusted_data)) {
          identified <- ifelse(is_this_type & !is.na(adjusted_data[[amount_col]]), 
                             adjusted_data[[amount_col]], 0)
          adjusted_data[[paste0(type, "_total_identified")]] <- adjusted_data[[paste0(type, "_total_identified")]] + identified
        }
        
        # Calculate weighted average of adjusted proportions
        if (sum(is_this_type, na.rm = TRUE) > 0) {
          type_props <- ifelse(is_this_type, adjusted_data[[adj_prop_col]], NA)
          adjusted_data[[type_col]] <- ifelse(
            is.na(adjusted_data[[type_col]]),
            type_props,
            pmax(adjusted_data[[type_col]], type_props, na.rm = TRUE)
          )
        }
      }
    }
    
    # Calculate overall proportion for this type (based on original sample size)
    overall_type_col <- paste0(type, "_overall_proportion")
    adjusted_data[[overall_type_col]] <- adjusted_data[[paste0(type, "_total_identified")]] / adjusted_data$sample_size
  }
  
  # Calculate adjusted overall CR proportion
  adjusted_data$adjusted_cr_proportion <- (adjusted_data$sample_size - adjusted_data$remaining_sample) / adjusted_data$sample_size
  
  return(adjusted_data)
}

# Main function
main <- function() {
  # Load data and codebook
  cat("Loading data and codebook...\n")
  codebook <- load_codebook('codebook.json')
  
  # Find main data file
  possible_files <- c(
    "data/careless_data.csv",
    "careless_data.csv"
  )
  
  data_file <- NULL
  for (file in possible_files) {
    if (file.exists(file)) {
      data_file <- file
      cat("Using data file:", data_file, "\n")
      break
    }
  }
  
  if (is.null(data_file)) {
    stop("No data file found. Tried: ", paste(possible_files, collapse=", "))
  }
  
  data <- read.csv(data_file, stringsAsFactors = FALSE)
  cat("Loaded data with", nrow(data), "rows and", ncol(data), "columns\n")
  
  # Calculate adjusted proportions
  cat("Calculating adjusted proportions...\n")
  adjusted_data <- calculate_adjusted_proportions(data)
  
  # Create output directory if it doesn't exist
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  # Save adjusted dataset
  write.csv(adjusted_data, "output/careless_data_with_adjusted_proportions.csv", row.names = FALSE)
  cat("Saved adjusted dataset to output/careless_data_with_adjusted_proportions.csv\n")
  
  # Compute aggregate statistics for original and adjusted proportions
  cat("Computing aggregate statistics...\n")
  
  # Original proportion stats
  if ("proportions_total" %in% colnames(adjusted_data)) {
    original_prop_col <- "proportions_total"
  } else if ("cr_total_amount" %in% colnames(adjusted_data) && "sample_size" %in% colnames(adjusted_data)) {
    adjusted_data$proportions_total <- adjusted_data$cr_total_amount / adjusted_data$sample_size
    original_prop_col <- "proportions_total"
  } else {
    cat("Warning: Could not find or compute original proportions\n")
    original_prop_col <- NULL
  }
  
  # Create a summary dataframe
  summary_stats <- data.frame(
    statistic = character(),
    original_value = numeric(),
    adjusted_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(original_prop_col)) {
    # Calculate summary stats for original proportions
    orig_mean <- mean(adjusted_data[[original_prop_col]], na.rm = TRUE)
    orig_median <- median(adjusted_data[[original_prop_col]], na.rm = TRUE)
    orig_sd <- sd(adjusted_data[[original_prop_col]], na.rm = TRUE)
    orig_min <- min(adjusted_data[[original_prop_col]], na.rm = TRUE)
    orig_max <- max(adjusted_data[[original_prop_col]], na.rm = TRUE)
    
    # Calculate summary stats for adjusted proportions
    adj_mean <- mean(adjusted_data$adjusted_cr_proportion, na.rm = TRUE)
    adj_median <- median(adjusted_data$adjusted_cr_proportion, na.rm = TRUE)
    adj_sd <- sd(adjusted_data$adjusted_cr_proportion, na.rm = TRUE)
    adj_min <- min(adjusted_data$adjusted_cr_proportion, na.rm = TRUE)
    adj_max <- max(adjusted_data$adjusted_cr_proportion, na.rm = TRUE)
    
    # Add to summary dataframe
    summary_stats <- rbind(summary_stats, 
                         data.frame(
                           statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
                           original_value = c(orig_mean, orig_median, orig_sd, orig_min, orig_max),
                           adjusted_value = c(adj_mean, adj_median, adj_sd, adj_min, adj_max),
                           stringsAsFactors = FALSE
                         ))
  }
  
  # Save summary stats
  write.csv(summary_stats, "output/proportion_adjustment_summary.csv", row.names = FALSE)
  cat("Saved summary statistics to output/proportion_adjustment_summary.csv\n")
  
  # Calculate proportions by CR method type
  cat("Calculating proportions by CR method type...\n")
  
  method_types <- c("attention_check", "consistency", "response_time", "outlier", "response_pattern", "self_report")
  
  type_stats <- data.frame(
    method_type = character(),
    original_mean = numeric(),
    adjusted_mean = numeric(),
    difference = numeric(),
    studies_using = integer(),
    stringsAsFactors = FALSE
  )
  
  for (type in method_types) {
    overall_type_col <- paste0(type, "_overall_proportion")
    adj_type_col <- paste0(type, "_adj_proportion")
    
    if (all(c(overall_type_col, adj_type_col) %in% colnames(adjusted_data))) {
      studies_using <- sum(!is.na(adjusted_data[[adj_type_col]]))
      
      if (studies_using > 0) {
        orig_mean <- mean(adjusted_data[[overall_type_col]], na.rm = TRUE)
        adj_mean <- mean(adjusted_data[[adj_type_col]], na.rm = TRUE)
        
        type_stats <- rbind(type_stats, 
                          data.frame(
                            method_type = type,
                            original_mean = orig_mean,
                            adjusted_mean = adj_mean,
                            difference = adj_mean - orig_mean,
                            studies_using = studies_using,
                            stringsAsFactors = FALSE
                          ))
      }
    }
  }
  
  # Save method type stats
  write.csv(type_stats, "output/method_type_adjustment_effects.csv", row.names = FALSE)
  cat("Saved method type statistics to output/method_type_adjustment_effects.csv\n")
  
  # Create Excel workbook with multiple sheets
  wb <- createWorkbook()
  
  # Add main data with adjustments
  addWorksheet(wb, "Adjusted Data")
  writeData(wb, "Adjusted Data", adjusted_data)
  
  # Add summary statistics
  addWorksheet(wb, "Summary Statistics")
  writeData(wb, "Summary Statistics", summary_stats)
  
  # Add method type statistics
  addWorksheet(wb, "Method Type Effects")
  writeData(wb, "Method Type Effects", type_stats)
  
  # Save Excel workbook
  saveWorkbook(wb, "output/careless_responding_with_order_effects.xlsx", overwrite = TRUE)
  cat("Saved combined results to output/careless_responding_with_order_effects.xlsx\n")
  
  cat("Analysis complete.\n")
}

# Run the main function
main()