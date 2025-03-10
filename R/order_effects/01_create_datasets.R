###############################################################################
# R script to create datasets for order effects analysis in careless responding
#
# This script creates specialized datasets for analyzing order effects in 
# careless responding detection. It splits the main dataset into three strategy-based
# datasets (single method, sequential, non-sequential) and creates method-specific 
# datasets for both individual methods and method types.
#
# Workflow:
# 1. Load required packages:
#    - dplyr: For data manipulation and transformation
#    - tidyr: For reshaping data
#    - jsonlite: For parsing JSON codebook
#
# 2. Define utility functions:
#    - load_codebook: Loads category definitions from JSON
#    - apply_method_type: Maps individual methods to broader categories
#    - create_strategy_datasets: Splits data by screening strategy
#
# 3. Define dataset creation functions:
#    - create_method_dataset: For individual CR detection methods
#    - create_method_type_dataset: For broader method categories
#    - calculate_remaining_sample: Helper for sequential screening adjustments
#
# 4. Define main function that:
#    - Loads codebook and raw data
#    - Creates and saves strategy-based datasets
#    - Generates and saves method-specific datasets
#    - Creates method-type datasets
#    - Produces a dataset for meta-regression analysis
#
# Key output datasets:
# - strategy_single.csv: Studies using a single CR detection method
# - strategy_sequential.csv: Studies using sequential screening
# - strategy_non_sequential.csv: Studies using multiple non-sequential methods
# - method_[code].csv: For each specific CR detection method
# - method_type_[name].csv: For each broader method category
# - position_regression.csv: Dataset for meta-regression with position as moderator
#
###############################################################################

library(dplyr)
library(tidyr)
library(jsonlite)
library(readr)

load_codebook <- function(filename) {
  return(fromJSON(filename))
}

apply_method_type <- function(method_code, codebook) {
  get_type <- function(single_code) {
    single_code <- as.character(single_code)
    for (type_name in names(codebook$cr_method_type)) {
      if (single_code %in% as.character(codebook$cr_method_type[[type_name]])) {
        return(type_name)
      }
    }
    return("other")
  }
  
  if (length(method_code) > 1) {
    return(sapply(method_code, get_type))
  } else {
    return(get_type(method_code))
  }
}

create_strategy_datasets <- function(data) {
  if (!dir.exists("output/order_effects/data")) {
    dir.create("output/order_effects/data", recursive = TRUE)
  }
  
  single_data <- data %>% filter(cr_multiple == 0)
  write.csv(single_data, "output/order_effects/data/strategy_single.csv", row.names = FALSE)
  
  sequential_data <- data %>% filter(cr_multiple == 1, cr_sequential == 1)
  write.csv(sequential_data, "output/order_effects/data/strategy_sequential.csv", row.names = FALSE)
  
  non_sequential_data <- data %>% filter(cr_multiple == 1, cr_sequential == 0)
  write.csv(non_sequential_data, "output/order_effects/data/strategy_non_sequential.csv", row.names = FALSE)
  
  return(list(
    single = single_data,
    sequential = sequential_data,
    non_sequential = non_sequential_data
  ))
}

calculate_remaining_sample <- function(row, current_pos) {
  remaining <- row$sample_size
  
  for (i in 1:(current_pos-1)) {
    amount_col <- paste0("cr_", i, "_amount")
    
    if (amount_col %in% names(row)) {
      amount <- row[[amount_col]]
      
      if (!is.na(amount) && is.numeric(amount) && amount >= 0) {
        remaining <- remaining - amount
      }
    }
  }
  
  return(max(remaining, 1))
}

create_method_dataset <- function(method_code, strategy_datasets, codebook) {
  method_name <- tryCatch(
    codebook$cr_method[[as.character(method_code)]],
    error = function(e) paste("Method", method_code)
  )
  
  result <- data.frame()
  
  single_method <- strategy_datasets$single %>%
    filter(cr_1_method == method_code) %>%
    mutate(
      strategy = "Single Method",
      method_position = 1,
      method_code = method_code,
      method_name = method_name,
      cr_amount = cr_1_amount,
      proportion = cr_amount / sample_size,
      remaining_sample = sample_size,
      adjusted_proportion = proportion
    )
  
  if (nrow(single_method) > 0) {
    single_method$method_type <- apply_method_type(method_code, codebook)
    
    single_method <- single_method %>%
      select(
        ID, strategy, method_position, method_code, method_name, 
        method_type, sample_size, remaining_sample, cr_amount, 
        proportion, adjusted_proportion
      )
    
    result <- bind_rows(result, single_method)
  }
  
  for (pos in 1:4) {
    method_col <- paste0("cr_", pos, "_method")
    amount_col <- paste0("cr_", pos, "_amount")
    
    sequential_pos <- strategy_datasets$sequential %>%
      filter(.data[[method_col]] == method_code)
    
    if (nrow(sequential_pos) > 0) {
      sequential_pos <- sequential_pos %>%
        mutate(
          strategy = "Sequential",
          method_position = pos,
          method_code = method_code,
          method_name = method_name,
          cr_amount = .data[[amount_col]],
          proportion = cr_amount / sample_size
        )
      
      remaining_samples <- numeric(nrow(sequential_pos))
      for (i in 1:nrow(sequential_pos)) {
        row <- sequential_pos[i,]
        remaining_samples[i] <- calculate_remaining_sample(row, pos)
      }
      sequential_pos$remaining_sample <- remaining_samples
      sequential_pos$adjusted_proportion <- sequential_pos$cr_amount / sequential_pos$remaining_sample
      sequential_pos$method_type <- apply_method_type(method_code, codebook)
      
      sequential_pos <- sequential_pos %>%
        select(
          ID, strategy, method_position, method_code, method_name, 
          method_type, sample_size, remaining_sample, cr_amount, 
          proportion, adjusted_proportion
        )
      
      result <- bind_rows(result, sequential_pos)
    }
  }
  
  for (pos in 1:4) {
    method_col <- paste0("cr_", pos, "_method")
    amount_col <- paste0("cr_", pos, "_amount")
    
    non_sequential_pos <- strategy_datasets$non_sequential %>%
      filter(.data[[method_col]] == method_code)
    
    if (nrow(non_sequential_pos) > 0) {
      non_sequential_pos <- non_sequential_pos %>%
        mutate(
          strategy = "Non-Sequential",
          method_position = pos,
          method_code = method_code,
          method_name = method_name,
          cr_amount = .data[[amount_col]],
          proportion = cr_amount / sample_size,
          remaining_sample = sample_size,
          adjusted_proportion = proportion
        )
      
      non_sequential_pos$method_type <- apply_method_type(method_code, codebook)
      
      non_sequential_pos <- non_sequential_pos %>%
        select(
          ID, strategy, method_position, method_code, method_name, 
          method_type, sample_size, remaining_sample, cr_amount, 
          proportion, adjusted_proportion
        )
      
      result <- bind_rows(result, non_sequential_pos)
    }
  }
  
  if (nrow(result) > 0) {
    write.csv(result, paste0("output/order_effects/data/method_", method_code, ".csv"), row.names = FALSE)
  }
  
  return(result)
}

create_method_type_dataset <- function(method_type, method_datasets) {
  type_dataset <- bind_rows(method_datasets) %>%
    filter(method_type == method_type)
  
  if (nrow(type_dataset) > 0) {
    filename <- paste0("output/order_effects/data/method_type_", method_type, ".csv")
    write.csv(type_dataset, filename, row.names = FALSE)
  }
  
  return(type_dataset)
}

# Enhanced position dataset function for 01_create_datasets.R
create_position_regression_dataset <- function(strategy_datasets, codebook) {
  cat("Creating position regression dataset...\n")
  
  # Process single method studies (as position 1)
  single_data <- strategy_datasets$single %>%
    filter(!is.na(cr_1_method), cr_1_method != -1) %>%
    mutate(
      method_position = 1,
      method_code = cr_1_method,
      cr_amount = cr_1_amount,
      strategy = "Single Method",
      remaining_sample = sample_size,
      proportion = cr_amount / sample_size,
      adjusted_proportion = proportion,
      position_factor = as.factor(1)
    )
  
  # Add method name and type information
  if (nrow(single_data) > 0) {
    single_data$method_name <- sapply(single_data$method_code, function(code) {
      tryCatch(
        codebook$cr_method[[as.character(code)]],
        error = function(e) paste("Method", code)
      )
    })
    
    single_data$method_type <- sapply(single_data$method_code, function(code) {
      apply_method_type(code, codebook)
    })
    
    cat("  Added", nrow(single_data), "single method studies (as position 1)\n")
  } else {
    cat("  No valid single method studies found\n")
  }
  
  # Process sequential screening studies
  sequential_data <- strategy_datasets$sequential
  sequential_processed <- data.frame()
  
  if (nrow(sequential_data) > 0) {
    # Process each position
    for (pos in 1:4) {
      method_col <- paste0("cr_", pos, "_method")
      amount_col <- paste0("cr_", pos, "_amount")
      
      pos_data <- sequential_data %>%
        filter(!is.na(.data[[method_col]]), .data[[method_col]] != -1) %>%
        mutate(
          method_position = pos,
          method_code = .data[[method_col]],
          cr_amount = .data[[amount_col]],
          strategy = "Sequential"
        )
      
      if (nrow(pos_data) > 0) {
        # Add method information and calculate proportions
        pos_data$method_name <- sapply(pos_data$method_code, function(code) {
          tryCatch(
            codebook$cr_method[[as.character(code)]],
            error = function(e) paste("Method", code)
          )
        })
        
        pos_data$method_type <- sapply(pos_data$method_code, function(code) {
          apply_method_type(code, codebook)
        })
        
        # Calculate remaining sample and proportions
        remaining_samples <- numeric(nrow(pos_data))
        for (i in 1:nrow(pos_data)) {
          row <- pos_data[i,]
          remaining_samples[i] <- calculate_remaining_sample(row, pos)
        }
        pos_data$remaining_sample <- remaining_samples
        pos_data$proportion <- pos_data$cr_amount / pos_data$sample_size
        pos_data$adjusted_proportion <- pos_data$cr_amount / pos_data$remaining_sample
        pos_data$position_factor <- as.factor(pos)
        
        sequential_processed <- bind_rows(sequential_processed, pos_data)
        cat("  Added", nrow(pos_data), "sequential studies for position", pos, "\n")
      } else {
        cat("  No sequential studies found for position", pos, "\n")
      }
    }
  } else {
    cat("  No sequential studies found\n")
  }
  
  # Combine all data
  position_data <- bind_rows(single_data, sequential_processed)
  
  # Select relevant columns
  position_data <- position_data %>%
    select(
      ID, year, strategy, method_position, position_factor, 
      method_code, method_name, method_type, sample_size, 
      remaining_sample, cr_amount, proportion, adjusted_proportion
    )
  
  # Provide summary statistics
  cat("\nPosition regression dataset summary:\n")
  cat("  Total observations:", nrow(position_data), "\n")
  
  strategy_counts <- table(position_data$strategy)
  cat("  By strategy:\n")
  for (strat in names(strategy_counts)) {
    cat("    ", strat, ":", strategy_counts[strat], "observations\n")
  }
  
  position_counts <- table(position_data$method_position)
  cat("  By position:\n")
  for (pos in names(position_counts)) {
    cat("    Position", pos, ":", position_counts[pos], "observations\n")
  }
  
  # Save dataset
  write_csv(position_data, "output/order_effects/data/position_regression.csv")
  
  return(position_data)
}

main <- function() {
  codebook <- load_codebook("codebook.json")
  data <- read.csv("data/careless_data.csv", stringsAsFactors = FALSE)
  
  if (!dir.exists("output/order_effects")) {
    dir.create("output/order_effects", recursive = TRUE)
  }
  
  cat("Creating strategy-based datasets...\n")
  strategy_datasets <- create_strategy_datasets(data)
  
  all_methods <- c()
  for (i in 1:4) {
    method_col <- paste0("cr_", i, "_method")
    methods <- unique(data[[method_col]])
    all_methods <- c(all_methods, methods)
  }
  all_methods <- unique(all_methods)
  all_methods <- all_methods[all_methods != -1 & !is.na(all_methods)]
  
  cat("Creating method-specific datasets...\n")
  method_datasets <- list()
  for (method in all_methods) {
    cat("  Processing method", method, ":", 
        tryCatch(codebook$cr_method[[as.character(method)]], 
                error = function(e) paste("Method", method)), "\n")
    method_datasets[[as.character(method)]] <- create_method_dataset(method, strategy_datasets, codebook)
  }
  
  cat("Creating method type datasets...\n")
  method_types <- unique(unlist(lapply(method_datasets, function(df) unique(df$method_type))))
  for (type in method_types) {
    cat("  Processing method type:", type, "\n")
    create_method_type_dataset(type, method_datasets)
  }
  
  cat("Creating position regression dataset...\n")
  position_dataset <- create_position_regression_dataset(strategy_datasets, codebook)
  
  cat("\nDataset Creation Summary:\n")
  cat("  Created", length(strategy_datasets), "strategy-based datasets\n")
  cat("  Created", length(method_datasets), "method-specific datasets\n")
  cat("  Created", length(method_types), "method-type datasets\n")
  cat("  Created position regression dataset with", nrow(position_dataset), "observations\n")
  
  return(list(
    strategy_datasets = strategy_datasets,
    method_datasets = method_datasets,
    position_dataset = position_dataset
  ))
}

main()