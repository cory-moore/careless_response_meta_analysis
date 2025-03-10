###############################################################################
# R script to calculate proportions of careless responding in survey data
#
# This script calculates various proportions of careless responding across
# different categories and subgroups, computes standard errors and confidence
# intervals, and exports the results to an Excel workbook.
#
# Workflow:
# 1. Load required packages:
#    - dplyr: For data manipulation and transformation
#    - tidyr: For reshaping data
#    - openxlsx: For reading/writing Excel files
#    - jsonlite: For parsing JSON codebook
#
# 2. Define utility functions:
#    - load_codebook: Loads category definitions from JSON
#    - compute_standard_errors: Calculates SE using binomial formula
#    - compute_confidence_interval: Calculates 95% CIs with proper bounds
#    - add_stats_to_df: Adds SE and CIs to proportion dataframes
#
# 3. Define proportion calculation functions:
#    - compute_proportions: Calculates overall proportions
#    - compute_proportions_by_year: Groups and calculates by publication year
#    - compute_proportions_by_journal: Groups by journal with name lookup
#    - compute_proportions_by_sample_source: Groups by sample source type
#    - compute_proportions_by_sample_method: Groups by sampling methodology
#    - compute_proportions_by_sample_platform: Groups by data collection platform
#    - subset_data_by_cr_method: Extracts data for specific CR detection methods
#    - compute_cr_method_proportions: Calculates proportions by CR method
#
# 4. Define process_group function:
#    - Applies proportion calculations across multiple category values
#    - Handles errors gracefully for each subgroup
#    - Adds statistical measures to each result
#    - Combines results into consolidated dataframes
#
# 5. Define main function that:
#    - Loads codebook and raw data
#    - Maps journal names to codes for consistent processing
#    - Calculates overall proportions across the entire dataset
#    - Defines category codes for all grouping variables
#    - Processes all category groups using the process_group function
#    - Creates Excel workbook with separate sheets for each category
#    - Saves results to output directory
#
###############################################################################

library(dplyr)
library(tidyr)
library(openxlsx)
library(jsonlite)

load_codebook <- function(filename) {
  return(fromJSON(filename))
}

compute_standard_errors <- function(p, n) {
  if (is.na(p) || is.na(n) || n == 0) {
    return(NA)
  }
  p <- pmin(pmax(p, 0), 1)
  return(sqrt(p * (1 - p) / n))
}

compute_confidence_interval <- function(p, n, alpha = 0.05) {
  if (is.na(p) || is.na(n) || n == 0) {
    return(c(NA, NA))
  }
  p <- pmin(pmax(p, 0), 1)
  z <- qnorm(1 - alpha/2)
  se <- compute_standard_errors(p, n)
  ci_lower <- p - z * se
  ci_upper <- p + z * se
  ci_lower <- pmin(pmax(ci_lower, 0), 1)
  ci_upper <- pmin(pmax(ci_upper, 0), 1)
  return(c(round(ci_lower, 4), round(ci_upper, 4)))
}

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

compute_proportions <- function(data, cr_total) {
  cr_proportion_total <- data[[cr_total]] / data$sample_size
  data$proportions_total <- round(cr_proportion_total, 4)
  return(data)
}

compute_proportions_by_year <- function(data, year) {
  subset_df <- data %>% 
    filter(year == !!year) %>%
    mutate(proportions_year = round(cr_total_amount / sample_size, 4),
           year = !!year) %>%
    select(ID, year, sample_size, cr_total_amount, proportions_year)
  
  return(subset_df)
}

compute_proportions_by_journal <- function(data, journal_code, codebook) {
  subset_df <- data %>% 
    filter(journal_code == !!journal_code) %>%
    mutate(proportions_journal = round(cr_total_amount / sample_size, 4),
           journal_name = tryCatch(codebook$journal[[as.character(journal_code)]], 
                                  error = function(e) paste("Journal", journal_code))) %>%
    select(ID, journal_code, journal_name, sample_size, cr_total_amount, proportions_journal)
  
  return(subset_df)
}

compute_proportions_by_sample_source <- function(data, source_code, codebook) {
  subset_df <- data %>% 
    filter(sample_source == !!source_code) %>%
    mutate(proportions_sample_source = round(cr_total_amount / sample_size, 4),
           sample_source_name = tryCatch(codebook$sample_source[[as.character(source_code)]], 
                                       error = function(e) paste("Source", source_code))) %>%
    select(ID, sample_source, sample_source_name, sample_size, cr_total_amount, proportions_sample_source)
  
  return(subset_df)
}

compute_proportions_by_sample_method <- function(data, method_code, codebook) {
  subset_df <- data %>% 
    filter(sample_method == !!method_code) %>%
    mutate(proportions_sample_method = round(cr_total_amount / sample_size, 4),
           sample_method_name = tryCatch(codebook$sample_method[[as.character(method_code)]], 
                                       error = function(e) paste("Method", method_code))) %>%
    select(ID, sample_method, sample_method_name, sample_size, cr_total_amount, proportions_sample_method)
  
  return(subset_df)
}

compute_proportions_by_sample_platform <- function(data, platform_code, codebook) {
  subset_df <- data %>% 
    filter(sample_platform == !!platform_code) %>%
    mutate(proportions_platform = round(cr_total_amount / sample_size, 4),
           sample_platform_name = tryCatch(codebook$sample_platform[[as.character(platform_code)]], 
                                         error = function(e) paste("Platform", platform_code))) %>%
    select(ID, sample_platform, sample_platform_name, sample_size, cr_total_amount, proportions_platform)
  
  return(subset_df)
}

subset_data_by_cr_method <- function(data, cr_method_code, codebook) {
  subset_df <- data %>% 
    filter((cr_multiple == 0 & cr_sequential == -1) | 
           (cr_multiple == 1 & cr_sequential == 1))
  
  results <- data.frame(
    ID = character(),
    cr_method = integer(),
    cr_method_name = character(),
    sample_size = integer(),
    cr_amount = integer(),
    cr_detail = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(subset_df)) {
    row <- subset_df[i,]
    for (j in 1:4) {
      method_col <- paste0("cr_", j, "_method")
      if (!is.na(row[[method_col]]) && row[[method_col]] == cr_method_code) {
        cr_amount <- row[[paste0("cr_", j, "_amount")]]
        cr_method_detail <- row[[paste0("cr_", j, "_method_detail")]]
        cr_method_name <- tryCatch(codebook$cr_method[[as.character(cr_method_code)]], 
                                  error = function(e) paste("Method", cr_method_code))
        
        results <- rbind(results, data.frame(
          ID = row$ID,
          sample_size = row$sample_size,
          cr_method = cr_method_code,
          cr_amount = cr_amount,
          cr_detail = cr_method_detail,
          cr_method_name = cr_method_name,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

compute_cr_method_proportions <- function(data, cr_method_code, codebook) {
  subset_df <- subset_data_by_cr_method(data, cr_method_code, codebook)
  if (nrow(subset_df) > 0) {
    subset_df$proportions_cr_method <- round(subset_df$cr_amount / subset_df$sample_size, 4)
  }
  return(subset_df)
}

process_group <- function(process_func, data, items, dfs_dict, result_key, codebook = NULL) {
  group_dfs <- list()
  
  for (item in items) {
    tryCatch({
      if (!is.null(codebook)) {
        df <- process_func(data, item, codebook)
      } else {
        df <- process_func(data, item)
      }
      
      if (nrow(df) > 0) {
        proportion_col <- tail(colnames(df), 1)[1]
        df <- add_stats_to_df(df, proportion_col)
        group_dfs[[length(group_dfs) + 1]] <- df
      }
    }, error = function(e) {
    })
  }
  
  if (length(group_dfs) > 0) {
    dfs_dict[[result_key]] <- do.call(rbind, group_dfs)
  } else {
    dfs_dict[[result_key]] <- data.frame()
  }
  
  return(dfs_dict)
}

main <- function() {
  codebook <- load_codebook('codebook.json')
  data <- read.csv('data/careless_data.csv', stringsAsFactors = FALSE)
  
  data$journal_code <- 0  
  for (i in 1:nrow(data)) {
    for (j in 0:(length(codebook$journal) - 1)) {
      if (data$journal[i] == codebook$journal[[as.character(j)]]) {
        data$journal_code[i] <- j
        break
      }
    }
  }
  
  dfs <- list()
  
  proportions_total_df <- compute_proportions(data, 'cr_total_amount') %>%
    select(ID, sample_size, cr_total_amount, proportions_total) %>%
    add_stats_to_df('proportions_total')
  
  dfs$proportions_total <- proportions_total_df
  
  years <- 2000:2024
  journal_codes <- 0:(length(codebook$journal) - 1)
  source_codes <- 0:(length(codebook$sample_source) - 2)
  method_codes <- 0:(length(codebook$sample_method) - 2)
  platform_codes <- 0:(length(codebook$sample_platform) - 2)
  cr_method_codes <- 0:(length(codebook$cr_method) - 2)
  
  dfs <- process_group(compute_proportions_by_year, data, years, dfs, 'proportions_year')
  dfs <- process_group(compute_proportions_by_journal, data, journal_codes, dfs, 'proportions_journal', codebook)
  dfs <- process_group(compute_proportions_by_sample_source, data, source_codes, dfs, 'proportions_sample_source', codebook)
  dfs <- process_group(compute_proportions_by_sample_method, data, method_codes, dfs, 'proportions_sample_method', codebook)
  dfs <- process_group(compute_proportions_by_sample_platform, data, platform_codes, dfs, 'proportions_platform', codebook)
  dfs <- process_group(compute_cr_method_proportions, data, cr_method_codes, dfs, 'proportions_cr_method', codebook)
  
  wb <- createWorkbook()
  
  for (name in names(dfs)) {
    if (nrow(dfs[[name]]) > 0) {
      addWorksheet(wb, name)
      writeData(wb, name, dfs[[name]], rowNames = FALSE)
    }
  }
  
  saveWorkbook(wb, "output/proportions_r.xlsx", overwrite = TRUE)
  
  cat("Results saved to output/proportions_r.xlsx\n")
}

main()