# 00_eda_simple.R
#
# A simplified script for examining careless responding data
# and collecting key findings into a single Excel workbook.
# Uses codebook for human-readable names.

library(tidyverse)
library(openxlsx)  # For Excel output
library(jsonlite)  # For reading the codebook JSON

# Create output directory
dir.create("output/data_examination", showWarnings = FALSE, recursive = TRUE)

# Create a workbook for all findings
wb <- createWorkbook()

# 1. LOAD DATA AND CODEBOOK ----

# Load codebook
load_codebook <- function(path = "codebook.json") {
  if (!file.exists(path)) {
    warning("Codebook file not found at: ", path)
    return(NULL)
  }
  
  tryCatch({
    codebook <- fromJSON(path)
    return(codebook)
  }, error = function(e) {
    warning("Error reading codebook: ", e$message)
    return(NULL)
  })
}

codebook <- load_codebook()

# Function to get human-readable labels from codebook
get_readable_name <- function(value, variable) {
  if (is.null(codebook) || !variable %in% names(codebook)) {
    return(as.character(value))  # Return as-is if no mapping available
  }
  
  var_mapping <- codebook[[variable]]
  value_str <- as.character(value)
  
  if (value_str %in% names(var_mapping)) {
    return(var_mapping[[value_str]])
  } else {
    return(value_str)  # Return original if not found in mapping
  }
}

# Function to apply codebook mapping to dataframe
apply_codebook_mapping <- function(df, var_name) {
  if (!var_name %in% names(df) || is.null(codebook) || !var_name %in% names(codebook)) {
    return(df)  # Return unchanged if mapping not possible
  }
  
  # Create a new column with mapped values
  mapped_col_name <- paste0(var_name, "_label")
  df[[mapped_col_name]] <- sapply(df[[var_name]], get_readable_name, variable = var_name)
  
  # Reorder columns to put label right after the original variable
  var_pos <- which(names(df) == var_name)
  new_order <- c(
    names(df)[1:var_pos],
    mapped_col_name,
    names(df)[(var_pos+1):(length(names(df))-1)]
  )
  
  df <- df[, new_order]
  return(df)
}

# Load all datasets
first_method <- read_csv("data/processed/first_method_data.csv", show_col_types = FALSE)
single_method <- read_csv("data/processed/single_method_data.csv", show_col_types = FALSE)
sequential <- try(read_csv("data/processed/sequential_data.csv", show_col_types = FALSE), silent = TRUE)

# Handle potentially missing sequential data
if(inherits(sequential, "try-error")) {
  sequential <- NULL
  message("Sequential dataset not found")
}

# 2. DATASET SUMMARY ----

dataset_summary <- data.frame(
  Dataset = c("first_method", "single_method", "sequential"),
  Rows = c(nrow(first_method), nrow(single_method), ifelse(is.null(sequential), NA, nrow(sequential))),
  Unique_Studies = c(n_distinct(first_method$ID), n_distinct(single_method$ID), 
                     ifelse(is.null(sequential), NA, n_distinct(sequential$ID))),
  Mean_Proportion = c(
    paste0(round(mean(first_method$proportion, na.rm = TRUE)*100, 2), "%"),
    paste0(round(mean(single_method$proportion, na.rm = TRUE)*100, 2), "%"),
    NA
  ),
  Range_Proportion = c(
    paste0(round(min(first_method$proportion, na.rm = TRUE)*100, 2), "% - ", 
           round(max(first_method$proportion, na.rm = TRUE)*100, 2), "%"),
    paste0(round(min(single_method$proportion, na.rm = TRUE)*100, 2), "% - ", 
           round(max(single_method$proportion, na.rm = TRUE)*100, 2), "%"),
    NA
  )
)

addWorksheet(wb, "Dataset_Summary")
writeData(wb, "Dataset_Summary", dataset_summary)

# 3. DESCRIPTIVE STATISTICS ----

descriptive_stats <- data.frame(
  Statistic = c("Sample Size Range", "Mean Sample Size", "Median Sample Size",
                "CR Rate Range", "Mean CR Rate", "Median CR Rate",
                "Number of Studies", "Years Covered"),
  Value = c(
    paste0(min(first_method$sample_size), " - ", max(first_method$sample_size)),
    round(mean(first_method$sample_size)),
    median(first_method$sample_size),
    paste0(round(min(first_method$proportion, na.rm = TRUE)*100, 2), "% - ", 
           round(max(first_method$proportion, na.rm = TRUE)*100, 2), "%"),
    paste0(round(mean(first_method$proportion, na.rm = TRUE)*100, 2), "%"),
    paste0(round(median(first_method$proportion, na.rm = TRUE)*100, 2), "%"),
    n_distinct(first_method$ID),
    paste0(min(first_method$year), " - ", max(first_method$year))
  )
)

addWorksheet(wb, "Descriptive_Stats")
writeData(wb, "Descriptive_Stats", descriptive_stats)

# 4. CLASS IMBALANCE ANALYSIS ----

# Check available categorical variables
cat_vars <- names(first_method)[sapply(first_method, is.factor) | 
                               sapply(first_method, is.character) |
                               names(first_method) %in% names(codebook)]

# Filter for likely categorical variables even if not factor/character
numeric_cat_vars <- names(first_method)[sapply(first_method, is.numeric) & 
                                       names(first_method) %in% names(codebook)]
cat_vars <- unique(c(cat_vars, numeric_cat_vars))

# Analyze distribution for key variables if they exist
analyze_variable <- function(data, var_name) {
  if(!var_name %in% names(data)) return(NULL)
  
  counts <- data %>%
    count(!!sym(var_name)) %>%
    drop_na() %>%
    mutate(
      percentage = n / sum(n) * 100,
      percentage_formatted = paste0(round(percentage, 2), "%")
    ) %>%
    arrange(desc(n))
  
  # Add human-readable labels if available in codebook
  if(!is.null(codebook) && var_name %in% names(codebook)) {
    counts <- counts %>%
      mutate(label = sapply(!!sym(var_name), get_readable_name, variable = var_name)) %>%
      select(!!sym(var_name), label, everything())
  }
  
  # Calculate Gini coefficient as measure of imbalance
  p <- counts$percentage / 100
  n <- length(p)
  gini <- ifelse(n > 1, 
                sum(abs(outer(p, p, "-"))) / (2 * n^2 * mean(p)),
                0)
  
  # Add Gini to the data
  attr(counts, "gini") <- gini
  
  return(counts)
}

# Try to analyze key variables, gracefully handling if they don't exist
key_vars <- c("cr_method", "sample_source", "sample_platform", "journal", 
              "sample_recruitment", "sample_level", "sample_incentive", 
              "sample_country", "design_method", "design_location")

for(var in key_vars) {
  result <- analyze_variable(first_method, var)
  if(!is.null(result)) {
    sheet_name <- paste0("Distribution_", var)
    if(nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31) # Excel worksheet name length limit
    
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, result)
    
    # Add Gini coefficient as a note
    writeData(wb, sheet_name, data.frame(
      Note = paste0("Gini coefficient (measure of imbalance): ", round(attr(result, "gini"), 3),
                   " (0 = perfect balance, 1 = perfect imbalance)")
    ), startRow = nrow(result) + 3)
  }
}

# 5. ASSOCIATION ANALYSIS ----

# Function to prepare data for cross-tabulation with readable labels
prepare_crosstab_data <- function(data, var1, var2) {
  if(!all(c(var1, var2) %in% names(data))) return(NULL)
  
  # Create a temporary data frame with the selected variables
  temp_df <- data %>% select(!!sym(var1), !!sym(var2)) %>% drop_na()
  
  # Add readable labels if available
  if(!is.null(codebook) && var1 %in% names(codebook)) {
    temp_df[[paste0(var1, "_label")]] <- sapply(temp_df[[var1]], get_readable_name, variable = var1)
    var1_use <- paste0(var1, "_label")
  } else {
    var1_use <- var1
  }
  
  if(!is.null(codebook) && var2 %in% names(codebook)) {
    temp_df[[paste0(var2, "_label")]] <- sapply(temp_df[[var2]], get_readable_name, variable = var2)
    var2_use <- paste0(var2, "_label")
  } else {
    var2_use <- var2
  }
  
  return(list(data = temp_df, var1 = var1_use, var2 = var2_use))
}

# Function to analyze associations between variables with readable labels
analyze_association <- function(data, var1, var2) {
  temp_data <- prepare_crosstab_data(data, var1, var2)
  if(is.null(temp_data)) return(NULL)
  
  # Get the data and variable names to use
  df <- temp_data$data
  var1_use <- temp_data$var1
  var2_use <- temp_data$var2
  
  # Create cross-tabulation
  cross_table <- table(df[[var1_use]], df[[var2_use]])
  
  # Chi-square test
  chi_test <- try(suppressWarnings(chisq.test(cross_table)), silent = TRUE)
  
  # Convert to data frame for Excel
  cross_df <- as.data.frame.matrix(cross_table)
  
  # Add test results
  if(!inherits(chi_test, "try-error")) {
    attr(cross_df, "chi_p") <- chi_test$p.value
    
    # Calculate Cramer's V
    n <- sum(cross_table)
    attr(cross_df, "cramers_v") <- sqrt(chi_test$statistic / 
                                      (n * min(dim(cross_table) - 1)))
  }
  
  return(cross_df)
}

# Analyze key associations
key_pairs <- list(
  c("cr_method", "sample_source"),
  c("cr_method", "sample_platform"),
  c("cr_method", "journal"),
  c("sample_source", "sample_platform")
)

for(pair in key_pairs) {
  if(length(pair) == 2) {
    result <- analyze_association(first_method, pair[1], pair[2])
    if(!is.null(result)) {
      sheet_name <- paste0("Association_", pair[1], "_", pair[2])
      if(nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31) # Excel limit
      
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, result)
      
      # Add test statistics as notes
      if(!is.null(attr(result, "chi_p"))) {
        writeData(wb, sheet_name, data.frame(
          Statistic = c("Chi-square p-value", "Cramer's V"),
          Value = c(format.pval(attr(result, "chi_p"), digits = 3),
                   round(attr(result, "cramers_v"), 3))
        ), startRow = nrow(result) + 3)
      }
    }
  }
}

# 6. SEQUENTIAL SCREENING ANALYSIS ----

if(!is.null(sequential)) {
  # Position distribution
  position_counts <- sequential %>%
    count(method_position) %>%
    mutate(percentage = n / sum(n) * 100)
  
  addWorksheet(wb, "Position_Distribution")
  writeData(wb, "Position_Distribution", position_counts)
  
  # Method by position - find available method variable
  method_vars <- intersect(c("method_type", "cr_method", "method_code"), names(sequential))
  
  if(length(method_vars) > 0) {
    method_var <- method_vars[1]  # Use the first available method variable
    
    # Create position counts by method
    method_position_data <- sequential %>%
      count(!!sym(method_var), method_position)
    
    # Add human-readable method labels if available
    if(!is.null(codebook) && method_var %in% names(codebook)) {
      method_position_data <- method_position_data %>%
        mutate(method_label = sapply(!!sym(method_var), get_readable_name, 
                                    variable = method_var))
    }
    
    # Create a wide format table - carefully avoiding reference to columns that might not exist
    if("method_label" %in% names(method_position_data)) {
      # Use the human-readable label column
      method_position_wide <- method_position_data %>%
        select(method_label, method_position, n) %>%
        pivot_wider(
          id_cols = "method_label",
          names_from = method_position, 
          values_from = n, 
          names_prefix = "Position_",
          values_fill = 0
        )
    } else {
      # Use the original method variable
      method_position_wide <- method_position_data %>%
        pivot_wider(
          id_cols = method_var,
          names_from = method_position, 
          values_from = n, 
          names_prefix = "Position_",
          values_fill = 0
        )
    }
    
    addWorksheet(wb, "Method_by_Position")
    writeData(wb, "Method_by_Position", method_position_wide)
  }
  
  # Raw vs adjusted proportions
  if(all(c("raw_proportion", "adjusted_proportion") %in% names(sequential))) {
    prop_by_position <- sequential %>%
      group_by(method_position) %>%
      summarize(
        mean_raw = mean(raw_proportion, na.rm = TRUE) * 100,
        mean_adjusted = mean(adjusted_proportion, na.rm = TRUE) * 100,
        difference = mean_adjusted - mean_raw,
        n = n(),
        .groups = "drop"
      )
    
    addWorksheet(wb, "Position_Proportions")
    writeData(wb, "Position_Proportions", prop_by_position)
  }
}

# 7. TEMPORAL ANALYSIS ----

# CR rates by year
year_trends <- first_method %>%
  group_by(year) %>%
  summarize(
    mean_proportion = mean(proportion, na.rm = TRUE) * 100,
    median_proportion = median(proportion, na.rm = TRUE) * 100,
    min_proportion = min(proportion, na.rm = TRUE) * 100,
    max_proportion = max(proportion, na.rm = TRUE) * 100,
    studies = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

addWorksheet(wb, "Temporal_Trends")
writeData(wb, "Temporal_Trends", year_trends)

# 8. MULTILEVEL STRUCTURE ASSESSMENT ----

# Studies per method category
method_vars <- Filter(function(x) x %in% names(first_method), 
                      c("cr_method", "method_type"))

if(length(method_vars) > 0) {
  method_var <- method_vars[1]  # Use the first available method variable
  
  method_counts <- first_method %>%
    group_by(!!sym(method_var)) %>%
    summarize(
      studies = n_distinct(ID),
      mean_proportion = mean(proportion, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(studies))
  
  # Add human-readable labels if available
  if(!is.null(codebook) && method_var %in% names(codebook)) {
    method_counts <- method_counts %>%
      mutate(method_label = sapply(!!sym(method_var), get_readable_name, 
                                  variable = method_var)) %>%
      select(!!sym(method_var), method_label, everything())
  }
  
  addWorksheet(wb, "Method_Counts")
  writeData(wb, "Method_Counts", method_counts)
}

# Studies per sample source
if("sample_source" %in% names(first_method)) {
  sample_counts <- first_method %>%
    group_by(sample_source) %>%
    summarize(
      studies = n_distinct(ID),
      mean_proportion = mean(proportion, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(studies))
  
  # Add human-readable labels if available
  if(!is.null(codebook) && "sample_source" %in% names(codebook)) {
    sample_counts <- sample_counts %>%
      mutate(source_label = sapply(sample_source, get_readable_name, 
                                  variable = "sample_source")) %>%
      select(sample_source, source_label, everything())
  }
  
  addWorksheet(wb, "Sample_Counts")
  writeData(wb, "Sample_Counts", sample_counts)
}

# Multilevel structure summary
structure_info <- data.frame(
  Element = c(
    "Total Studies",
    "Method Categories",
    "Sample Categories",
    "Typical Studies per Method",
    "Range of Studies per Method",
    "Sequential Studies",
    "Maximum Position in Sequential"
  ),
  Value = c(
    n_distinct(first_method$ID),
    if(exists("method_counts")) n_distinct(method_counts[[method_var]]) else "Unknown",
    if("sample_source" %in% names(first_method)) n_distinct(first_method$sample_source) else "Unknown",
    if(exists("method_counts")) round(mean(method_counts$studies), 1) else "Unknown",
    if(exists("method_counts")) paste0(min(method_counts$studies), " - ", max(method_counts$studies)) else "Unknown",
    if(!is.null(sequential)) n_distinct(sequential$ID) else "N/A",
    if(!is.null(sequential)) max(sequential$method_position) else "N/A"
  )
)

addWorksheet(wb, "Multilevel_Structure")
writeData(wb, "Multilevel_Structure", structure_info)

# 9. DATA QUALITY ASSESSMENT ----

# Missing values
missing_values <- data.frame(
  Variable = names(first_method),
  Missing_Count = sapply(first_method, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(first_method, function(x) sum(is.na(x)) / length(x) * 100)
) %>%
  arrange(desc(Missing_Count))

addWorksheet(wb, "Missing_Values")
writeData(wb, "Missing_Values", missing_values)

# Extreme values
proportion_outliers <- first_method %>%
  filter(proportion > quantile(proportion, 0.75, na.rm = TRUE) + 
         1.5 * IQR(proportion, na.rm = TRUE)) %>%
  arrange(desc(proportion))

if(nrow(proportion_outliers) > 0) {
  # Get subset with key columns
  outlier_summary <- proportion_outliers %>%
    select(ID, proportion, year, sample_size, cr_method)
  
  # Add human-readable labels if available
  if(!is.null(codebook) && "cr_method" %in% names(codebook)) {
    outlier_summary$method_label <- sapply(outlier_summary$cr_method, 
                                          get_readable_name, variable = "cr_method")
  }
  
  addWorksheet(wb, "Proportion_Outliers")
  writeData(wb, "Proportion_Outliers", outlier_summary)
}

# 10. SAVE RESULTS ----

# Save Excel workbook
saveWorkbook(wb, "output/data_examination/data_examination.xlsx", overwrite = TRUE)

cat("\nData examination complete. Results saved to output/data_examination/data_examination.xlsx\n")