###############################################################################
# 08_combined_pub_bias_sensitivity.R
# 
# This script performs publication bias assessment and sensitivity analyses
# for the meta-analysis of careless responding rates. It includes:
#
# PUBLICATION BIAS:
# 1. Egger's regression test for funnel plot asymmetry
#
# SENSITIVITY ANALYSES:
# 1. Influence diagnostics to identify potentially influential studies
# 2. Leave-one-out meta-analysis to assess estimate stability
# 3. Subgroup exclusion analyses to test robustness across contexts
# 4. Methodological sensitivity analyses with alternative estimators
# 5. Sample size threshold analyses to assess small-study influence
#
# The analyses are performed primarily on the First-Method approach data,
# with selected analyses repeated for the Single-Method and Overall approaches.
# Results are saved as tables and visualizations for inclusion in the dissertation.
###############################################################################

library(tidyverse)
library(metafor)
library(meta)
library(ggplot2)
library(gridExtra)
library(glue)
library(knitr)

# Create output directories if they don't exist
dir.create("output/r_results/pub_bias", recursive = TRUE, showWarnings = FALSE)
dir.create("output/r_results/sensitivity", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/pub_bias", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/sensitivity", recursive = TRUE, showWarnings = FALSE)

###############################################################################
# DATA PREPARATION
###############################################################################

# Function to load and prepare a dataset for meta-analysis
prepare_dataset <- function(approach) {
  cat(paste0("\nPreparing ", approach, " dataset for analysis...\n"))
  
  # Load appropriate dataset
  data <- read.csv(glue("data/for_r_meta/{approach}_data.csv"), 
                   check.names = FALSE, 
                   stringsAsFactors = FALSE)
  
  # Create the necessary escalc effect size object
  if("proportion" %in% colnames(data)) {
    # Calculate number of events based on proportion and sample size
    events <- round(data$proportion * data$sample_size)
    n <- data$sample_size
    
    # Apply continuity correction for studies with zero events
    has_zeros <- sum(events == 0)
    if (has_zeros > 0) {
      needs_correction <- (events == 0)
      events[needs_correction] <- events[needs_correction] + 0.5
      n[needs_correction] <- n[needs_correction] + 1
      cat("  Applied continuity correction to", sum(needs_correction), "studies with zero events\n")
    }
    
    # Calculate logit-transformed proportions using escalc
    es <- escalc(measure = "PLO", xi = events, ni = n)
    data$yi <- es$yi  # Logit-transformed proportion
    data$vi <- es$vi  # Variance of the logit-transformed proportion
    
    # Run random-effects meta-analysis
    meta_result <- rma(yi = yi, vi = vi, data = data, method = "DL")
    
    cat("  Processed", nrow(data), "studies with", sum(data$sample_size), "total participants\n")
    
    return(list(data = data, meta_result = meta_result, events = events, n = n))
  } else {
    cat("  Error: proportion column not found in dataset\n")
    return(NULL)
  }
}

# Prepare datasets for all three approaches
first_method <- prepare_dataset("first_method")
single_method <- prepare_dataset("single_method")
overall_method <- prepare_dataset("overall")

###############################################################################
# PUBLICATION BIAS ANALYSIS
###############################################################################

cat("\n=================================================\n")
cat("PUBLICATION BIAS ANALYSIS\n")
cat("=================================================\n")

# Function to run publication bias analyses and create visualizations
run_pub_bias_analysis <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  cat(paste0("\nAnalyzing publication bias for ", approach_name, " approach:\n"))
  
  meta_result <- data_obj$meta_result
  data <- data_obj$data
  
  # 1. Egger's regression test for funnel plot asymmetry
  eggers_test <- regtest(meta_result)
  cat("  Egger's test results:\n")
  cat("    z =", round(eggers_test$zval, 2), ", p =", round(eggers_test$pval, 3), "\n")
  
  # Calculate publication bias results for reporting
  results <- data.frame(
    Approach = approach_name,
    Original_Est = transf.ilogit(meta_result$b),
    Original_CI_Lower = transf.ilogit(meta_result$ci.lb),
    Original_CI_Upper = transf.ilogit(meta_result$ci.ub),
    Egger_Z = eggers_test$zval,
    Egger_P = eggers_test$pval
  )
  
  # Create funnel plot
  png(glue("output/figures/pub_bias/contour_funnel_{approach_name}.png"), 
      width = 8, height = 7, units = "in", res = 300)
  
  # Build a contour-enhanced funnel plot
  funnel(meta_result, 
         level = c(0.1, 0.05, 0.01), 
         shade = c("darkgray", "gray", "lightgray"),
         refline = meta_result$b,
         legend = TRUE,
         ylab = "Standard Error",
         xlab = "Proportion of Careless Responses (logit scale)",
         main = glue("Contour-Enhanced Funnel Plot - {approach_name} Approach"))
  
  dev.off()
  
  return(results)
}

# Run publication bias analyses for all three approaches
pub_bias_results <- list(
  First_Method = run_pub_bias_analysis(first_method, "First-Method"),
  Single_Method = run_pub_bias_analysis(single_method, "Single-Method"),
  Overall = run_pub_bias_analysis(overall_method, "Overall")
)

# Combine results into a single dataframe
pub_bias_summary <- bind_rows(pub_bias_results)

# Format for reporting
pub_bias_table <- pub_bias_summary %>%
  mutate(
    Original_Est_CI = glue("{round(Original_Est*100, 2)}% [{round(Original_CI_Lower*100, 2)}-{round(Original_CI_Upper*100, 2)}%]"),
    Egger_Result = glue("z = {round(Egger_Z, 2)}, p = {round(Egger_P, 3)}")
  ) %>%
  select(Approach, Original_Est_CI, Egger_Result)

# Save publication bias results
write.csv(pub_bias_summary, "output/r_results/pub_bias/full_results.csv", row.names = FALSE)
write.csv(pub_bias_table, "output/r_results/pub_bias/summary_table.csv", row.names = FALSE)

###############################################################################
# SENSITIVITY ANALYSES
###############################################################################

cat("\n=================================================\n")
cat("SENSITIVITY ANALYSES\n")
cat("=================================================\n")

# 1. Influence Diagnostics
cat("\nRunning influence diagnostics...\n")

run_influence_analysis <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  meta_result <- data_obj$meta_result
  data <- data_obj$data
  
  cat(paste0("  Analyzing influence for ", approach_name, " approach\n"))
  
  # Try to conduct influence analysis with robust error handling
  tryCatch({
    # Use influence.rma safely with explicit specification
    infl <- influence(meta_result, digits = 4)
    
    # Check if we got valid results
    if(is.null(infl) || !is.list(infl) || length(infl) == 0 || 
       is.null(infl$rstudent) || length(infl$rstudent) != nrow(data)) {
      cat("    Warning: Could not compute influence diagnostics\n")
      return(NULL)
    }
    
    # Create dataframe with influence metrics
    infl_df <- data.frame(
      ID = data$ID,
      rstudent = infl$rstudent,
      dffits = infl$dffits,
      cook_d = infl$cook.d,
      cov_ratio = infl$cov.r,
      proportion = transf.ilogit(data$yi),
      sample_size = data$sample_size
    )
    
    # Calculate absolute values for sorting
    infl_df <- infl_df %>%
      mutate(
        abs_rstudent = abs(rstudent),
        abs_dffits = abs(dffits)
      ) %>%
      arrange(desc(abs_rstudent))
    
    # Identify potentially influential studies (absolute studentized residual > 2)
    influential <- infl_df %>% 
      filter(abs_rstudent > 2) %>%
      select(ID, rstudent, dffits, cook_d, proportion, sample_size) %>%
      arrange(desc(abs(rstudent)))
    
    cat("    Identified", nrow(influential), "potentially influential studies\n")
    
    # Save full influence data
    write.csv(infl_df, glue("output/r_results/sensitivity/influence_{approach_name}.csv"), 
              row.names = FALSE)
    
    # Save influential studies
    if(nrow(influential) > 0) {
      write.csv(influential, 
                glue("output/r_results/sensitivity/influential_studies_{approach_name}.csv"), 
                row.names = FALSE)
    }
    
    # Create alternative influence plot using baujat plot which is more robust
    png(glue("output/figures/sensitivity/baujat_plot_{approach_name}.png"), 
        width = 10, height = 8, units = "in", res = 300)
    
    baujat(meta_result, 
           main = glue("Baujat Plot - {approach_name} Approach"),
           xlab = "Contribution to Overall Heterogeneity",
           ylab = "Influence on Overall Result")
    
    dev.off()
    
    # Return top influential studies
    top_infl <- head(infl_df %>% arrange(desc(abs_rstudent)), 10)
    return(list(influential = influential, top10 = top_infl))
    
  }, error = function(e) {
    cat("    Error in influence analysis:", e$message, "\n")
    
    # Try an alternative approach using leave-one-out
    cat("    Attempting alternative influence assessment via leave-one-out...\n")
    tryCatch({
      loo <- leave1out(meta_result)
      
      # Calculate the difference between each leave-one-out estimate and the overall estimate
      loo_df <- data.frame(
        loo,
        ID = data$ID,
        removed_proportion = transf.ilogit(data$yi),
        effect_on_estimate = meta_result$b - loo$estimate
      ) %>%
        mutate(
          abs_effect = abs(effect_on_estimate),
          effect_perc = abs(effect_on_estimate / meta_result$b) * 100
        ) %>%
        arrange(desc(abs_effect))
      
      # Identify top influential studies based on leave-one-out
      influential <- loo_df %>%
        filter(effect_perc > 5) %>%
        select(ID, estimate, ci.lb, ci.ub, effect_on_estimate, effect_perc, removed_proportion)
      
      write.csv(loo_df, glue("output/r_results/sensitivity/loo_influence_{approach_name}.csv"), 
                row.names = FALSE)
      
      if(nrow(influential) > 0) {
        write.csv(influential, 
                  glue("output/r_results/sensitivity/alternative_influential_{approach_name}.csv"), 
                  row.names = FALSE)
      }
      
      # Create simple plot of study effects on estimate
      png(glue("output/figures/sensitivity/influence_effect_{approach_name}.png"), 
          width = 10, height = 8, units = "in", res = 300)
      
      # Get top 15 most influential studies
      top_influential <- head(loo_df %>% arrange(desc(abs_effect)), 15)
      
      # Plot effect on estimate
      par(mar = c(5, 10, 4, 2) + 0.1)
      barplot(top_influential$effect_on_estimate, 
              names.arg = top_influential$ID, 
              horiz = TRUE,
              las = 1,  
              col = ifelse(top_influential$effect_on_estimate > 0, "darkred", "darkblue"),
              main = glue("Effect on Estimate When Study is Removed - {approach_name} Approach"),
              xlab = "Change in Pooled Estimate (logit scale)")
      abline(v = 0, lty = 2)
      
      dev.off()
      
      return(list(alternative = TRUE, influential = influential, top10 = head(loo_df, 10)))
      
    }, error = function(e2) {
      cat("    Error in alternative influence analysis:", e2$message, "\n")
      return(NULL)
    })
  })
}

influence_results <- list()
for (approach in c("First-Method", "Single-Method", "Overall")) {
  data_obj <- switch(approach,
                   "First-Method" = first_method,
                   "Single-Method" = single_method,
                   "Overall" = overall_method)
  
  influence_results[[tolower(gsub("-", "_", approach))]] <- 
    run_influence_analysis(data_obj, approach)
}

# 2. Leave-One-Out Meta-Analysis
cat("\nRunning leave-one-out meta-analysis...\n")
# Leave-One-Out Analysis improvements 
# (addressing the recycling array warnings)
run_leave_one_out <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  meta_result <- data_obj$meta_result
  data <- data_obj$data
  
  cat(paste0("  Running leave-one-out analysis for ", approach_name, " approach\n"))
  
  # Perform leave-one-out meta-analysis
  loo <- leave1out(meta_result)
  
  # Create a vector of the same length as loo$estimate containing the overall estimate
  overall_b_vector <- rep(meta_result$b, length(loo$estimate))
  
  # Calculate the difference between each leave-one-out estimate and the overall estimate
  loo_df <- data.frame(
    loo,
    ID = data$ID,
    removed_proportion = transf.ilogit(data$yi),
    effect_on_estimate = overall_b_vector - loo$estimate
  )
  
  # Find studies with largest impact on the pooled estimate
  loo_df <- loo_df %>%
    mutate(
      abs_effect = abs(effect_on_estimate),
      # Use the correct vector division
      effect_perc = abs(effect_on_estimate / as.vector(overall_b_vector)) * 100
    ) %>%
    arrange(desc(abs_effect))
  
  # Identify studies with substantial impact (>5% change in estimate)
  substantial_impact <- loo_df %>%
    filter(effect_perc > 5) %>%
    select(ID, estimate, ci.lb, ci.ub, effect_on_estimate, effect_perc, removed_proportion)
  
  cat("    Identified", nrow(substantial_impact), "studies with >5% impact on pooled estimate\n")
  
  # Save full leave-one-out results
  write.csv(loo_df, glue("output/r_results/sensitivity/loo_{approach_name}.csv"), row.names = FALSE)
  
  # Save substantial impact studies
  if(nrow(substantial_impact) > 0) {
    write.csv(substantial_impact, 
              glue("output/r_results/sensitivity/substantial_impact_{approach_name}.csv"), 
              row.names = FALSE)
  }
  
  # Create leave-one-out plot for top influential studies
  top_influential <- head(loo_df %>% arrange(desc(abs_effect)), min(15, nrow(loo_df)))
  
  png(glue("output/figures/sensitivity/loo_plot_{approach_name}.png"), 
      width = 10, height = 8, units = "in", res = 300)
  
  par(mar = c(5, 10, 4, 2) + 0.1)
  
  forest(top_influential$estimate, 
         ci.lb = top_influential$ci.lb, 
         ci.ub = top_influential$ci.ub,
         slab = top_influential$ID,
         refline = meta_result$b,
         xlab = "Pooled Proportion (logit scale) when Study is Omitted",
         main = glue("Leave-One-Out Analysis - {approach_name} Approach\n(Most Influential Studies)"))
  
  dev.off()
  
  return(list(
    substantial_impact = substantial_impact,
    top = top_influential
  ))
}

# Run with better error handling
loo_results <- list()
for (approach in c("First-Method", "Single-Method", "Overall")) {
  data_obj <- switch(approach,
                   "First-Method" = first_method,
                   "Single-Method" = single_method,
                   "Overall" = overall_method)
  
  loo_results[[tolower(gsub("-", "_", approach))]] <- 
    tryCatch({
      run_leave_one_out(data_obj, approach)
    }, error = function(e) {
      cat("  Error in leave-one-out analysis for", approach, ":", e$message, "\n")
      return(NULL)
    })
}

# 3. Subgroup Exclusion Analysis
cat("\nRunning subgroup exclusion analysis...\n")

run_subgroup_exclusion <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  data <- data_obj$data
  base_estimate <- transf.ilogit(data_obj$meta_result$b)
  
  cat(paste0("  Running subgroup exclusion for ", approach_name, " approach\n"))
  
  # Define key subgroup variables to test
  subgroup_vars <- c()
  
  # Only include method-related variables for First-Method and Single-Method approaches
  if(approach_name %in% c("First-Method", "Single-Method")) {
    subgroup_vars <- c(subgroup_vars, "method_type", "cr_method_name", "cr_method")
  }
  
  # Common subgroup variables for all approaches
  common_vars <- c("sample_source_name", "sample_platform_name", "journal_name", "sample_recruitment_name")
  subgroup_vars <- c(subgroup_vars, common_vars)
  
  # Filter to variables actually in the dataset
  subgroup_vars <- subgroup_vars[subgroup_vars %in% colnames(data)]
  
  if(length(subgroup_vars) == 0) {
    cat("    No suitable subgroup variables found\n")
    return(NULL)
  }
  
  # Process each subgroup variable separately and combine results at the end
  all_exclusions <- list()
  
  for(var in subgroup_vars) {
    cat("    Processing subgroup:", var, "\n")
    # Get unique values for the subgroup variable
    unique_vals <- unique(data[[var]])
    unique_vals <- unique_vals[!is.na(unique_vals)]
    
    var_exclusions <- list()
    for(val in unique_vals) {
      # Skip if there would be fewer than 2 studies left
      subset <- data[data[[var]] != val, ]
      if(nrow(subset) < 2) next
      
      # Run meta-analysis excluding the subgroup
      result <- tryCatch({
        rma(yi = yi, vi = vi, data = subset, method = "DL")
      }, error = function(e) {
        cat("      Error analyzing subset without", var, "=", val, ":", e$message, "\n")
        return(NULL)
      })
      
      if(is.null(result)) next
      
      # Calculate the change from the base estimate
      new_estimate <- transf.ilogit(result$b)
      abs_change <- abs(new_estimate - base_estimate)
      perc_change <- abs_change / base_estimate * 100
      
      # Store results for this exclusion
      var_exclusions[[length(var_exclusions) + 1]] <- list(
        Variable = var,
        Excluded_Value = as.character(val),  # Convert to character to avoid type conflicts
        Excluded_Label = as.character(val),  # Use the value itself as label
        Studies_Remaining = nrow(subset),
        New_Estimate = new_estimate,
        CI_Lower = transf.ilogit(result$ci.lb),
        CI_Upper = transf.ilogit(result$ci.ub),
        Abs_Change = abs_change,
        Perc_Change = perc_change
      )
    }
    
    # Convert to data frame if we have results
    if(length(var_exclusions) > 0) {
      var_df <- do.call(rbind, lapply(var_exclusions, as.data.frame))
      all_exclusions[[var]] <- var_df
    }
  }
  
  # Combine all results if we have any
  if(length(all_exclusions) > 0) {
    exclusion_results <- do.call(rbind, all_exclusions)
    
    # Sort by percent change
    exclusion_results <- exclusion_results %>%
      arrange(desc(Perc_Change))
    
    cat("    Completed", nrow(exclusion_results), "subgroup exclusion analyses\n")
    
    # Save results
    write.csv(exclusion_results, 
              glue("output/r_results/sensitivity/subgroup_exclusion_{approach_name}.csv"), 
              row.names = FALSE)
    
    # Create summary visualization of top subgroup exclusions (if we have any)
    if(nrow(exclusion_results) > 0) {
      top_exclusions <- head(exclusion_results, min(10, nrow(exclusion_results)))
      
      png(glue("output/figures/sensitivity/subgroup_exclusion_{approach_name}.png"), 
          width = 10, height = 8, units = "in", res = 300)
      
      par(mar = c(5, 12, 4, 2) + 0.1)
      
      # Generate forest plot of subgroup exclusions
      forest(top_exclusions$New_Estimate, 
             ci.lb = top_exclusions$CI_Lower, 
             ci.ub = top_exclusions$CI_Upper,
             slab = top_exclusions$Excluded_Label,
             refline = base_estimate,
             xlab = "Pooled Proportion when Subgroup is Excluded",
             main = glue("Subgroup Exclusion Analysis - {approach_name} Approach\n(Most Influential Exclusions)"))
      
      dev.off()
    }
    
    return(list(
      all_exclusions = exclusion_results,
      top = if(nrow(exclusion_results) > 0) head(exclusion_results, 10) else NULL
    ))
  } else {
    cat("    No valid subgroup exclusions could be performed\n")
    return(NULL)
  }
}

# Run for each approach with better error handling
subgroup_exclusion_results <- list()
for (approach in c("First-Method", "Single-Method", "Overall")) {
  data_obj <- switch(approach,
                   "First-Method" = first_method,
                   "Single-Method" = single_method,
                   "Overall" = overall_method)
  
  subgroup_exclusion_results[[tolower(gsub("-", "_", approach))]] <- 
    tryCatch({
      run_subgroup_exclusion(data_obj, approach)
    }, error = function(e) {
      cat("  Error in subgroup exclusion for", approach, ":", e$message, "\n")
      return(NULL)
    })
}

# 4. Methodological Sensitivity Analysis (Alternative estimators)
cat("\nRunning methodological sensitivity analysis...\n")

run_methodological_sensitivity <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  meta_result <- data_obj$meta_result
  data <- data_obj$data
  
  cat(paste0("  Testing alternative estimators for ", approach_name, " approach\n"))
  
  # Define alternative methods to test
  estimators <- c("DL", "REML", "ML", "EB", "SJ", "HE", "HS", "PM")
  
  # Run meta-analysis with each estimator
  method_results <- map_dfr(estimators, function(method) {
    # Skip if estimator is not available
    result <- tryCatch({
      rma(yi = yi, vi = vi, data = data, method = method)
    }, error = function(e) {
      cat("    Error with estimator", method, ":", e$message, "\n")
      return(NULL)
    })
    
    if(is.null(result)) return(NULL)
    
    tibble(
      Estimator = method,
      Estimate = transf.ilogit(result$b),
      CI_Lower = transf.ilogit(result$ci.lb),
      CI_Upper = transf.ilogit(result$ci.ub),
      tau2 = result$tau2,
      I2 = result$I2,
      H2 = result$H2,
      Q = result$QE,
      p_Q = result$QEp
    )
  })
  
  cat("    Compared", nrow(method_results), "different estimators\n")
  
  # Save method results
  write.csv(method_results, 
            glue("output/r_results/sensitivity/estimator_sensitivity_{approach_name}.csv"), 
            row.names = FALSE)
  
  # Create visualization comparing estimators
  png(glue("output/figures/sensitivity/estimator_comparison_{approach_name}.png"), 
      width = 9, height = 7, units = "in", res = 300)
  
  ggplot(method_results, aes(x = reorder(Estimator, Estimate))) +
    geom_point(aes(y = Estimate), size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    labs(
      title = glue("Sensitivity to Method Selection - {approach_name} Approach"),
      x = "Between-Study Variance Estimator",
      y = "Pooled Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    coord_flip() +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  dev.off()
  
  return(method_results)
}

estimator_results <- list(
  first = run_methodological_sensitivity(first_method, "First-Method"),
  single = run_methodological_sensitivity(single_method, "Single-Method"),
  overall = run_methodological_sensitivity(overall_method, "Overall")
)

# 5. Sample Size Threshold Analysis
cat("\nRunning sample size threshold analysis...\n")

run_sample_size_threshold <- function(data_obj, approach_name) {
  if(is.null(data_obj)) return(NULL)
  
  meta_result <- data_obj$meta_result
  data <- data_obj$data
  
  cat(paste0("  Testing sample size thresholds for ", approach_name, " approach\n"))
  
  # Define thresholds to test (minimum sample size to include)
  n_quantiles <- quantile(data$sample_size, probs = seq(0, 0.75, 0.25))
  n_thresholds <- c(50, 100, 200, 500, 1000)
  n_thresholds <- c(1, n_thresholds, n_quantiles)
  n_thresholds <- sort(unique(round(n_thresholds)))
  
  # For each threshold, run meta-analysis including only studies with sample size >= threshold
  threshold_results <- map_dfr(n_thresholds, function(threshold) {
    subset <- data[data$sample_size >= threshold, ]
    if(nrow(subset) < 2) return(NULL)
    
    result <- rma(yi = yi, vi = vi, data = subset, method = "DL")
    
    tibble(
      Threshold = threshold,
      Studies_Included = result$k,
      Proportion_of_Studies = result$k / meta_result$k,
      Participants_Included = sum(subset$sample_size),
      Proportion_of_Participants = sum(subset$sample_size) / sum(data$sample_size),
      Estimate = transf.ilogit(result$b),
      CI_Lower = transf.ilogit(result$ci.lb),
      CI_Upper = transf.ilogit(result$ci.ub),
      I2 = result$I2,
      tau2 = result$tau2
    )
  })
  
  cat("    Tested", nrow(threshold_results), "sample size thresholds\n")
  
  # Save threshold results
  write.csv(threshold_results, 
            glue("output/r_results/sensitivity/sample_size_threshold_{approach_name}.csv"), 
            row.names = FALSE)
  
  # Create visualization of threshold impact
  png(glue("output/figures/sensitivity/sample_size_threshold_{approach_name}.png"), 
      width = 10, height = 8, units = "in", res = 300)
  
  par(mfrow = c(2, 1), mar = c(5, 4, 4, 4) + 0.1)
  
  # Plot 1: Effect on pooled estimate
  plot(threshold_results$Threshold, threshold_results$Estimate,
       type = "b", pch = 19, xlab = "Minimum Sample Size",
       ylab = "Pooled Proportion", main = "Effect of Sample Size Threshold on Pooled Estimate")
  
  # Add confidence intervals
  segments(
    x0 = threshold_results$Threshold,
    y0 = threshold_results$CI_Lower,
    x1 = threshold_results$Threshold,
    y1 = threshold_results$CI_Upper
  )
  
  # Add reference line for overall estimate
  abline(h = transf.ilogit(meta_result$b), lty = 2, col = "red")
  
  # Plot 2: Effect on number of studies and I²
  par(mar = c(5, 4, 4, 4) + 0.1)
  plot(threshold_results$Threshold, threshold_results$Studies_Included,
       type = "b", pch = 19, xlab = "Minimum Sample Size",
       ylab = "Number of Studies Included", main = "Effect on Study Inclusion and Heterogeneity")
  
  # Add I² on second axis
  par(new = TRUE)
  plot(threshold_results$Threshold, threshold_results$I2,
       type = "b", pch = 17, col = "blue",
       axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, col = "blue", col.axis = "blue")
  mtext("I² (%)", side = 4, line = 2, col = "blue")
  
# Add legend
  legend("topright", 
         legend = c("Number of Studies", "I² (%)"),
         pch = c(19, 17),
         col = c("black", "blue"),
         lty = c(1, 1))
  
  dev.off()
  
  return(threshold_results)
}

sample_size_results <- list(
  first = run_sample_size_threshold(first_method, "First-Method"),
  single = run_sample_size_threshold(single_method, "Single-Method"),
  overall = run_sample_size_threshold(overall_method, "Overall")
)

###############################################################################
# SUMMARY OF FINDINGS WITH EXCEL EXPORT
###############################################################################

cat("\n=================================================\n")
cat("GENERATING SUMMARY REPORTS IN EXCEL FORMAT\n")
cat("=================================================\n")

# Function to create Excel workbook with results
create_excel_summary <- function() {
  # Check if the openxlsx package is installed, and install if needed
  if (!require("openxlsx", quietly = TRUE)) {
    cat("Installing openxlsx package for Excel export...\n")
    install.packages("openxlsx", repos = "https://cran.r-project.org")
    library(openxlsx)
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # 1. Publication Bias Sheet
  addWorksheet(wb, "Publication Bias")
  
  if(!is.null(pub_bias_summary) && nrow(pub_bias_summary) > 0) {
    # Format the data nicely
    pub_bias_table <- pub_bias_summary %>%
      mutate(
        Original_Estimate = paste0(round(Original_Est*100, 2), "%"),
        CI_Range = paste0("[", round(Original_CI_Lower*100, 2), "-", 
                        round(Original_CI_Upper*100, 2), "%]"),
        Egger_Z = round(Egger_Z, 2),
        Egger_P = round(Egger_P, 3)
      ) %>%
      select(Approach, Original_Estimate, CI_Range, Egger_Z, Egger_P) %>%
      rename(
        "Approach" = Approach,
        "Pooled Proportion" = Original_Estimate,
        "95% Confidence Interval" = CI_Range,
        "Egger's Test Z" = Egger_Z,
        "Egger's Test p-value" = Egger_P
      )
    
    writeData(wb, "Publication Bias", pub_bias_table, startRow = 1)
    
    # Add title and explanation
    writeData(wb, "Publication Bias", "Publication Bias Assessment", 
              startRow = nrow(pub_bias_table) + 3)
    writeData(wb, "Publication Bias", paste0(
      "Egger's test examines funnel plot asymmetry, where significant p-values ",
      "suggest potential publication bias or small-study effects."
    ), startRow = nrow(pub_bias_table) + 4)
    
    # Add some formatting
    addStyle(wb, "Publication Bias", createStyle(textDecoration = "bold"), rows = 1, cols = 1:ncol(pub_bias_table))
  } else {
    writeData(wb, "Publication Bias", "No publication bias analysis results available")
  }
  
  # 2. Leave-One-Out Analysis
  addWorksheet(wb, "Leave-One-Out Analysis")
  
  # Create a combined dataset of all leave-one-out results
  loo_combined <- data.frame()
  
  for(approach in c("First-Method", "Single-Method", "Overall")) {
    result_list <- switch(approach,
                       "First-Method" = loo_results$first_method,
                       "Single-Method" = loo_results$single_method,
                       "Overall" = loo_results$overall)
    
    if(!is.null(result_list) && !is.null(result_list$top) && is.data.frame(result_list$top)) {
      result_df <- result_list$top %>%
        mutate(Approach = approach) %>%
        select(any_of(c("Approach", "ID", "estimate", "ci.lb", "ci.ub", "effect_on_estimate", "abs_effect")))
      
      loo_combined <- bind_rows(loo_combined, result_df)
    }
  }
  
  if(nrow(loo_combined) > 0) {
    # Format for readability
    loo_combined <- loo_combined %>%
      mutate(
        Estimate = paste0(round(transf.ilogit(estimate)*100, 2), "%"),
        CI_Range = paste0("[", round(transf.ilogit(ci.lb)*100, 2), "-", 
                      round(transf.ilogit(ci.ub)*100, 2), "%]"),
        Effect_Value = round(effect_on_estimate, 4)
      ) %>%
      select(Approach, ID, Estimate, CI_Range, Effect_Value) %>%
      rename(
        "Approach" = Approach,
        "Study ID" = ID,
        "Estimate When Omitted" = Estimate,
        "95% CI When Omitted" = CI_Range,
        "Effect on Estimate" = Effect_Value
      ) %>%
      arrange(Approach, desc(abs(`Effect on Estimate`)))
    
    writeData(wb, "Leave-One-Out Analysis", loo_combined)
    addStyle(wb, "Leave-One-Out Analysis", createStyle(textDecoration = "bold"), 
             rows = 1, cols = 1:ncol(loo_combined))
  } else {
    writeData(wb, "Leave-One-Out Analysis", "No leave-one-out results found with substantial impact")
  }
  
  # 3. Subgroup Exclusion Analysis
addWorksheet(wb, "Subgroup Exclusion")

subgroup_combined <- data.frame()

for(approach in c("First-Method", "Single-Method", "Overall")) {
  result_list <- switch(approach,
                       "First-Method" = subgroup_exclusion_results$first_method,
                       "Single-Method" = subgroup_exclusion_results$single_method,
                       "Overall" = subgroup_exclusion_results$overall)
  
  if(!is.null(result_list) && !is.null(result_list$all_exclusions) && 
     is.data.frame(result_list$all_exclusions) && nrow(result_list$all_exclusions) > 0) {
    
    result_df <- result_list$all_exclusions %>%
      mutate(Approach = approach) %>%
      select(Approach, Variable, Excluded_Value, New_Estimate, CI_Lower, CI_Upper, 
             Abs_Change, Perc_Change, Studies_Remaining)
    
    subgroup_combined <- bind_rows(subgroup_combined, result_df)
  }
}

if(nrow(subgroup_combined) > 0) {
  # Format for readability
  subgroup_combined <- subgroup_combined %>%
    mutate(
      New_Estimate_Fmt = paste0(round(New_Estimate*100, 2), "%"),
      CI_Range = paste0("[", round(CI_Lower*100, 2), "-", round(CI_Upper*100, 2), "%]"),
      Change_Pct = paste0(round(Perc_Change, 1), "%")
    ) %>%
    select(Approach, Variable, Excluded_Value, New_Estimate_Fmt, CI_Range, 
           Change_Pct, Studies_Remaining) %>%
    rename(
      "Approach" = Approach,
      "Variable" = Variable,
      "Excluded Subgroup" = Excluded_Value,
      "Pooled Estimate" = New_Estimate_Fmt,
      "95% CI" = CI_Range,
      "% Change" = Change_Pct,
      "Remaining Studies" = Studies_Remaining
    )
  
  # Create a numeric column for sorting
  subgroup_combined$Change_Value <- as.numeric(gsub("%", "", subgroup_combined$`% Change`))
  
  # Sort by approach and absolute change
  subgroup_combined <- subgroup_combined %>%
    arrange(Approach, desc(abs(Change_Value))) %>%
    select(-Change_Value)  # Remove the temporary column after sorting
  
  writeData(wb, "Subgroup Exclusion", subgroup_combined)
  addStyle(wb, "Subgroup Exclusion", createStyle(textDecoration = "bold"), 
           rows = 1, cols = 1:ncol(subgroup_combined))
} else {
  writeData(wb, "Subgroup Exclusion", "No subgroup exclusion results available")
}
  
  # 4. Methodological Sensitivity (Alternative Estimators)
  addWorksheet(wb, "Alternative Estimators")
  
  for(approach in c("First-Method", "Single-Method", "Overall")) {
    result_df <- switch(approach,
                       "First-Method" = estimator_results$first,
                       "Single-Method" = estimator_results$single,
                       "Overall" = estimator_results$overall)
    
    if(!is.null(result_df) && is.data.frame(result_df) && nrow(result_df) > 0) {
      # Add a title for this approach
      writeData(wb, "Alternative Estimators", paste0(approach, " Approach"), 
                startRow = if(exists("last_row")) last_row + 2 else 1)
      
      # Format for readability
      est_table <- result_df %>%
        mutate(
          Estimate = paste0(round(Estimate*100, 2), "%"),
          CI_Range = paste0("[", round(CI_Lower*100, 2), "-", round(CI_Upper*100, 2), "%]"),
          tau2 = round(tau2, 4),
          I2 = paste0(round(I2, 1), "%")
        ) %>%
        select(Estimator, Estimate, CI_Range, tau2, I2) %>%
        rename(
          "Estimator" = Estimator,
          "Pooled Proportion" = Estimate,
          "95% Confidence Interval" = CI_Range,
          "τ²" = tau2,
          "I²" = I2
        )
      
      # Write the data and update the last row position
      writeData(wb, "Alternative Estimators", est_table, 
                startRow = if(exists("last_row")) last_row + 3 else 2)
      addStyle(wb, "Alternative Estimators", createStyle(textDecoration = "bold"), 
               rows = if(exists("last_row")) last_row + 3 else 2, 
               cols = 1:ncol(est_table))
      
      last_row <- if(exists("last_row")) last_row + 3 + nrow(est_table) else 2 + nrow(est_table)
    }
  }
  
  # 5. Sample Size Threshold Analysis
  addWorksheet(wb, "Sample Size Thresholds")
  
  threshold_combined <- data.frame()
  
  for(approach in c("First-Method", "Single-Method", "Overall")) {
    result_df <- switch(approach,
                       "First-Method" = sample_size_results$first,
                       "Single-Method" = sample_size_results$single,
                       "Overall" = sample_size_results$overall)
    
    if(!is.null(result_df) && is.data.frame(result_df) && nrow(result_df) > 0) {
      result_df <- result_df %>%
        mutate(Approach = approach)
      
      threshold_combined <- bind_rows(threshold_combined, result_df)
    }
  }
  
  if(nrow(threshold_combined) > 0) {
    # Format for readability
    threshold_combined <- threshold_combined %>%
      mutate(
        Estimate_Fmt = paste0(round(Estimate*100, 2), "%"),
        CI_Range = paste0("[", round(CI_Lower*100, 2), "-", round(CI_Upper*100, 2), "%]"),
        I2_Fmt = paste0(round(I2, 1), "%"),
        Studies_Pct = paste0(round(Proportion_of_Studies*100, 1), "%"),
        Participants_Pct = paste0(round(Proportion_of_Participants*100, 1), "%")
      ) %>%
      select(Approach, Threshold, Studies_Included, Studies_Pct, 
             Participants_Included, Participants_Pct, Estimate_Fmt, CI_Range, I2_Fmt) %>%
      rename(
        "Approach" = Approach,
        "Min Sample Size" = Threshold,
        "Studies" = Studies_Included,
        "% of Studies" = Studies_Pct,
        "Participants" = Participants_Included,
        "% of Participants" = Participants_Pct,
        "Pooled Estimate" = Estimate_Fmt,
        "95% CI" = CI_Range,
        "I²" = I2_Fmt
      ) %>%
      arrange(Approach, `Min Sample Size`)
    
    writeData(wb, "Sample Size Thresholds", threshold_combined)
    addStyle(wb, "Sample Size Thresholds", createStyle(textDecoration = "bold"), 
             rows = 1, cols = 1:ncol(threshold_combined))
  } else {
    writeData(wb, "Sample Size Thresholds", "No sample size threshold results available")
  }
  
  # Apply formatting to all worksheets
  for(sheet in names(wb)) {
    # Auto-adjust column widths for better readability
    setColWidths(wb, sheet, cols = 1:50, widths = "auto")
    
    # Add some styling to the headers
    addStyle(wb, sheet, createStyle(fgFill = "#DDEBF7", border = "Bottom", 
                                   borderColour = "#4472C4", borderStyle = "medium"),
             rows = 1, cols = 1:50, gridExpand = TRUE)
  }
  
  # Save the workbook
  file_path <- "output/r_results/sensitivity_analysis_results.xlsx"
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  cat("  Excel workbook created and saved to:", file_path, "\n")
  return(file_path)
}

excel_path <- create_excel_summary()

# Final notification
cat("\nPublication bias and sensitivity analyses complete.\n")
cat("All results saved to output/r_results/ and output/figures/\n")
cat("Summary Excel workbook has been generated at:\n")
cat(excel_path, "\n\n")