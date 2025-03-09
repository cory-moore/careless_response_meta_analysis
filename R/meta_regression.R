###############################################################################
# R script to perform meta-regression analyses on careless responding data
###############################################################################

# Set CRAN mirror first to avoid mirror selection error
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required packages if not already installed
required_packages <- c("dplyr", "metafor", "ggplot2", "gridExtra")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

library(dplyr)
library(metafor)
library(ggplot2)
library(gridExtra)

# Function to prepare data for meta-regression
prepare_data <- function(file_path) {
  # Read the data
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Print column names for debugging
  cat("Columns in data file:", paste(colnames(data), collapse=", "), "\n")
  
  # Check if critical columns exist
  if (!"proportions_total" %in% colnames(data)) {
    # Try to compute it if cr_total_amount and sample_size exist
    if ("cr_total_amount" %in% colnames(data) && "sample_size" %in% colnames(data)) {
      cat("Computing proportions_total from cr_total_amount and sample_size\n")
      data$proportions_total <- data$cr_total_amount / data$sample_size
    } else {
      stop("Cannot find or compute proportions_total column. Available columns: ", 
           paste(colnames(data), collapse=", "))
    }
  }
  
  # Check for zero or one values and apply small adjustments to avoid infinity in logit
  data$proportions_total <- pmin(pmax(data$proportions_total, 0.001), 0.999)
  
  # Convert proportions to logits and calculate variance
  data$logit_p <- log(data$proportions_total / (1 - data$proportions_total))
  data$var <- 1 / (data$sample_size * data$proportions_total * (1 - data$proportions_total))
  
  # Return prepared data
  return(data)
}

# Function to run a meta-regression model
run_meta_regression <- function(data, moderator_formula) {
  tryCatch({
    # First, run the intercept-only model for comparison
    intercept_model <- rma(yi = logit_p, vi = var, data = data, method = "REML")
    
    # Then run model with moderator
    model <- rma(yi = logit_p, vi = var, data = data, method = "REML", 
                mods = as.formula(paste("~", moderator_formula)))
    
    # Check if model has enough data points
    if (model$k < 5) {
      cat("  Warning: Model has fewer than 5 data points (", model$k, ")\n", sep="")
    }
    
    # Return both models
    return(list(model = model, intercept_model = intercept_model))
  }, error = function(e) {
    cat("Error fitting model with moderator:", moderator_formula, "\n")
    cat("  Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Function to create bubble plot for meta-regression
create_bubble_plot <- function(model_results, data, moderator, x_lab = NULL) {
  # Skip if model is NULL
  if (is.null(model_results)) return(NULL)
  
  model <- model_results$model
  
  # Set x-axis label if not provided
  if (is.null(x_lab)) x_lab <- moderator
  
  # Extract predicted values
  pred_data <- data.frame(
    x = data[[moderator]],
    y = data$logit_p,
    size = 1/sqrt(data$var)  # Weight by precision
  )
  
  # Create plot based on moderator type
  tryCatch({
    if (is.numeric(data[[moderator]])) {
      # For continuous moderators
      
      # Create a sequence of values for prediction
      moderator_range <- range(data[[moderator]], na.rm = TRUE)
      new_mod_values <- seq(from = moderator_range[1], to = moderator_range[2], length.out = 100)
      
      # Create prediction dataset
      newdata <- data.frame(x = new_mod_values)
      names(newdata) <- moderator
      
      # Get predictions
      pred <- predict(model, newmods = cbind(1, newdata[[moderator]]))
      
      # Create dataframe for line
      pred_line <- data.frame(
        x = newdata[[moderator]],
        y = pred$pred
      )
      
      # Create the plot
      p <- ggplot(pred_data, aes(x = x, y = y, size = size)) +
        geom_point(alpha = 0.5) +
        geom_line(data = pred_line, aes(x = x, y = y), color = "red", inherit.aes = FALSE) +
        labs(title = paste("Meta-regression: Effect of", moderator),
             x = x_lab,
             y = "Logit of Proportion",
             size = "Precision") +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom"
        )
      
      # Calculate r-squared and add as annotation
      if (!is.null(model_results$intercept_model)) {
        tau2_base <- model_results$intercept_model$tau2
        tau2_mod <- model$tau2
        r2 <- max(0, (tau2_base - tau2_mod) / tau2_base)  
        
        p <- p + annotate("text", x = min(data[[moderator]], na.rm = TRUE), 
                         y = max(data$logit_p, na.rm = TRUE),
                         label = sprintf("R² = %.2f%%", r2 * 100),
                         hjust = 0, vjust = 1)
      }
      
    } else {
      # For categorical moderators
      # Create boxplot + points
      p <- ggplot(pred_data, aes(x = factor(x), y = y)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        geom_jitter(aes(size = size), width = 0.2, alpha = 0.5) +
        labs(title = paste("Meta-regression: Effect of", moderator),
             x = x_lab,
             y = "Logit of Proportion",
             size = "Precision") +
        theme_classic() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    return(p)
  }, error = function(e) {
    cat("Error creating plot for moderator", moderator, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Function to format meta-regression results
format_meta_regression_results <- function(model_results, moderator) {
  if (is.null(model_results)) {
    return(data.frame(
      Moderator = moderator,
      QM = NA,
      QM_p = NA,
      QE = NA,
      QE_p = NA,
      I2 = NA,
      R2 = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  model <- model_results$model
  
  # Extract key statistics
  QM <- model$QM
  QM_p <- model$QMp
  QE <- model$QE
  QE_p <- model$QEp
  I2 <- model$I2
  
  # Calculate R-squared if we have both models
  R2 <- NA
  if (!is.null(model_results$intercept_model)) {
    tau2_base <- model_results$intercept_model$tau2
    tau2_mod <- model$tau2
    R2 <- max(0, (tau2_base - tau2_mod) / tau2_base)  
  }
  
  # Create formatted results
  results <- data.frame(
    Moderator = moderator,
    QM = round(QM, 2),
    QM_p = format.pval(QM_p, digits = 3),
    QE = round(QE, 2),
    QE_p = format.pval(QE_p, digits = 3),
    I2 = paste0(round(I2, 1), "%"),
    R2 = ifelse(is.na(R2), NA, paste0(round(R2 * 100, 1), "%")),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Main function
main <- function() {
  # Create output directory if it doesn't exist
  if (!dir.exists("output/meta_regression")) {
    dir.create("output/meta_regression", recursive = TRUE)
  }
  
  # Find the right input file - try multiple possibilities
  possible_files <- c(
    "data/careless_data.csv",
    "output/proportions.xlsx",
    "output/proportions_r.xlsx",
    "output/pooled_proportions.csv",
    "output/pooled_proportions_r.csv",
    "output/random_effects_pooled_proportions.csv",
    "output/random_effects_pooled_proportions_R.csv"
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
    stop("No suitable data file found. Tried: ", paste(possible_files, collapse=", "))
  }
  
  # Prepare data for meta-regression
  data <- prepare_data(data_file)
  
  # List of potential moderators to examine - adjust based on available columns
  all_columns <- colnames(data)
  
  # Filter for potential continuous moderators
  continuous_moderators <- all_columns[all_columns %in% c(
    "year", "sample_size"
  )]
  
  # Filter for potential categorical moderators
  categorical_moderators <- all_columns[all_columns %in% c(
    "journal_code", "journal", "sample_source", "sample_method", 
    "sample_platform", "cr_multiple", "cr_sequential"
  )]
  
  cat("Using continuous moderators:", paste(continuous_moderators, collapse=", "), "\n")
  cat("Using categorical moderators:", paste(categorical_moderators, collapse=", "), "\n")
  
  # Initialize results dataframe
  all_results <- data.frame(
    Moderator = character(),
    QM = numeric(),
    QM_p = character(),
    QE = numeric(),
    QE_p = character(),
    I2 = character(),
    R2 = character(),
    stringsAsFactors = FALSE
  )
  
  # Store plots
  all_plots <- list()
  
  # Run meta-regression for each continuous moderator
  cat("Running meta-regression for continuous moderators...\n")
  for (moderator in continuous_moderators) {
    cat("  - Analyzing moderator:", moderator, "\n")
    
    # Run meta-regression
    model_results <- run_meta_regression(data, moderator)
    
    # Format and store results
    results <- format_meta_regression_results(model_results, moderator)
    all_results <- rbind(all_results, results)
    
    # Create and save bubble plot
    if (!is.null(model_results)) {
      plot <- create_bubble_plot(model_results, data, moderator)
      if (!is.null(plot)) {
        all_plots[[moderator]] <- plot
        ggsave(paste0("output/meta_regression/", moderator, "_bubble_plot.png"), 
              plot, width = 8, height = 6, bg = "white", dpi = 300)
        cat("    - Plot saved to output/meta_regression/", moderator, "_bubble_plot.png\n", sep="")
      }
    }
  }
  
  # Run meta-regression for each categorical moderator
  cat("Running meta-regression for categorical moderators...\n")
  for (moderator in categorical_moderators) {
    cat("  - Analyzing moderator:", moderator, "\n")
    
    # Make sure moderator has multiple levels
    if (length(unique(na.omit(data[[moderator]]))) > 1) {
      # Run meta-regression (categorical variables need to be treated as factors)
      model_results <- run_meta_regression(data, paste("factor(", moderator, ")", sep = ""))
      
      # Format and store results
      results <- format_meta_regression_results(model_results, moderator)
      all_results <- rbind(all_results, results)
      
      # Create and save bubble plot
      if (!is.null(model_results)) {
        plot <- create_bubble_plot(model_results, data, moderator)
        if (!is.null(plot)) {
          all_plots[[moderator]] <- plot
          ggsave(paste0("output/meta_regression/", moderator, "_bubble_plot.png"), 
                plot, width = 8, height = 6, bg = "white", dpi = 300)
          cat("    - Plot saved to output/meta_regression/", moderator, "_bubble_plot.png\n", sep="")
        }
      }
    } else {
      cat("    Skipping", moderator, "- not enough variation (only one level)\n")
    }
  }
  
  # Save all plots to PDF if possible
  if (length(all_plots) > 0) {
    tryCatch({
      pdf("output/meta_regression/all_meta_regression_plots.pdf", width = 10, height = 8)
      for (plot_name in names(all_plots)) {
        print(all_plots[[plot_name]])
      }
      dev.off()
      cat("All plots saved to output/meta_regression/all_meta_regression_plots.pdf\n")
    }, error = function(e) {
      cat("Error saving PDF of all plots:", conditionMessage(e), "\n")
    })
  }
  
  # Save results to CSV
  write.csv(all_results, "output/meta_regression/meta_regression_results.csv", row.names = FALSE)
  cat("Results saved to output/meta_regression/meta_regression_results.csv\n")
  
  # Create a table with model coefficients for each significant moderator
  cat("Creating coefficient tables for significant moderators...\n")
  coef_results <- data.frame()
  
  significant_moderators <- all_results$Moderator[all_results$QM_p == "<0.001" | 
                                                 all_results$QM_p == "0.001" |
                                                 grepl("^0\\.[0-9]{3}$", all_results$QM_p) & 
                                                 as.numeric(all_results$QM_p) < 0.05]
  
  if (length(significant_moderators) > 0) {
    for (moderator in significant_moderators) {
      cat("  - Extracting coefficients for significant moderator:", moderator, "\n")
      
      formula <- ifelse(moderator %in% categorical_moderators, 
                       paste("factor(", moderator, ")", sep = ""), 
                       moderator)
      
      model_results <- run_meta_regression(data, formula)
      
      if (!is.null(model_results)) {
        model <- model_results$model
        
        # Extract coefficients and format
        coefs <- coef(summary(model))
        coefs_df <- as.data.frame(coefs)
        coefs_df$Moderator <- rownames(coefs_df)
        coefs_df$moderator_group <- moderator
        
        # Convert back to proportion scale for interpretability
        coefs_df$prop_estimate <- exp(coefs_df$estimate) / (1 + exp(coefs_df$estimate))
        
        # Calculate lower and upper CI in proportion scale
        ci_lower <- exp(coefs_df$estimate - 1.96 * coefs_df$se) / 
                   (1 + exp(coefs_df$estimate - 1.96 * coefs_df$se))
        ci_upper <- exp(coefs_df$estimate + 1.96 * coefs_df$se) / 
                   (1 + exp(coefs_df$estimate + 1.96 * coefs_df$se))
        
        coefs_df$prop_ci <- paste0(round(ci_lower, 3), " - ", round(ci_upper, 3))
        
        # Add to combined results
        coef_results <- rbind(coef_results, coefs_df)
      }
    }
    
    # Save significant coefficients
    write.csv(coef_results, "output/meta_regression/significant_moderator_coefficients.csv", 
             row.names = FALSE)
    cat("Significant moderator coefficients saved to output/meta_regression/significant_moderator_coefficients.csv\n")
  } else {
    cat("No significant moderators found.\n")
  }
  
  cat("Meta-regression analysis complete. Results saved to output/meta_regression/\n")
}

# Run the main function
main()