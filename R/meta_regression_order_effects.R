###############################################################################
# R script to analyze order effects in sequential CR screening
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

# Function to extract order-specific data from the dataset
extract_order_data <- function(data) {
  # Initialize empty dataframe for order data
  order_data <- data.frame()
  
  # Loop through possible CR methods (1-4)
  for (i in 1:4) {
    method_col <- paste0("cr_", i, "_method")
    amount_col <- paste0("cr_", i, "_amount")
    
    # Check if columns exist
    if (method_col %in% colnames(data) && amount_col %in% colnames(data)) {
      # Extract data for this order position
      subset <- data[!is.na(data[[method_col]]) & !is.na(data[[amount_col]]), ]
      
      if (nrow(subset) > 0) {
        # Create order-specific dataframe
        order_subset <- data.frame(
          ID = subset$ID,
          sample_size = subset$sample_size,
          method = subset[[method_col]],
          amount = subset[[amount_col]],
          order_position = i,
          stringsAsFactors = FALSE
        )
        
        # Calculate proportion
        order_subset$proportion <- order_subset$amount / order_subset$sample_size
        
        # Apply logit transformation for meta-analysis
        order_subset$logit_p <- log(pmin(pmax(order_subset$proportion, 0.001), 0.999) / 
                                  (1 - pmin(pmax(order_subset$proportion, 0.001), 0.999)))
        order_subset$var <- 1 / (order_subset$sample_size * 
                               pmin(pmax(order_subset$proportion, 0.001), 0.999) * 
                               (1 - pmin(pmax(order_subset$proportion, 0.001), 0.999)))
        
        # Add to the combined dataset
        order_data <- rbind(order_data, order_subset)
      }
    }
  }
  
  return(order_data)
}

# Function to run meta-regression on order effects
analyze_order_effects <- function(order_data) {
  # Check if we have enough data
  if (nrow(order_data) < 10) {
    cat("Not enough data for order effects analysis (need at least 10 data points)\n")
    return(NULL)
  }
  
  # Run meta-regression with order as moderator
  order_model <- try(rma(yi = logit_p, vi = var, data = order_data, 
                        mods = ~ factor(order_position)), silent = TRUE)
  
  if (inherits(order_model, "try-error")) {
    cat("Error fitting order effects model:", conditionMessage(order_model), "\n")
    return(NULL)
  }
  
  # Run model with method as moderator
  method_model <- try(rma(yi = logit_p, vi = var, data = order_data, 
                         mods = ~ factor(method)), silent = TRUE)
  
  if (inherits(method_model, "try-error")) {
    cat("Error fitting method effects model:", conditionMessage(method_model), "\n")
    method_model <- NULL
  }
  
  # Run model with interaction between method and order
  interaction_model <- try(rma(yi = logit_p, vi = var, data = order_data, 
                              mods = ~ factor(method) * factor(order_position)), silent = TRUE)
  
  if (inherits(interaction_model, "try-error")) {
    cat("Error fitting interaction model:", conditionMessage(interaction_model), "\n")
    interaction_model <- NULL
  }
  
  # Return all models
  return(list(
    order_model = order_model,
    method_model = method_model,
    interaction_model = interaction_model
  ))
}

# Function to create visualizations for order effects
create_order_plots <- function(order_data, models) {
  plot_list <- list()
  
  # 1. Basic plot of CR rates by position
  if (nrow(order_data) > 0) {
    # Calculate means for each position
    position_means <- order_data %>%
      group_by(order_position) %>%
      summarize(
        mean_proportion = mean(proportion, na.rm = TRUE),
        mean_logit = mean(logit_p, na.rm = TRUE),
        n_studies = n()
      )
    
    # Create plot of rates by position
    p1 <- ggplot(order_data, aes(x = factor(order_position), y = proportion)) +
      geom_boxplot(fill = "lightblue", alpha = 0.5) +
      geom_jitter(aes(size = 1/sqrt(var)), width = 0.2, alpha = 0.5) +
      geom_text(data = position_means, 
               aes(label = paste0("n=", n_studies), y = 0), 
               vjust = -1, size = 3) +
      labs(title = "Careless Responding Rates by Screening Position",
           x = "Position in Screening Sequence",
           y = "CR Rate",
           size = "Precision") +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    plot_list[["position_plot"]] <- p1
  }
  
  # 2. Plot of CR rates by position and method
  if (nrow(order_data) > 0) {
    # Ensure method is treated as factor
    order_data$method_factor <- factor(order_data$method)
    
    p2 <- ggplot(order_data, aes(x = factor(order_position), y = proportion, 
                               color = method_factor)) +
      geom_jitter(aes(size = 1/sqrt(var)), width = 0.2, alpha = 0.7) +
      labs(title = "CR Rates by Position and Method",
           x = "Position in Screening Sequence",
           y = "CR Rate",
           color = "Method",
           size = "Precision") +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    plot_list[["position_method_plot"]] <- p2
  }
  
  # 3. Plot showing predicted values from the order model
  if (!is.null(models$order_model)) {
    # Get predicted values
    preds <- data.frame(
      order_position = factor(1:4),
      pred = predict(models$order_model, 
                    newmods = model.matrix(~ factor(1:4) - 1)[, -1, drop = FALSE])$pred
    )
    
    # Get study counts
    pos_counts <- order_data %>%
      group_by(order_position) %>%
      summarize(n = n())
    
    # Convert predictions back to proportions
    preds$prop_pred <- exp(preds$pred) / (1 + exp(preds$pred))
    
    # Create plot
    p3 <- ggplot(preds, aes(x = order_position, y = prop_pred, group = 1)) +
      geom_line(color = "red", size = 1) +
      geom_point(size = 3) +
      geom_text(aes(label = paste0("n=", pos_counts$n[match(order_position, pos_counts$order_position)])), 
               vjust = -1, size = 3) +
      labs(title = "Predicted CR Rates by Screening Position",
           subtitle = paste("QM =", round(models$order_model$QM, 2), 
                          "p =", format.pval(models$order_model$QMp, digits = 3)),
           x = "Position in Screening Sequence",
           y = "Predicted CR Rate") +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    plot_list[["predicted_plot"]] <- p3
  }
  
  return(plot_list)
}

# Function to format meta-regression results
format_model_results <- function(model, model_name) {
  if (is.null(model)) {
    return(data.frame(
      Model = model_name,
      QM = NA,
      QM_p = NA,
      QE = NA,
      QE_p = NA,
      I2 = NA,
      k = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract key statistics
  QM <- model$QM
  QM_p <- model$QMp
  QE <- model$QE
  QE_p <- model$QEp
  I2 <- model$I2
  k <- model$k
  
  # Create formatted results
  results <- data.frame(
    Model = model_name,
    QM = round(QM, 2),
    QM_p = format.pval(QM_p, digits = 3),
    QE = round(QE, 2),
    QE_p = format.pval(QE_p, digits = 3),
    I2 = paste0(round(I2, 1), "%"),
    k = k,
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Function to extract coefficient details
extract_coefficients <- function(model, model_name) {
  if (is.null(model)) return(NULL)
  
  # Extract coefficients
  coefs <- coef(summary(model))
  coefs_df <- as.data.frame(coefs)
  coefs_df$parameter <- rownames(coefs_df)
  coefs_df$model <- model_name
  
  # Add proportion-scale estimates for easier interpretation
  coefs_df$prop_estimate <- exp(coefs_df$estimate) / (1 + exp(coefs_df$estimate))
  
  # Calculate proportion-scale confidence intervals
  ci_lower <- exp(coefs_df$estimate - 1.96 * coefs_df$se) / 
             (1 + exp(coefs_df$estimate - 1.96 * coefs_df$se))
  ci_upper <- exp(coefs_df$estimate + 1.96 * coefs_df$se) / 
             (1 + exp(coefs_df$estimate + 1.96 * coefs_df$se))
  
  coefs_df$prop_ci <- paste0(round(ci_lower, 3), " - ", round(ci_upper, 3))
  
  return(coefs_df)
}

# Main function
main <- function() {
  # Create output directory if it doesn't exist
  if (!dir.exists("output/order_effects")) {
    dir.create("output/order_effects", recursive = TRUE)
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
  
  # Read the data
  data <- read.csv(data_file, stringsAsFactors = FALSE)
  
  # Print column names for debugging
  cat("Columns in data file:", paste(colnames(data), collapse=", "), "\n")
  
  # Extract order-specific data
  cat("Extracting order-specific data...\n")
  order_data <- extract_order_data(data)
  
  cat("Extracted", nrow(order_data), "data points for order analysis\n")
  
  # Run meta-regression on order effects
  cat("Running meta-regression models for order effects...\n")
  models <- analyze_order_effects(order_data)
  
  # Create and save visualizations
  cat("Creating visualizations...\n")
  plots <- create_order_plots(order_data, models)
  
  for (plot_name in names(plots)) {
    filename <- paste0("output/order_effects/", plot_name, ".png")
    tryCatch({
      ggsave(filename, plots[[plot_name]], width = 8, height = 6, bg = "white", dpi = 300)
      cat("Saved plot to", filename, "\n")
    }, error = function(e) {
      cat("Error saving plot", plot_name, ":", conditionMessage(e), "\n")
    })
  }
  
  # Save combined plots to PDF
  if (length(plots) > 0) {
    tryCatch({
      pdf("output/order_effects/all_order_effect_plots.pdf", width = 10, height = 8)
      for (plot in plots) {
        print(plot)
      }
      dev.off()
      cat("All plots saved to output/order_effects/all_order_effect_plots.pdf\n")
    }, error = function(e) {
      cat("Error saving PDF of all plots:", conditionMessage(e), "\n")
    })
  }
  
  # Save order-specific dataset
  write.csv(order_data, "output/order_effects/order_specific_data.csv", row.names = FALSE)
  cat("Order-specific dataset saved to output/order_effects/order_specific_data.csv\n")
  
  # Format and save model results
  if (!is.null(models)) {
    # Combine model results
    all_results <- rbind(
      format_model_results(models$order_model, "Order Position"),
      format_model_results(models$method_model, "CR Method"),
      format_model_results(models$interaction_model, "Method x Position")
    )
    
    write.csv(all_results, "output/order_effects/order_effects_results.csv", row.names = FALSE)
    cat("Model results saved to output/order_effects/order_effects_results.csv\n")
    
    # Extract and save coefficient details
    coefficients <- rbind(
      extract_coefficients(models$order_model, "Order Position"),
      extract_coefficients(models$method_model, "CR Method"),
      extract_coefficients(models$interaction_model, "Method x Position")
    )
    
    if (!is.null(coefficients)) {
      write.csv(coefficients, "output/order_effects/coefficient_details.csv", row.names = FALSE)
      cat("Coefficient details saved to output/order_effects/coefficient_details.csv\n")
    }
  }
  
  cat("Order effects analysis complete.\n")
}

# Run the main function
main()