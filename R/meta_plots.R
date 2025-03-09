###############################################################################
# R script to generate forest plots from meta-analysis results (fixed version)
###############################################################################

# Set CRAN mirror first to avoid mirror selection error
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required packages if not already installed
required_packages <- c("dplyr", "ggplot2", "gridExtra", "forcats")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)

# Function to create a forest plot for a specific group
create_forest_plot <- function(data, group_name, title_prefix = "Forest Plot of ") {
  # Determine column names
  pooled_prev_col <- ifelse("Pooled.Prevalence" %in% colnames(data), "Pooled.Prevalence", "Pooled Prevalence")
  lower_ci_col <- ifelse("Lower.CI" %in% colnames(data), "Lower.CI", "Lower CI")
  upper_ci_col <- ifelse("Upper.CI" %in% colnames(data), "Upper.CI", "Upper CI")
  
  # Filter data for the specific group and non-Total subgroups
  group_data <- data %>%
    filter(Group == group_name, Subgroup != "Total" & Subgroup != "")
  
  # Skip if no data
  if (nrow(group_data) == 0) {
    warning(paste("No data for group:", group_name))
    return(NULL)
  }
  
  # Sort data by prevalence (descending)
  group_data <- group_data %>% 
    arrange(desc(!!sym(pooled_prev_col)))
  
  # Set factor levels for proper ordering in the plot
  group_data$Subgroup <- factor(group_data$Subgroup, 
                               levels = group_data$Subgroup)
  
  # Create the forest plot
  forest_plot <- ggplot(group_data, aes_string(y = "Subgroup", x = pooled_prev_col)) +
    geom_vline(xintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbarh(aes_string(xmin = lower_ci_col, xmax = upper_ci_col), 
                  height = 0.2, color = "black") +
    geom_point(size = 3, color = "black", fill = "black") +
    labs(title = paste(title_prefix, group_name),
         x = "Proportion",
         y = NULL) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.y = element_text(hjust = 0),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor.x = element_line(color = "gray95")
    ) +
    scale_x_continuous(limits = c(0, max(group_data[[upper_ci_col]], na.rm = TRUE) * 1.1),
                      breaks = seq(0, ceiling(max(group_data[[upper_ci_col]], na.rm = TRUE) * 10) / 10, by = 0.05))
  
  return(forest_plot)
}

# Function to create a funnel plot to assess publication bias
create_funnel_plot <- function(data, group_name) {
  # Determine column names
  pooled_prev_col <- ifelse("Pooled.Prevalence" %in% colnames(data), "Pooled.Prevalence", "Pooled Prevalence")
  pooled_se_col <- ifelse("Pooled.Standard.Error" %in% colnames(data), "Pooled.Standard.Error", "Pooled Standard Error")
  
  # Filter data for the specific group
  group_data <- data %>%
    filter(Group == group_name)
  
  # Add overall pooled estimate
  overall <- group_data %>%
    filter(Subgroup == "Total")
  
  # Skip if no data
  if (nrow(group_data) == 0 || nrow(overall) == 0) {
    warning(paste("No data for group:", group_name))
    return(NULL)
  }
  
  # Calculate precision (1/SE)
  group_data$precision <- 1 / group_data[[pooled_se_col]]
  
  # Create funnel plot
  funnel_plot <- ggplot(group_data, aes_string(x = pooled_prev_col, y = "precision")) +
    geom_point(alpha = 0.7, color = "black", fill = "black", size = 2) +
    geom_vline(xintercept = overall[[pooled_prev_col]][1], linetype = "dashed", color = "red") +
    labs(title = paste("Funnel Plot for", group_name),
         x = "Proportion",
         y = "Precision (1/SE)") +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
  
  return(funnel_plot)
}

# Main function
main <- function() {
  # Create plots directory if it doesn't exist
  if (!dir.exists("output/plots")) {
    dir.create("output/plots", recursive = TRUE)
  }
  
  # Read pooled results
  file_path <- "output/pooled_proportions_r.csv"
  if (!file.exists(file_path)) {
    # Try alternative filenames
    alternative_paths <- c(
      "output/random_effects_pooled_proportions.csv",
      "output/random_effects_pooled_proportions_R.csv",
      "output/pooled_proportions.csv"
    )
    
    for (alt_path in alternative_paths) {
      if (file.exists(alt_path)) {
        file_path <- alt_path
        cat("Using file:", file_path, "\n")
        break
      }
    }
  }
  
  # Read the data
  results <- read.csv(file_path)
  
  # Print column names for debugging
  cat("Original column names:", paste(colnames(results), collapse=", "), "\n")
  
  # Get unique groups (excluding "Overall")
  groups <- unique(results$Group[results$Group != "Overall"])
  
  # Create forest plots
  forest_plots <- list()
  for (group in groups) {
    cat("Creating forest plot for group:", group, "\n")
    plot <- create_forest_plot(results, group)
    if (!is.null(plot)) {
      forest_plots[[group]] <- plot
    }
  }
  
  # Save forest plots
  for (group in names(forest_plots)) {
    filename <- paste0("output/plots/forest_plot_", gsub(" ", "_", tolower(group)), ".png")
    tryCatch({
      ggsave(filename, forest_plots[[group]], width = 10, height = 8, bg = "white", dpi = 300)
      cat("Saved forest plot for", group, "to", filename, "\n")
    }, error = function(e) {
      cat("Error saving forest plot for", group, ":", conditionMessage(e), "\n")
    })
  }
  
  # Create combined forest plot for selected groups
  key_groups <- c("Year", "Sample Source", "Careless Response Method")
  key_plots <- forest_plots[names(forest_plots) %in% key_groups]
  
  if (length(key_plots) > 0) {
    tryCatch({
      # Make sure we have plots to combine
      if (length(key_plots) > 0) {
        combined_plot <- gridExtra::grid.arrange(grobs = key_plots, ncol = 1)
        ggsave("output/plots/combined_forest_plot.png", combined_plot, width = 12, height = 15, 
              bg = "white", dpi = 300)
        cat("Saved combined forest plot to output/plots/combined_forest_plot.png\n")
      }
    }, error = function(e) {
      cat("Error saving combined forest plot:", conditionMessage(e), "\n")
    })
  }
  
  # Create funnel plots for publication bias assessment
  funnel_plots <- list()
  for (group in groups) {
    cat("Creating funnel plot for group:", group, "\n")
    plot <- create_funnel_plot(results, group)
    if (!is.null(plot)) {
      funnel_plots[[group]] <- plot
      
      # Save individual funnel plots
      filename <- paste0("output/plots/funnel_plot_", gsub(" ", "_", tolower(group)), ".png")
      tryCatch({
        ggsave(filename, plot, width = 8, height = 6, bg = "white", dpi = 300)
        cat("Saved funnel plot for", group, "to", filename, "\n")
      }, error = function(e) {
        cat("Error saving funnel plot for", group, ":", conditionMessage(e), "\n")
      })
    }
  }
  
  # Create overall funnel plot
  cat("Creating overall funnel plot\n")
  overall_funnel <- create_funnel_plot(results, "Overall")
  if (!is.null(overall_funnel)) {
    tryCatch({
      ggsave("output/plots/funnel_plot_overall.png", overall_funnel, width = 8, height = 6, 
            bg = "white", dpi = 300)
      cat("Saved overall funnel plot to output/plots/funnel_plot_overall.png\n")
    }, error = function(e) {
      cat("Error saving overall funnel plot:", conditionMessage(e), "\n")
    })
  }
  
  cat("All plots saved to output/plots directory\n")
}

# Run the main function
main()