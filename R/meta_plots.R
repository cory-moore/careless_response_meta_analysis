###############################################################################
# R script for generating forest and funnel plots from meta-analysis results
#
# This script creates visualizations of meta-analysis results, focusing on 
# proportions of careless responding in survey data. It generates forest plots 
# to display effect sizes across subgroups and funnel plots to assess 
# publication bias.
#
# Workflow:
# 1. Load and install required packages:
#    - dplyr: For data manipulation and transformation
#    - ggplot2: For creating high-quality plots
#    - gridExtra: For arranging multiple plots in a grid layout
#    - forcats: For factor manipulation in plots
#
# 2. Define `create_forest_plot` function that:
#    - Takes meta-analysis results and group name as inputs
#    - Handles different column naming conventions in input data
#    - Filters data for the specified group and non-total subgroups
#    - Sorts subgroups by prevalence in descending order
#    - Creates a forest plot showing point estimates and confidence intervals
#    - Applies consistent formatting with proper axis scaling and labels
#    - Returns a ggplot object for further manipulation or saving
#
# 3. Define `create_funnel_plot` function that:
#    - Takes meta-analysis results and group name as inputs
#    - Calculates precision (1/SE) for the y-axis
#    - Plots effect sizes against precision to visualize publication bias
#    - Adds a vertical reference line at the overall pooled estimate
#    - Returns a formatted ggplot object with appropriate labels and theme
#
# 4. Define `main` function that:
#    - Creates output directory structure if needed
#    - Locates and loads the appropriate results file with flexible path handling
#    - Extracts unique analysis groups from the data
#    - Generates and saves individual forest plots for each group
#    - Creates a combined forest plot for key groups (Year, Sample Source, CR Method)
#    - Generates and saves funnel plots for each group and overall results
#    - Handles errors gracefully with informative messages
#    - Saves all plots as high-resolution PNG files
#
# Key visualization features:
# - Forest plots: Show effect sizes (proportions) with 95% confidence intervals
#   for each subgroup, ordered by effect size magnitude
# - Funnel plots: Display effect sizes against precision (1/SE) to assess
#   publication bias, with a reference line at the pooled estimate
# - Combined plots: Aggregate key forest plots for easier comparison across
#   important moderator variables
#
###############################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)

create_forest_plot <- function(data, group_name, title_prefix = "Forest Plot of ") {
  pooled_prev_col <- ifelse("Pooled.Prevalence" %in% colnames(data), "Pooled.Prevalence", "Pooled Prevalence")
  lower_ci_col <- ifelse("Lower.CI" %in% colnames(data), "Lower.CI", "Lower CI")
  upper_ci_col <- ifelse("Upper.CI" %in% colnames(data), "Upper.CI", "Upper CI")
  
  group_data <- data %>%
    filter(Group == group_name, Subgroup != "Total" & Subgroup != "") %>%
    arrange(desc(!!sym(pooled_prev_col)))
  
  if (nrow(group_data) == 0) {
    warning(paste("No data for group:", group_name))
    return(NULL)
  }
  
  group_data$Subgroup <- factor(group_data$Subgroup, levels = group_data$Subgroup)
  
  forest_plot <- ggplot(group_data, aes_string(y = "Subgroup", x = pooled_prev_col)) +
    geom_vline(xintercept = 0, linetype = "longdash", alpha = 0.5) +
    geom_errorbarh(aes_string(xmin = lower_ci_col, xmax = upper_ci_col), height = 0.2, color = "black") +
    geom_point(size = 3, color = "black", fill = "black") +
    labs(title = paste(title_prefix, group_name), x = "Proportion", y = NULL) +
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

create_funnel_plot <- function(data, group_name) {
  pooled_prev_col <- ifelse("Pooled.Prevalence" %in% colnames(data), "Pooled.Prevalence", "Pooled Prevalence")
  pooled_se_col <- ifelse("Pooled.Standard.Error" %in% colnames(data), "Pooled.Standard.Error", "Pooled Standard Error")
  
  group_data <- data %>% filter(Group == group_name)
  overall <- group_data %>% filter(Subgroup == "Total")
  
  if (nrow(group_data) == 0 || nrow(overall) == 0) {
    warning(paste("No data for group:", group_name))
    return(NULL)
  }
  
  group_data$precision <- 1 / group_data[[pooled_se_col]]
  
  funnel_plot <- ggplot(group_data, aes_string(x = pooled_prev_col, y = "precision")) +
    geom_point(alpha = 0.7, color = "black", fill = "black", size = 2) +
    geom_vline(xintercept = overall[[pooled_prev_col]][1], linetype = "dashed", color = "red") +
    labs(title = paste("Funnel Plot for", group_name), x = "Proportion", y = "Precision (1/SE)") +
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

main <- function() {
  if (!dir.exists("output/plots")) dir.create("output/plots", recursive = TRUE)
  
  file_path <- "output/pooled_proportions_r.csv"
  if (!file.exists(file_path)) {
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
  
  results <- read.csv(file_path)
  cat("Original column names:", paste(colnames(results), collapse=", "), "\n")
  
  groups <- unique(results$Group[results$Group != "Overall"])
  
  forest_plots <- list()
  for (group in groups) {
    cat("Creating forest plot for group:", group, "\n")
    plot <- create_forest_plot(results, group)
    if (!is.null(plot)) forest_plots[[group]] <- plot
  }
  
  for (group in names(forest_plots)) {
    filename <- paste0("output/plots/forest_plot_", gsub(" ", "_", tolower(group)), ".png")
    tryCatch({
      ggsave(filename, forest_plots[[group]], width = 10, height = 8, bg = "white", dpi = 300)
      cat("Saved forest plot for", group, "to", filename, "\n")
    }, error = function(e) {
      cat("Error saving forest plot for", group, ":", conditionMessage(e), "\n")
    })
  }
  
  key_groups <- c("Year", "Sample Source", "Careless Response Method")
  key_plots <- forest_plots[names(forest_plots) %in% key_groups]
  
  if (length(key_plots) > 0) {
    tryCatch({
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
  
  funnel_plots <- list()
  for (group in groups) {
    cat("Creating funnel plot for group:", group, "\n")
    plot <- create_funnel_plot(results, group)
    if (!is.null(plot)) {
      funnel_plots[[group]] <- plot
      filename <- paste0("output/plots/funnel_plot_", gsub(" ", "_", tolower(group)), ".png")
      tryCatch({
        ggsave(filename, plot, width = 8, height = 6, bg = "white", dpi = 300)
        cat("Saved funnel plot for", group, "to", filename, "\n")
      }, error = function(e) {
        cat("Error saving funnel plot for", group, ":", conditionMessage(e), "\n")
      })
    }
  }
  
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

main()