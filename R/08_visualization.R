library(ggplot2)
library(dplyr)
library(meta)
library(metafor)
library(viridis)

##############################################
# Forest Plot of Method Types
##############################################
method_results <- read.csv("output/r_results/primary/subgroup_method_type.csv")
method_results <- method_results %>% arrange(desc(Pooled_Proportion))

ggplot(method_results, aes(y = reorder(Subgroup, Pooled_Proportion))) +
  geom_point(aes(x = Pooled_Proportion), size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  labs(
    title = "Careless Responding Rates by Detection Method Type",
    subtitle = "First-Method Approach (PRIMARY)",
    x = "Proportion of Careless Responses",
    y = "Detection Method Type"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  annotate("text", x = max(method_results$CI_Upper) * 1.1, 
           y = 1:nrow(method_results), 
           label = paste0("k=", method_results$k), 
           hjust = 0) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(fill = "white")
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 100, 10, 10)) 

ggsave("output/figures/method_type_forest_plot.png", width = 10, height = 7, dpi = 300)  

##############################################
# Forest Plot of Method Timing (A Priori vs. Post Hoc)
##############################################
timing_results <- read.csv("output/r_results/primary/subgroup_method_timing.csv")
timing_results <- timing_results %>% arrange(desc(Pooled_Proportion))

ggplot(timing_results, aes(y = reorder(Subgroup, Pooled_Proportion))) +
  geom_point(aes(x = Pooled_Proportion), size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  labs(
    title = "Careless Responding Rates by Detection Method Timing",
    subtitle = "First-Method Approach (PRIMARY)",
    x = "Proportion of Careless Responses",
    y = "Detection Method Timing"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  annotate("text", x = max(timing_results$CI_Upper) * 1.1, 
           y = 1:nrow(timing_results), 
           label = paste0("k=", timing_results$k), 
           hjust = 0) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(fill = "white")
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 100, 10, 10))

ggsave("output/figures/method_timing_forest_plot.png", width = 10, height = 5, dpi = 300)

# Add combination plot comparing method type and method timing
if(file.exists("output/r_results/primary/subgroup_method_type.csv") && 
   file.exists("output/r_results/primary/subgroup_method_timing.csv")) {
   
  type_results <- read.csv("output/r_results/primary/subgroup_method_type.csv")
  timing_results <- read.csv("output/r_results/primary/subgroup_method_timing.csv")
  
  # Combine datasets with a classification column
  type_results$classification <- "Method Type"
  timing_results$classification <- "Method Timing"
  
  combined_results <- rbind(
    select(type_results, Subgroup, Pooled_Proportion, CI_Lower, CI_Upper, k, classification),
    select(timing_results, Subgroup, Pooled_Proportion, CI_Lower, CI_Upper, k, classification)
  )
  
  # Create multi-panel plot
  ggplot(combined_results, aes(y = reorder(Subgroup, Pooled_Proportion), x = Pooled_Proportion)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
    facet_wrap(~classification, scales = "free_y", ncol = 1) +
    labs(
      title = "Careless Responding Rates by Method Classification",
      subtitle = "First-Method Approach (PRIMARY)",
      x = "Proportion of Careless Responses",
      y = ""
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "lightblue"),
      strip.text = element_text(face = "bold")
    )
  
  ggsave("output/figures/method_classifications_comparison.png", width = 10, height = 8, dpi = 300)
}

##############################################
# Funnel Plot Analysis
##############################################

first_method_data <- read.csv("data/for_r_meta/first_method_data.csv")

events <- round(first_method_data$proportion * first_method_data$sample_size)
n <- first_method_data$sample_size

has_zeros <- sum(events == 0)
if (has_zeros > 0) {
  needs_correction <- (events == 0)
  events[needs_correction] <- events[needs_correction] + 0.5
  n[needs_correction] <- n[needs_correction] + 1
  cat("\nApplied continuity correction to", sum(needs_correction), "studies with zero events\n")
}

es <- escalc(measure = "PLO", xi = events, ni = n)
res <- rma(yi = es$yi, vi = es$vi, method = "DL")

png("output/figures/funnel_plot_standard.png", width = 8, height = 7, units = "in", res = 300)

funnel(res, 
       xlab = "Proportion of Careless Responses",
       ylab = "Standard Error",
       atransf = transf.ilogit,
       ylim = c(1.5, 0),
       level = c(0.9, 0.95, 0.99),
       legend = TRUE,
       pch = 19,
       refline = res$b)

dev.off()

logit <- function(p) log(p/(1-p))
inv_logit <- function(x) exp(x)/(1+exp(x))

png("output/figures/funnel_plot_complete.png", width = 10, height = 8, units = "in", res = 300)

plot(
  transf.ilogit(es$yi), sqrt(es$vi),
  xlab = "Proportion of Careless Responses", 
  ylab = "Standard Error",
  xlim = c(0, 0.5),
  ylim = c(1.5, 0),
  pch = 19,
  cex = 0.7, 
  col = "black"
)

abline(v = inv_logit(res$b), lty = 2)

dev.off()

##############################################
# Forest Plot of Sample Platforms
##############################################
platform_results <- read.csv("output/r_results/primary/subgroup_sample_platform.csv")
platform_results <- platform_results %>% arrange(desc(Pooled_Proportion))

ggplot(platform_results, aes(y = reorder(Subgroup, Pooled_Proportion))) +
  geom_point(aes(x = Pooled_Proportion), size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  labs(
    title = "Careless Responding Rates by Sample Platform",
    subtitle = "First-Method Approach (PRIMARY)",
    x = "Proportion of Careless Responses",
    y = "Sample Platform"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  annotate("text", x = max(platform_results$CI_Upper) * 1.1, 
           y = 1:nrow(platform_results), 
           label = paste0("k=", platform_results$k, ", N=", format(platform_results$N, big.mark=",")), 
           hjust = 0) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_rect(fill = "white")
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 150, 10, 10))  

ggsave("output/figures/sample_platform_forest_plot.png", width = 11, height = 6, dpi = 300)  # Increased width from 9 to 11

##############################################
# Temporal Trend Analysis Plot
##############################################
year_results <- read.csv("output/r_results/primary/temporal_trends.csv")
year_results$Year <- as.numeric(as.character(year_results$Subgroup))
year_results <- year_results %>% arrange(Year)

meta_reg <- lm(Pooled_Proportion ~ Year, data = year_results, weights = k)
pred_years <- data.frame(Year = seq(min(year_results$Year), max(year_results$Year), by = 0.1))
pred_years$Predicted <- predict(meta_reg, newdata = pred_years)

min_year <- min(year_results$Year)
max_year <- max(year_results$Year)
year_breaks <- min_year:max_year

ggplot(year_results, aes(x = Year, y = Pooled_Proportion)) +
  geom_point(aes(size = k)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_line(data = pred_years, aes(x = Year, y = Predicted), linetype = "dashed", color = "blue") +
  labs(
    title = "Temporal Trend in Careless Responding Rates (2014-2024)",
    subtitle = "First-Method Approach (PRIMARY)",
    x = "Publication Year",
    y = "Proportion of Careless Responses",
    size = "Number of Studies"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = year_breaks) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white")
  )

ggsave("output/figures/temporal_trend_primary.png", width = 10, height = 6, dpi = 300)

##############################################
# Combined Forest Plot of Overall Approaches
##############################################
overall_data <- read.csv("output/r_results/comparisons/approach_comparison.csv")

ggplot(overall_data, aes(y = reorder(Analysis, Pooled_Proportion))) +
  geom_point(aes(x = Pooled_Proportion), size = 4, color = "black") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "black") +
  labs(
    title = "Overall Careless Responding Prevalence",
    subtitle = "Comparison of Analytical Approaches",
    x = "Proportion of Careless Responses",
    y = "",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1), 
                     limits = c(0.05, 0.10)) + 
  annotate("text", x = 0.095, 
           y = 1:nrow(overall_data), 
           label = paste0("k=", overall_data$k, ", N=", format(overall_data$N, big.mark=",")), 
           hjust = 0) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 100, 10, 10)) 

ggsave("output/figures/overall_approaches_forest.png", width = 10, height = 5, dpi = 300)  

cat("All figures have been saved to the 'output/figures' directory.\n")

##############################################
# Histogram of Proportions with Reference Lines
##############################################
unweighted_mean <- mean(transf.ilogit(es$yi))

png("output/figures/proportion_histogram.png", width = 10, height = 6, units = "in", res = 300)

par(mar = c(5, 4, 4, 8) + 0.1)  

hist(transf.ilogit(es$yi), 
     breaks = 20, 
     main = paste("Distribution of Proportions (All", length(es$yi), "Studies)"), 
     xlab = "Proportion of Careless Responses",
     col = "lightgrey",
     border = "white")

abline(v = inv_logit(res$b), col = "red", lwd = 2, lty = 2)
abline(v = unweighted_mean, col = "blue", lwd = 2, lty = 3)

legend("topright", 
       inset = c(-0.2, 0),
       legend = c(
         paste("Weighted Mean:", round(inv_logit(res$b) * 100, 1), "%"),
         paste("Unweighted Mean:", round(unweighted_mean * 100, 1), "%")
       ),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bty = "n",   
       xpd = TRUE   
)

dev.off()

##############################################
# Study Influence Analysis
##############################################
loo <- leave1out(res)

influence_values <- res$b - loo$estimate
influence_order <- order(abs(influence_values), decreasing = TRUE)

influence_data <- data.frame(
  id = first_method_data$ID[influence_order],
  influence = influence_values[influence_order],
  proportion = transf.ilogit(es$yi)[influence_order]
)

influence_data <- head(influence_data, 20)

png("output/figures/influence_plot.png", width = 11, height = 7, units = "in", res = 300)
par(mar = c(5, 10, 4, 2) + 0.1)  

barplot(influence_data$influence, 
        names.arg = influence_data$id, 
        horiz = TRUE,
        las = 1,  
        col = ifelse(influence_data$influence > 0, "darkred", "darkblue"),
        main = "Top 20 Most Influential Studies",
        xlab = "Change in Pooled Estimate (logit scale) if Study is Removed")

abline(v = 0, lty = 2)

text(ifelse(influence_data$influence > 0, 
           influence_data$influence + 0.01, 
           influence_data$influence - 0.01),
     seq(0.7, length(influence_data$influence) * 1.2, by = 1.2),
     paste0(round(influence_data$proportion * 100, 1), "%"),
     cex = 0.8)

dev.off()

##############################################
# Cook's Distance Plot for Influential Diagnostics
##############################################

# Calculate Cook's distances based on leave-one-out results
k <- length(loo$estimate)

# Vectorized calculation (more efficient)
# Cook's distance formula: D_i = (theta_hat - theta_hat_i)^2 / (p * MSE)
# For meta-analysis adaptation: D_i = (b - b_i)^2 / var(b_i)
overall_b <- rep(res$b, k)
cooks_d <- (overall_b - loo$estimate)^2 / (loo$se^2)

# Prepare data for plotting
cooks_data <- data.frame(
  ID = first_method_data$ID,
  cooks_distance = cooks_d,
  sample_size = first_method_data$sample_size,
  proportion = transf.ilogit(es$yi),
  study_index = 1:length(cooks_d)
)

# Sort by Cook's distance for ranking
cooks_data <- cooks_data[order(cooks_data$cooks_distance, decreasing = TRUE), ]
cooks_data$rank <- 1:nrow(cooks_data)

# Calculate cutoff (common rule of thumb: 4/n)
cutoff <- 4/nrow(cooks_data)

# Identify influential studies (exceeding cutoff)
cooks_data$influential <- cooks_data$cooks_distance > cutoff
cooks_data$label <- ifelse(cooks_data$influential, as.character(cooks_data$ID), "")

# Revert to original order for x-axis ordering in plot
cooks_data <- cooks_data[order(cooks_data$study_index), ]

# Create modern scatter plot of all Cook's distances
p <- ggplot(cooks_data, aes(x = study_index, y = cooks_distance)) +
  geom_point(aes(color = influential, size = proportion * 100), alpha = 0.7) +
  geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkred"),
                    labels = c("FALSE" = "Non-influential", "TRUE" = "Influential")) +
  scale_size_continuous(name = "Proportion (%)", range = c(2, 6)) +
  labs(
    title = "Influential Diagnostics Based on Cook's Distance",
    subtitle = paste0("Cutoff: ", round(cutoff, 4), " (", sum(cooks_data$influential), 
                    " studies exceed threshold)"),
    x = "Study Index",
    y = "Cook's Distance",
    color = "Influence Status"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  )

# Add a custom legend entry for the cutoff line
p <- p + guides(
  color = guide_legend(override.aes = list(size = 4)),
  size = guide_legend(),
  linetype = guide_legend(
    override.aes = list(color = "red", size = 0.5),
    title = NULL,
    keywidth = unit(2.5, "cm"),
    label.position = "right"
  )
) +
annotate("segment", x = 0, xend = 0, y = 0, yend = 0, 
        linetype = "dashed", color = "red", key_glyph = "path") +
scale_linetype_manual(name = NULL, 
                    values = "dashed", 
                    labels = paste0("Cutoff: ", round(cutoff, 4)),
                    guide = guide_legend(order = 3))

# Add text labels for influential studies
influential_subset <- cooks_data[cooks_data$influential, ]
if(nrow(influential_subset) > 0) {
  p <- p + 
    ggrepel::geom_text_repel(
      data = influential_subset,
      aes(label = ID),
      nudge_y = 0.002,
      size = 3.5,
      box.padding = 0.5,
      point.padding = 0.3,
      force = 1,
      direction = "both",
      segment.color = "gray50",
      min.segment.length = 0
    )
}

# Save the plot
ggsave("output/figures/cooks_distance_scatter.pdf", p, width = 12, height = 8)
ggsave("output/figures/cooks_distance_scatter.png", p, width = 12, height = 8, dpi = 300)

# Create horizontal bar plot version (alternative visualization)
# Order by Cook's distance value
cooks_data <- cooks_data[order(cooks_data$cooks_distance, decreasing = TRUE), ]

# Create data frame for horizontal bar plot
p_bar <- ggplot(cooks_data, aes(x = reorder(ID, cooks_distance), y = cooks_distance)) +
  geom_col(aes(fill = influential)) +
  geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkred"),
                  labels = c("FALSE" = "Non-influential", "TRUE" = "Influential")) +
  annotate("text", x = 10, y = cutoff * 1.1, 
         label = paste0("Cutoff = 4/n = ", round(cutoff, 4)), 
         color = "red", hjust = 0) +
  labs(
    title = "Influential Diagnostics Based on Cook's Distance",
    subtitle = paste0("Cutoff: 4/n = ", round(cutoff, 4), " (", sum(cooks_data$influential), 
                    " studies exceed threshold)"),
    x = "Study ID",
    y = "Cook's Distance",
    fill = "Influence Status"
  ) +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.x = element_line(color = "gray90")
  )

# Add text annotations for proportions
p_bar <- p_bar + 
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), "%")),
    hjust = -0.2,
    size = 3
  )

# Save the bar plot version
ggsave("output/figures/cooks_distance_plot.pdf", p_bar, width = 11, height = 14)
ggsave("output/figures/cooks_distance_plot.png", p_bar, width = 11, height = 14, dpi = 300)

##############################################
# Weight Analysis Plots
##############################################
weights <- 1/es$vi
weights_percentage <- (weights/sum(weights)) * 100

weight_data <- data.frame(
  proportion = transf.ilogit(es$yi),
  sample_size = n,
  weight_percentage = weights_percentage
)

png("output/figures/weight_analysis.png", width = 12, height = 6, units = "in", res = 300)
par(mfrow = c(1, 2))

plot(
  weight_data$proportion, weight_data$weight_percentage,
  xlab = "Proportion of Careless Responses",
  ylab = "Study Weight (%)",
  pch = 19,
  cex = 0.7,
  main = "Study Weight by Proportion"
)
abline(v = inv_logit(res$b), lty = 2, col = "red")

plot(
  weight_data$sample_size, weight_data$weight_percentage,
  xlab = "Sample Size",
  ylab = "Study Weight (%)",
  pch = 19,
  cex = 0.7,
  main = "Study Weight by Sample Size"
)

dev.off()

##############################################
# Heterogeneity Visualization
##############################################
sampling_var <- sum(es$vi)/length(es$vi)
tau2 <- res$tau2
total_var <- sampling_var + tau2
het_data <- c(sampling_var, tau2)
labels <- c("Within-study variance", "Between-study variance")
percentages <- round(het_data/total_var * 100, 1)

png("output/figures/heterogeneity_components.png", width = 8, height = 6, units = "in", res = 300)

pie(het_data, 
    labels = paste0(labels, "\n(", percentages, "%)"),
    col = c("lightblue", "coral"),
    main = paste0("Heterogeneity Components (I² = ", round(res$I2, 1), "%)"))

dev.off()