###############################################################################
# 05_compare_metas.R
# 
# This script compares the results of the three meta-analytical approaches:
# 1. First-Method Approach (PRIMARY)
# 2. Single-Method Approach (SECONDARY)
# 3. Overall/Total Approach (TERTIARY)
#
# For each approach, it loads and compares:
# - Overall pooled proportions and confidence intervals
# - Study counts and sample sizes
# - Heterogeneity statistics
#
# It also conducts subgroup comparisons across approaches where applicable.
# Results are saved in CSV format and prepared for visualization.
###############################################################################

library(tidyverse)
library(metafor)
library(glue)

load_results <- function(approach) {
  path <- glue("output/r_results/{approach}/overall_results.csv")
  results <- read_csv(path, show_col_types = FALSE)
  cat(glue("Loaded {approach} results: {nrow(results)} rows\n"))
  return(results)
}

primary_results <- load_results("primary")
secondary_results <- load_results("secondary")
overall_results <- load_results("overall")

all_approaches <- bind_rows(
  primary_results,
  secondary_results,
  overall_results
) %>% 
  filter(!is.na(Analysis))

cat("\nCOMPARING META-ANALYSIS APPROACHES:\n")
cat("----------------------------------\n")

all_approaches <- all_approaches %>%
  mutate(
    Prop_Formatted = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]"),
    I2_Formatted = glue("{round(I2, 1)}%")
  )

cat("\nOverall Pooled Proportions:\n")
all_approaches %>%
  pwalk(function(Analysis, Prop_Formatted, k, N, ...) {
    cat(glue("  {Analysis}: {Prop_Formatted} ({k} studies, N={N})\n"))
  })

cat("\nPairwise Differences:\n")

approach_pairs <- combn(1:nrow(all_approaches), 2, simplify = FALSE) %>%
  map(~all_approaches[.x, ])

approach_pairs %>%
  map_dfr(~{
    approach1 <- .x$Analysis[1]
    approach2 <- .x$Analysis[2]
    prop1 <- .x$Pooled_Proportion[1]
    prop2 <- .x$Pooled_Proportion[2]
    ci_lower1 <- .x$CI_Lower[1]
    ci_upper1 <- .x$CI_Upper[1]
    ci_lower2 <- .x$CI_Lower[2]
    ci_upper2 <- .x$CI_Upper[2]
    
    abs_diff <- abs(prop1 - prop2)
    rel_diff <- abs_diff / min(prop1, prop2) * 100
    
    overlap <- !(ci_upper1 < ci_lower2 || ci_upper2 < ci_lower1)
    
    tibble(
      Comparison = glue("{approach1} vs {approach2}"),
      Absolute_Diff = abs_diff,
      Percentage_Diff = rel_diff,
      Overlap_CI = overlap
    )
  }) %>%
  pwalk(function(Comparison, Absolute_Diff, Percentage_Diff, Overlap_CI, ...) {
    overlap_text <- if_else(Overlap_CI, "- CIs overlap", "- CIs do not overlap")
    cat(glue("  {Comparison}: {round(Absolute_Diff * 100, 2)} percentage points ({round(Percentage_Diff, 1)}%) {overlap_text}\n"))
  })

write_csv(all_approaches, "output/r_results/comparisons/approach_comparison.csv")

cat("\nTest for Statistical Heterogeneity Between Approaches:\n")

test_data <- all_approaches %>%
  mutate(
    yi = log(Pooled_Proportion / (1 - Pooled_Proportion)),
    vi = (CI_Upper - CI_Lower)^2 / (2 * 1.96)^2
  )

meta_test <- rma(yi = yi, vi = vi, data = test_data)

p_value_text <- if_else(meta_test$QEp < 0.001, "< 0.001", glue("= {round(meta_test$QEp, 3)}"))
cat(glue("  Q({meta_test$k-1}) = {round(meta_test$QE, 2)}, p {p_value_text}\n"))

significance_text <- if_else(meta_test$QEp < 0.05,
                            "There is significant heterogeneity between approaches (p < 0.05)",
                            "There is no significant heterogeneity between approaches (p >= 0.05)")
cat(glue("  {significance_text}\n"))

cat("\nCOMPARING SUBGROUP RESULTS ACROSS APPROACHES:\n")
cat("--------------------------------------------\n")

subgroup_variables <- c(
  "method_type", 
  "sample_source_name", 
  "sample_platform_name", 
  "sample_method_name",
  "journal_name"
)

subgroup_variables %>%
  walk(function(var) {
    var_name <- str_replace(var, "_name$", "")
    cat(glue("\nComparing by {var_name}:\n"))
    
    approaches <- c("primary", "secondary", "overall")
    
    subgroup_data <- approaches %>%
      set_names() %>%
      map(~{
        file_path <- glue("output/r_results/{.x}/subgroup_{var}.csv")
        data <- read_csv(file_path, show_col_types = FALSE) %>%
          mutate(Approach = .x)
        cat(glue("  Loaded {.x} approach: {nrow(data)} subgroups\n"))
        return(data)
      }) %>%
      compact()
    
    combined_subgroups <- bind_rows(subgroup_data)
    
    comparison_wide <- combined_subgroups %>%
      mutate(
        Prop_CI = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]")
      ) %>%
      select(Subgroup, Approach, Prop_CI, k) %>%
      pivot_wider(
        id_cols = Subgroup,
        names_from = Approach,
        values_from = c(Prop_CI, k),
        names_sep = "_"
      )
    
    output_file <- glue("output/r_results/comparisons/subgroup_{var_name}_comparison.csv")
    write_csv(comparison_wide, output_file)
    cat(glue("  Saved comparison to {output_file}\n"))
    
    subgroup_diffs <- combined_subgroups %>%
      select(Subgroup, Approach, Pooled_Proportion) %>%
      group_by(Subgroup) %>%
      filter(n() > 1) %>%
      summarize(
        max_diff = max(Pooled_Proportion) - min(Pooled_Proportion),
        max_diff_pct = max_diff / min(Pooled_Proportion) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(max_diff_pct))
    
    cat("\n  Subgroups with largest proportional differences between approaches:\n")
    subgroup_diffs %>%
      head(3) %>%
      pwalk(function(Subgroup, max_diff, max_diff_pct, ...) {
        cat(glue("    {Subgroup}: {round(max_diff * 100, 2)} percentage points ({round(max_diff_pct, 1)}%)\n"))
      })
  })

cat("\nCOMPARING TEMPORAL TRENDS ACROSS APPROACHES:\n")
cat("------------------------------------------\n")

approaches <- c("primary", "secondary", "overall")

temporal_data <- approaches %>%
  set_names() %>%
  map(~{
    file_path <- glue("output/r_results/{.x}/temporal_trends.csv")
    data <- read_csv(file_path, show_col_types = FALSE) %>%
      mutate(Approach = .x)
    cat(glue("Loaded {.x} approach: {nrow(data)} time points\n"))
    return(data)
  }) %>%
  compact()

combined_temporal <- bind_rows(temporal_data)

temporal_wide <- combined_temporal %>%
  mutate(
    Year = as.numeric(Subgroup),
    Prop_CI = glue("{round(Pooled_Proportion * 100, 2)}% [{round(CI_Lower * 100, 2)}-{round(CI_Upper * 100, 2)}%]")
  ) %>%
  select(Year, Approach, Prop_CI, k) %>%
  pivot_wider(
    id_cols = Year,
    names_from = Approach,
    values_from = c(Prop_CI, k),
    names_sep = "_"
  ) %>%
  arrange(Year)

write_csv(temporal_wide, "output/r_results/comparisons/temporal_comparison.csv")
cat("Saved temporal comparison to output/r_results/comparisons/temporal_comparison.csv\n")

cat("\nTemporal Trend Analysis:\n")

names(temporal_data) %>%
  walk(function(approach) {
    trend_data <- temporal_data[[approach]] %>%
      mutate(Year = as.numeric(Subgroup) - min(as.numeric(Subgroup)))
    
    trend_model <- rma(yi = log(Pooled_Proportion / (1 - Pooled_Proportion)), 
                      vi = (CI_Upper - CI_Lower)^2 / (2 * 1.96)^2, 
                      mods = ~ Year, 
                      data = trend_data,
                      method = "REML")
    
    slope <- trend_model$b[2]
    p_value <- trend_model$pval[2]
    direction <- if_else(slope > 0, "increasing", "decreasing")
    p_value_text <- if_else(p_value < 0.001, "< 0.001", glue("= {round(p_value, 3)}"))
    
    cat(glue("  {approach} approach: {direction} trend (slope = {round(slope, 4)}, p {p_value_text})\n"))
  })

cat("\nComparison of meta-analysis approaches complete. Results saved to output/r_results/comparisons/\n")