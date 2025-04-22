scripts <- c("00_eda.R","01_data_import.R", "02_meta_analysis.R", "03_compare_metas.R", 
            "04_meta_regression.R", "05_multilevel_positions.R", "06_bias_sensitivity.R", 
            "07_results_summary.R", "08_visualization.R")

for(script in scripts) {
  cat("\n========== RUNNING:", script, "==========\n")
  result <- tryCatch(
    source(file.path("R", script)),
    error = function(e) cat("ERROR in", script, ":", e$message, "\n")
  )
  cat("\n")
}