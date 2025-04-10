# Careless Responding Meta-Analysis

This repository contains the code and analysis pipeline for a meta-analytic study of careless responding in psychological and organizational research. It provides a comprehensive and reproducible workflow for analyzing patterns, prevalence rates, and methodological considerations in detecting careless responding across published studies.

## Purpose

The primary aim of this repository is to:

1. Document the prevalence of careless responding in published research
2. Analyze how different detection methods affect careless responding estimates
3. Examine methodological factors that influence careless responding rates
4. Assess temporal and publication trends in careless responding research
5. Provide transparency in meta-analytic methods for the research community

## Repository Structure

```
careless_meta/
├── data/                       # Data files
│   ├── careless_data.csv       # Main dataset
│   └── coded_study_data_all.xlsx # Data for reliability analysis
├── python/                     # Python preprocessing scripts
│   ├── 00_preprocess.py        # Data cleaning and preparation
│   ├── 01_counts.py            # Generate descriptive counts
│   ├── 02_reliability.py       # Inter-rater reliability analysis
│   ├── 03_proportions.py       # Calculate careless proportions
│   ├── 04_meta_analysis.py     # Initial meta-analysis preparation
│   └── utils.py                # Utility functions
├── R/                          # R analysis scripts
│   ├── 00_eda.R                # Exploratory data analysis
│   ├── 01_data_import.R        # Import preprocessed data
│   ├── 02_meta_analysis.R      # Core meta-analysis
│   ├── 03_compare_metas.R      # Compare meta-analytic approaches
│   ├── 04_meta_regression.R    # Meta-regression models
│   ├── 05_multilevel_positions.R # Positional effects analysis
│   ├── 06_bias_sensitivity.R   # Publication bias and sensitivity analyses
│   ├── 07_results_summary.R    # Summarize results
│   ├── 08_visualization.R      # Create visualizations
│   └── run_all.R               # Execute all R scripts sequentially
├── output/                     # Analysis outputs
│   ├── data_examination/       # Data quality checks
│   ├── python_results/         # Results from Python preprocessing
│   ├── r_results/              # Results from R analyses
│   ├── figures/                # Generated figures
│   └── tables/                 # Generated tables
├── docs/                       # Documentation
├── codebook.json               # Codebook for variables and coding schemes
└── README.md                   # This file
```

## Data Flow

The analysis pipeline follows a sequential workflow:

1. **Data Preparation (Python)**
   - The `00_preprocess.py` script takes the coded study data and performs cleaning, validation, and transformation operations
   - Creates multiple datasets for different analytical approaches: first-method, single-method, sequential, and overall analyses
   - Exports prepared datasets to `data/for_meta/` directory

2. **Meta-Analysis Preparation (R)**
   - `01_data_import.R` imports the preprocessed datasets
   - Transforms proportion data for meta-analysis (logit transformations)
   - Prepares variables for subgroup analyses

3. **Core Meta-Analyses (R)**
   - `02_meta_analysis.R` performs three parallel meta-analytic approaches:
     - First-Method Approach: Using first methods from sequential screening studies
     - Single-Method Approach: Only using single-method studies
     - Overall Approach: Using total careless responding amounts

4. **Advanced Analyses (R)**
   - Meta-regression models examine moderating factors
   - Multilevel models analyze positional effects in sequential screening
   - Publication bias and sensitivity analyses assess robustness

5. **Results Synthesis (R)**
   - Results summarization and table generation
   - Visualization of key findings
   - Comprehensive outputs for publication

## Key Analytical Approaches

The meta-analysis employs three main analytical strategies:

1. **First-Method Approach** (Primary Analysis)
   - Combines single-method studies with only the first methods from sequential screening
   - Maximizes sample size while controlling for method ordering effects

2. **Single-Method Approach** (Secondary Analysis)
   - Restricted to studies using only one careless responding detection method
   - Offers maximum internal validity but with reduced sample size

3. **Overall Approach** (Tertiary Analysis)
   - Examines total careless responding rates regardless of method configuration
   - Provides general prevalence estimates across the literature

## Reproducing the Analysis

To reproduce this analysis:

1. Clone this repository
2. Ensure Python and R are installed with required packages
3. Run the Python preprocessing scripts in numerical order:
   ```
   python python/00_preprocess.py
   python python/01_counts.py
   # etc.
   ```
4. Run the R analysis pipeline using the run_all.R script:
   ```
   Rscript R/run_all.R
   ```

## Codebook

The `codebook.json` file contains detailed information on all variables, coding schemes, and categorical mappings used in the analysis. This includes:

- Journal classifications
- Sample characteristics (source, recruitment, platform)
- Careless responding method categorizations
- Study design information
- Methodological timing classifications

## License

This repository is publicly available for research transparency and reproducibility purposes.

## Citation

If you use this code or analysis in your research, please cite the associated publication (details to be added upon publication).
