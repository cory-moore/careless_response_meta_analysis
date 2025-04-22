# Careless Responding Meta-Analysis

This repository contains the complete analysis pipeline for a meta-analytic study examining careless responding prevalence and detection methods in psychological and organizational research. The repository is designed for full reproducibility of the dissertation analysis.

## Repository Structure

```
careless_meta/
├── data/                       # Data files
│   ├── careless_data.csv       # Main dataset containing coded study information
│   └── coded_study_data_all.xlsx # Raw data for reliability analysis
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

## Required Data Files

The analysis requires two core data files in the `data/` directory:

1. `careless_data.csv`: Main dataset containing coded study information, including:
   - Study characteristics
   - Sample information
   - Careless responding detection methods
   - Prevalence rates
   - Methodological details

2. `coded_study_data_all.xlsx`: Raw data used for reliability analysis, containing:
   - Original coding sheets
   - Inter-rater reliability data
   - Coding decisions and notes

## Key Analytical Decisions

The analysis employs three complementary approaches to address different research questions:

1. **First-Method Approach** (Primary Analysis)
   - Combines single-method studies with first methods from sequential screening
   - Maximizes sample size while controlling for method ordering effects
   - Primary focus for prevalence estimates and method comparisons

2. **Single-Method Approach** (Secondary Analysis)
   - Restricted to studies using only one detection method
   - Provides method-specific estimates with maximum internal validity
   - Used for sensitivity analysis of method effects

3. **Overall Approach** (Tertiary Analysis)
   - Examines total careless responding rates across all methods
   - Provides general prevalence estimates
   - Used for temporal trend analysis

## Reproducing the Analysis

### Prerequisites
- Python 3.8+ with packages: pandas, numpy, scipy, openpyxl
- R 4.0+ with packages: meta, metafor, ggplot2, dplyr, viridis
- Required data files in the `data/` directory

### Execution Steps

1. Run Python preprocessing pipeline:
   ```bash
   python python/00_preprocess.py
   python python/01_counts.py
   python python/02_reliability.py
   python python/03_proportions.py
   python python/04_meta_analysis.py
   ```

2. Run R analysis pipeline:
   ```bash
   Rscript R/run_all.R
   ```

All outputs will be generated in the `output/` directory, organized by analysis type.

## Key Outputs

- **Meta-analytic Results**: Pooled estimates, heterogeneity statistics, and subgroup analyses
- **Method Comparisons**: Forest plots comparing different detection methods
- **Temporal Analysis**: Trends in careless responding rates over time
- **Publication Bias**: Funnel plots and sensitivity analyses
- **Influence Analysis**: Cook's distance plots and leave-one-out analyses

## Documentation

- `codebook.json`: Complete variable definitions and coding schemes
- `docs/`: Additional documentation on methodological decisions
- Script headers: Detailed comments explaining each analysis step

## License

This repository is publicly available for research transparency and reproducibility purposes. Please cite the associated dissertation when using this code or analysis.
