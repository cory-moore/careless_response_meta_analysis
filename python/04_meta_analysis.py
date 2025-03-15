import numpy as np
import pandas as pd
import os
from scipy.stats import norm
import json

def logit_transform(p):
    """Transform proportions to logit scale"""
    p = np.clip(p, 0.00001, 0.99999)
    return np.log(p / (1 - p))

def logit_variance(p, n):
    """Calculate variance for logit-transformed proportions"""
    p = np.clip(p, 0.00001, 0.99999)
    return 1 / (n * p * (1 - p))

def back_transform(logit):
    """Convert logit values back to proportions"""
    return np.exp(logit) / (1 + np.exp(logit))

def calculate_Q(y, w):
    """Calculate Q statistic for heterogeneity
    
    Parameters:
    y - logit-transformed proportions
    w - inverse variance weights
    
    Returns:
    Q statistic, degrees of freedom
    """
    weighted_mean = np.sum(w * y) / np.sum(w)
    Q = np.sum(w * (y - weighted_mean)**2)
    df = len(y) - 1
    return Q, df

def calculate_tau_squared(Q, df, w):
    """Calculate between-study variance using DerSimonian-Laird method
    
    Parameters:
    Q - Q statistic for heterogeneity
    df - degrees of freedom
    w - inverse variance weights (fixed-effects)
    
    Returns:
    tau² (between-study variance)
    """
    sum_w = np.sum(w)
    sum_w2 = np.sum(w**2)
    denominator = sum_w - (sum_w2 / sum_w)
    if denominator <= 0:
        return 0
    return max(0, (Q - df) / denominator)

def calculate_I_squared(Q, df):
    """Calculate I² heterogeneity statistic
    
    Parameters:
    Q - Q statistic
    df - degrees of freedom
    
    Returns:
    I² as percentage (0-100%)
    """
    if Q <= df:
        return 0
    return 100 * (Q - df) / Q

def random_effects_meta(p, n):
    """Run random-effects meta-analysis on proportion data
    
    Parameters:
    p - array of proportions
    n - array of sample sizes
    
    Returns:
    dictionary with meta-analysis results
    """
    mask = ~np.isnan(p) & ~np.isnan(n) & (n > 0)
    p, n = p[mask], n[mask]    
    y = logit_transform(p)
    v = logit_variance(p, n)
    w = 1 / v
    Q, df = calculate_Q(y, w)
    tau2 = calculate_tau_squared(Q, df, w)
    I2 = calculate_I_squared(Q, df)
    w_random = 1 / (v + tau2)
    pooled_logit = np.sum(y * w_random) / np.sum(w_random)
    se = np.sqrt(1 / np.sum(w_random))
    ci = pooled_logit + np.array([-1, 1]) * 1.96 * se
    results = {
        'p_hat': float(back_transform(pooled_logit)),
        'ci_lower': float(back_transform(ci[0])),
        'ci_upper': float(back_transform(ci[1])),
        'k': int(len(p)),
        'n': int(np.sum(n)),
        'i2': float(I2),
        'tau2': float(tau2),
        'Q': float(Q)
    }
    return results

def analyze_dataset(data_path, analysis_type):
    """Analyze a dataset using random-effects meta-analysis
    
    Parameters:
    data_path - path to CSV file with individual study data
    analysis_type - string identifier for the analysis approach
    
    Returns:
    dictionary with meta-analysis results
    """
    print(f"\nRunning meta-analysis for {analysis_type.upper()} dataset:")
    results = {}
    data = pd.read_csv(data_path)
    print(f"  Loaded {len(data)} studies from {data_path}")
    
    prop_col = 'proportion' if 'proportion' in data.columns else 'raw_proportion'
    if prop_col not in data.columns:
        print(f"  Error: Could not find proportion column in {data_path}")
        return {}
    
    overall_meta = random_effects_meta(data[prop_col].values, data['sample_size'].values)
    results['overall'] = overall_meta
    
    dimensions = []
    for col in data.columns:
        if col.endswith('_name') or col in ['year', 'method_type', 'method_position']:
            dimensions.append(col)
    
    # Map dimension columns to their name columns for subgroup analysis
    dimension_name_mappings = {
        'journal': 'journal_name',
        'sample_source': 'sample_source_name',
        'sample_recruitment': 'sample_recruitment_name',
        'sample_method': 'sample_method_name',
        'sample_platform': 'sample_platform_name',
        'sample_level': 'sample_level_name',
        'sample_incentive': 'sample_incentive_name',
        'sample_country': 'sample_country_name',
        'cr_method': 'cr_method_name',
        'design_method': 'design_method_name',
        'design_location': 'design_location_name'
        # Add other dimensions as needed
    }
    
    for dim in dimensions:
        dim_results = {}
        dim_name = dim.replace('_name', '')
        
        if data[dim].isna().sum() > len(data) * 0.5:
            print(f"  Skipping dimension {dim} due to too many missing values")
            continue
        
        values = data[dim].dropna().unique()
        if len(values) <= 1:
            print(f"  Skipping dimension {dim} with only {len(values)} unique values")
            continue
        
        print(f"  Analyzing dimension: {dim} ({len(values)} categories)")
        
        categories = []
        for val in values:
            subset = data[data[dim] == val]
            if len(subset) < 2:
                print(f"    Skipping {dim}={val} with only {len(subset)} studies")
                continue
                
            subgroup_meta = random_effects_meta(subset[prop_col].values, subset['sample_size'].values)
            
            categories.append({
                'name': str(val),
                'p_hat': subgroup_meta['p_hat'],
                'ci_lower': subgroup_meta['ci_lower'],
                'ci_upper': subgroup_meta['ci_upper'],
                'k': subgroup_meta['k'],
                'n': subgroup_meta['n'],
                'i2': subgroup_meta['i2'],
                'tau2': subgroup_meta['tau2'],
                'Q': subgroup_meta['Q']
            })
        
        dim_results = {
            'dimension': dim_name,
            'categories': categories
        }
        
        results[dim_name] = dim_results
    
    return results

def save_results(results, output_path):
    """Save meta-analysis results to JSON file"""
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"  Saved results to {output_path}")

def main():
    """Run meta-analyses for primary and secondary approaches"""    
    print("\nMETA-ANALYSIS OF CARELESS RESPONDING")
    print("==================================")
    
    # First-Method approach (PRIMARY ANALYSIS)
    first_method_path = "data/for_meta/first_method_data.csv"
    results = analyze_dataset(first_method_path, 'first-method')
    save_results(results, "output/python_results/first_method_results.json")
    
    # Single-Method approach (SECONDARY ANALYSIS)
    single_method_path = "data/for_meta/single_method_data.csv"
    results = analyze_dataset(single_method_path, 'single-method')
    save_results(results, "output/python_results/single_method_results.json")
    
    # Overall meta analysis
    overall_path = "data/for_meta/overall_data.csv"
    results = analyze_dataset(overall_path, 'overall')
    save_results(results, "output/python_results/overall_results.json")
    
    print("\nMeta-analysis complete.")
if __name__ == "__main__":
    main()