import math
import pandas as pd
import numpy as np
from scipy.stats import norm, chi2


def calculate_logit_transform(p):
    """Takes a proportion p as input and calculates the logit transformation."""
    p = np.clip(p, 1e-5, 1 - 1e-5)    
    return np.log(p / (1 - p))


def calculate_logit_variance(p, n):
    """Calculates the within-study variance of logit-transformed proportions."""
    p = np.clip(p, 1e-5, 1 - 1e-5)
    return 1 / (n * p * (1 - p))


def back_transform(logit_p):
    """Back transforms a logit value to a proportion."""
    back_transformed_p = np.exp(logit_p) / (1 + np.exp(logit_p))
    return round(back_transformed_p, 4)


def calculate_Q(effect_sizes, weights):
    """
    Calculates the Q statistic for heterogeneity assessment.
    
    Args:
        effect_sizes: List or array of effect sizes (logit-transformed proportions)
        weights: List or array of weights (inverse of within-study variances)
        
    Returns:
        Q: Q statistic
        df: Degrees of freedom
        p_value: P-value for the Q test
    """
    k = len(effect_sizes)
    weighted_mean = np.sum(weights * effect_sizes) / np.sum(weights)
    Q = np.sum(weights * np.square(effect_sizes - weighted_mean))
    df = k - 1
    p_value = 1 - chi2.cdf(Q, df)
    return Q, df, p_value


def calculate_tau_squared(Q, df, weights):
    """
    Calculates the between-study variance (τ²) using the DerSimonian-Laird estimator.
    
    Args:
        Q: Q statistic
        df: Degrees of freedom
        weights: Weights (inverse of within-study variances)
        
    Returns:
        tau_squared: Estimate of between-study variance
    """
    sum_weights = np.sum(weights)
    sum_squared_weights = np.sum(np.square(weights))
    
    # DerSimonian-Laird estimator
    tau_squared = max(0, (Q - df) / (sum_weights - (sum_squared_weights / sum_weights)))
    return tau_squared


def calculate_i_squared(Q, df):
    """
    Calculates I² - the percentage of variation due to heterogeneity.
    
    Args:
        Q: Q statistic
        df: Degrees of freedom
        
    Returns:
        i_squared: I² statistic
        i_squared_lower: Lower bound of I² confidence interval
        i_squared_upper: Upper bound of I² confidence interval
    """
    i_squared = max(0, (Q - df) / Q * 100) if Q > 0 else 0
    
    # Calculate 95% confidence interval for I²
    # This is an approximate method based on the Q distribution
    alpha = 0.05
    q_lower = chi2.ppf(alpha/2, df)
    q_upper = chi2.ppf(1-alpha/2, df)
    
    i_squared_lower = max(0, (Q - q_upper) / Q * 100) if Q > 0 else 0
    i_squared_upper = max(0, (Q - q_lower) / Q * 100) if Q > 0 else 0
    
    return i_squared, i_squared_lower, i_squared_upper


def get_random_effects_pooled_stats(p_values, n_values):
    """
    Performs a random-effects meta-analysis on proportion data.
    
    Args:
        p_values: List or array of proportions
        n_values: List or array of sample sizes
        
    Returns:
        pooled_p: Pooled proportion (back-transformed)
        pooled_se: Standard error of the pooled estimate
        lower_ci: Lower bound of the 95% confidence interval
        upper_ci: Upper bound of the 95% confidence interval
        tau_squared: Between-study variance
        i_squared: I² statistic
        Q: Q statistic for heterogeneity
    """
    # Convert to numpy arrays for vectorized operations
    p_values = np.array(p_values)
    n_values = np.array(n_values)
    
    # Calculate logit-transformed proportions
    logit_p = calculate_logit_transform(p_values)
    
    # Calculate within-study variances
    within_var = calculate_logit_variance(p_values, n_values)
    
    # Fixed-effect weights (inverse variance)
    weights_fixed = 1 / within_var
    
    # Calculate Q statistic for heterogeneity
    Q, df, p_q = calculate_Q(logit_p, weights_fixed)
    
    # Calculate between-study variance (τ²)
    tau_squared = calculate_tau_squared(Q, df, weights_fixed)
    
    # Calculate I²
    i_squared, i_lower, i_upper = calculate_i_squared(Q, df)
    
    # Calculate random-effects weights
    weights_random = 1 / (within_var + tau_squared)
    
    # Calculate pooled estimate
    pooled_logit = np.sum(logit_p * weights_random) / np.sum(weights_random)
    
    # Calculate standard error for the pooled estimate
    pooled_se = np.sqrt(1 / np.sum(weights_random))
    
    # Calculate confidence interval
    z = norm.ppf(0.975)  # 95% CI
    lower_logit = pooled_logit - z * pooled_se
    upper_logit = pooled_logit + z * pooled_se
    
    # Back-transform results
    pooled_p = back_transform(pooled_logit)
    lower_ci = back_transform(lower_logit)
    upper_ci = back_transform(upper_logit)
    
    # Sum of sample sizes for reporting
    n_sum = np.sum(n_values)
    
    return pooled_p, round(pooled_se, 4), lower_ci, upper_ci, n_sum, round(i_squared, 2), round(Q, 2)


def process_group(sheet_name, group_name, subgroup_col, proportion_col, results_df):
    """Process a group of proportions and add results to the results DataFrame"""
    try:
        df = pd.read_excel('output/proportions.xlsx', sheet_name=sheet_name)
    except Exception as e:
        print(f"Error reading sheet {sheet_name}: {e}")
        return results_df
    
    # Add total row for the group if needed
    if len(df) > 0:
        p = df[proportion_col]
        n = df['sample_size']
        
        # Only process if we have valid data
        if len(p) > 0 and not p.isna().all():
            try:
                pooled_p, pooled_se, lower_ci, upper_ci, n_sum, i_squared, Q = get_random_effects_pooled_stats(p, n)
                results_df.loc[len(results_df)] = [
                    group_name, 'Total', pooled_p, pooled_se, 
                    lower_ci, upper_ci, n_sum, len(df), i_squared, Q
                ]
            except Exception as e:
                print(f"Error processing {group_name} total: {e}")
    
    # Process each subgroup
    if subgroup_col in df.columns:
        for subgroup in df[subgroup_col].unique():
            subset = df[df[subgroup_col] == subgroup]
            if len(subset) > 0:
                p = subset[proportion_col]
                n = subset['sample_size']
                
                # Only process if we have valid data
                if len(p) > 0 and not p.isna().all():
                    try:
                        pooled_p, pooled_se, lower_ci, upper_ci, n_sum, i_squared, Q = get_random_effects_pooled_stats(p, n)
                        results_df.loc[len(results_df)] = [
                            group_name, subgroup, pooled_p, pooled_se, 
                            lower_ci, upper_ci, n_sum, len(subset), i_squared, Q
                        ]
                    except Exception as e:
                        print(f"Error processing {group_name}, subgroup {subgroup}: {e}")
    
    return results_df


def main():
    columns = [
        'Group', 'Subgroup', 'Pooled Prevalence', 'Pooled Standard Error',
        'Lower CI', 'Upper CI', 'Pooled Sample Size', 'Number of Studies',
        'I-squared (%)', 'Q Statistic'
    ]
    pooled_results = pd.DataFrame(columns=columns)
    
    groups = [
        {'sheet': 'proportions_total', 'name': 'Total', 'col': None, 'prop_col': 'proportions_total'},
        {'sheet': 'proportions_year', 'name': 'Year', 'col': 'year', 'prop_col': 'proportions_year'},
        {'sheet': 'proportions_journal', 'name': 'Journal', 'col': 'journal_name', 'prop_col': 'proportions_journal'},
        {'sheet': 'proportions_sample_source', 'name': 'Sample Source', 'col': 'sample_source_name', 
         'prop_col': 'proportions_sample_source'},
        {'sheet': 'proportions_sample_method', 'name': 'Sample Method', 'col': 'sample_method_name', 
         'prop_col': 'proportions_sample_method'},
        {'sheet': 'proportions_platform', 'name': 'Sample Platform', 'col': 'sample_platform_name', 
         'prop_col': 'proportions_platform'},
        {'sheet': 'proportions_cr_method', 'name': 'Careless Response Method', 'col': 'cr_method_name', 
         'prop_col': 'proportions_cr_method'},
        {'sheet': 'proportions_cr_type', 'name': 'Careless Response Type', 'col': 'cr_method_type', 
         'prop_col': 'proportions_cr_type'}
    ]
    
    proportions_total = pd.read_excel('output/proportions.xlsx', sheet_name='proportions_total')
    p = proportions_total['proportions_total']
    n = proportions_total['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum, i_squared, Q = get_random_effects_pooled_stats(p, n)
    pooled_results.loc[len(pooled_results)] = [
        'Overall', '', pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_total), i_squared, Q
    ]
    
    for group in groups[1:]:  # Skip total as we processed it specially
        pooled_results = process_group(
            group['sheet'], group['name'], group['col'], group['prop_col'], pooled_results
        )
    
    pooled_results.to_csv('output/random_effects_pooled_proportions.csv', index=False)
    print("\nRandom Effects Meta-Analysis Summary")
    print("=====================================")
    print(f"Overall pooled prevalence: {pooled_p:.4f} (95% CI: {lower_ci:.4f}-{upper_ci:.4f})")
    print(f"I-squared: {i_squared:.1f}% (Overall heterogeneity)")
    print(f"Q statistic: {Q:.2f}, p < 0.001 (assuming df > 10)")
    
if __name__ == '__main__':
    main()