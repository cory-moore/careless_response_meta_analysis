import math
import pandas as pd
import numpy as np
from scipy.stats import norm


def calculate_logit_transform(p):
    """Takes a proportion p as input and calculates the logit transformation of the proportion."""
    p = np.clip(p, 1e-5, 1 - 1e-5)    
    return np.log(p / (1 - p))

def calculate_logit_variance(p, n):
    """Takes a raw proportion p and a sample size n and calculates the variance of the logit transformed proportion."""
    p = np.clip(p, 1e-5, 1 - 1e-5)
    return 1 / (n * p) + 1 / (n * (1 - p))

def calculate_pooled_prevalence(logit_p, n, p):
    """Calculates the pooled prevalence using inverse variance weighting"""
    weights = 1 / calculate_logit_variance(p, n)
    numerator = (logit_p * weights).sum()
    denominator = weights.sum()
    pooled_p = numerator / denominator
    return pooled_p

def back_transform(pooled_p):
    """Back transforms a logit value to a proportion"""
    back_transformed_p = np.exp(pooled_p) / (1 + np.exp(pooled_p))
    return round(back_transformed_p, 4)

def calculate_pooled_standard_error(p_values, n_values):
    """Calculates the pooled standard error"""
    weights = [1 / calculate_logit_variance(p, n) for p, n in zip(p_values, n_values)]
    pooled_var = 1 / sum(weights)
    return round(math.sqrt(pooled_var), 4)

def calculate_pooled_confidence_interval(pooled_p, pooled_se, alpha=0.05):
    """Calculates the confidence interval in logit scale"""
    z_crit = norm.ppf(1 - alpha / 2)
    me = z_crit * pooled_se
    return pooled_p - me, pooled_p + me

def back_transform_confidence_interval(lower_bound, upper_bound):
    """Back transforms logit confidence bounds to proportions"""
    lower_ci = np.exp(lower_bound) / (1 + np.exp(lower_bound))
    upper_ci = np.exp(upper_bound) / (1 + np.exp(upper_bound))
    return round(lower_ci, 4), round(upper_ci, 4)

def get_pooled_statistics(p, n):
    """Calculates all pooled statistics for a set of proportions and sample sizes"""
    logit_p = calculate_logit_transform(p)
    pooled_p_logit = calculate_pooled_prevalence(logit_p, n, p)
    pooled_p = back_transform(pooled_p_logit)
    pooled_se = calculate_pooled_standard_error(p, n)
    lower_bound, upper_bound = calculate_pooled_confidence_interval(pooled_p_logit, pooled_se)
    lower_ci, upper_ci = back_transform_confidence_interval(lower_bound, upper_bound)
    return pooled_p, pooled_se, lower_ci, upper_ci, sum(n)

def process_group(sheet_name, group_name, subgroup_col, proportion_col, results_df):
    """Process a group of proportions and add results to the results DataFrame"""
    df = pd.read_excel('output/proportions.xlsx', sheet_name=sheet_name)
    
    # Add total row for the group if needed
    if len(df) > 0:
        p = df[proportion_col]
        n = df['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        results_df.loc[len(results_df)] = [
            group_name, 'Total', pooled_p, pooled_se, 
            lower_ci, upper_ci, n_sum, len(df)
        ]
    
    # Process each subgroup
    for subgroup in df[subgroup_col].unique():
        subset = df[df[subgroup_col] == subgroup]
        if len(subset) > 0:
            p = subset[proportion_col]
            n = subset['sample_size']
            pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
            results_df.loc[len(results_df)] = [
                group_name, subgroup, pooled_p, pooled_se, 
                lower_ci, upper_ci, n_sum, len(subset)
            ]
    
    return results_df

def main():

    columns = [
        'Group', 'Subgroup', 'Pooled Prevalence', 'Pooled Standard Error',
        'Lower CI', 'Upper CI', 'Pooled Sample Size', 'Number of Studies'
    ]
    pooled_results = pd.DataFrame(columns=columns)
    
    proportions_total = pd.read_excel('output/proportions.xlsx', sheet_name='proportions_total')
    p = proportions_total['proportions_total']
    n = proportions_total['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    pooled_results.loc[len(pooled_results)] = [
        'Total', '', pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_total)
    ]
    
    groups = [
        {'sheet': 'proportions_year', 'name': 'Year', 'col': 'year', 'prop_col': 'proportions_year'},
        {'sheet': 'proportions_journal', 'name': 'Journal', 'col': 'journal_name', 'prop_col': 'proportions_journal'},
        {'sheet': 'proportions_sample_source', 'name': 'Sample Source', 'col': 'sample_source_name', 'prop_col': 'proportions_sample_source'},
        {'sheet': 'proportions_sample_method', 'name': 'Sample Method', 'col': 'sample_method_name', 'prop_col': 'proportions_sample_method'},
        {'sheet': 'proportions_platform', 'name': 'Sample Platform', 'col': 'sample_platform_name', 'prop_col': 'proportions_platform'},
        {'sheet': 'proportions_cr_method', 'name': 'Careless Response Method', 'col': 'cr_method_name', 'prop_col': 'proportions_cr_method'},
        {'sheet': 'proportions_cr_type', 'name': 'Careless Response Type', 'col': 'cr_method_type', 'prop_col': 'proportions_cr_type'}
    ]
    
    for group in groups:
        pooled_results = process_group(
            group['sheet'], group['name'], group['col'], group['prop_col'], pooled_results
        )
    
    pooled_results.to_csv('output/fixed_effects_pooled_proportions.csv', index=False)
    
if __name__ == '__main__':
    main()