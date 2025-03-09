import pandas as pd
import numpy as np
import scipy.stats as stats
from count import add_cr_method_type
from utils import load_codebook

codebook = load_codebook('codebook.json')

def compute_standard_errors(p, n):
    """Calculate standard errors for a given proportion and sample size."""
    if pd.isna(p) or pd.isna(n) or n == 0:
        return np.nan
    p = np.clip(p, 0, 1)
    return np.sqrt(p * (1 - p) / n)

def compute_confidence_interval(p, n, alpha=0.05):
    """
    Compute the confidence interval for a proportion.
    """
    if pd.isna(p) or pd.isna(n) or n == 0:
        return np.nan, np.nan
    p = np.clip(p, 0, 1)
    z = stats.norm.ppf(1 - alpha / 2)
    se = compute_standard_errors(p, n)
    ci_lower = p - z * se
    ci_upper = p + z * se
    ci_lower = np.clip(ci_lower, 0, 1)
    ci_upper = np.clip(ci_upper, 0, 1)
    return round(ci_lower, 4), round(ci_upper, 4)

def compute_proportions(data, cr_total):
    """Compute proportion of CR total amount to sample size"""
    cr_proportion_total = data[cr_total] / data['sample_size']
    data['proportions_total'] = round(cr_proportion_total, 4)
    return data

# Helper function to add stats to dataframe
def add_stats_to_df(df, proportion_column):
    """Add standard error and confidence intervals to a dataframe."""
    if df.empty:
        # Return empty dataframe with added columns
        df['se'] = []
        df['ci_lower'] = []
        df['ci_upper'] = []
        return df
        
    df['se'] = df.apply(lambda row: compute_standard_errors(row[proportion_column], row['sample_size']), axis=1)
    
    # Calculate confidence intervals
    ci_values = df.apply(
        lambda row: compute_confidence_interval(row[proportion_column], row['sample_size']), axis=1)
    
    # Only unpack if we have values
    if len(ci_values) > 0:
        df['ci_lower'], df['ci_upper'] = zip(*ci_values)
    else:
        df['ci_lower'] = []
        df['ci_upper'] = []
        
    return df

def compute_proportions_by_year(data, year):
    """Compute proportion of CR for a given year"""
    subset_df = data[data['year'] == year].copy()
    subset_df['proportions_year'] = (subset_df['cr_total_amount'] / subset_df['sample_size']).round(4)
    subset_df['year'] = year
    subset_df = subset_df[['ID', 'year', 'sample_size', 'cr_total_amount', 'proportions_year']]
    return subset_df

def compute_proportions_by_journal(data, journal_code):
    """Compute proportion of CR for a given journal"""
    subset_df = data[data['journal_code'] == journal_code].copy()
    subset_df['proportions_journal'] = (subset_df['cr_total_amount'] / subset_df['sample_size']).round(4)
    subset_df['journal_name'] = codebook['journal'][journal_code]
    subset_df = subset_df[['ID', 'journal_code', 'journal_name', 'sample_size', 'cr_total_amount', 'proportions_journal']]
    return subset_df

def compute_proportions_by_sample_source(data, source_code):
    """Compute proportion of CR for a given sample source"""
    subset_df = data[data['sample_source'] == source_code].copy()
    subset_df['proportions_sample_source'] = (subset_df['cr_total_amount'] / subset_df['sample_size']).round(4)
    subset_df['sample_source_name'] = codebook['sample_source'][source_code]
    subset_df = subset_df[['ID', 'sample_source', 'sample_source_name', 'sample_size', 'cr_total_amount', 'proportions_sample_source']]
    return subset_df

def compute_proportions_by_sample_method(data, method_code):
    """Compute proportion of CR for a given sample method"""
    subset_df = data[data['sample_method'] == method_code].copy()
    subset_df['proportions_sample_method'] = (subset_df['cr_total_amount'] / subset_df['sample_size']).round(4)
    subset_df['sample_method_name'] = codebook['sample_method'][method_code]
    subset_df = subset_df[['ID', 'sample_method', 'sample_method_name', 'sample_size', 'cr_total_amount', 'proportions_sample_method']]
    return subset_df

def compute_proportions_by_sample_platform(data, platform_code):
    """Compute proportion of CR for a given sample platform"""
    subset_df = data[data['sample_platform'] == platform_code].copy()
    subset_df['proportions_platform'] = (subset_df['cr_total_amount'] / subset_df['sample_size']).round(4)
    subset_df['sample_platform_name'] = codebook['sample_platform'][platform_code]
    subset_df = subset_df[['ID', 'sample_platform', 'sample_platform_name', 'sample_size', 'cr_total_amount', 'proportions_platform']]
    return subset_df

def subset_data_by_cr_method(data, cr_method_code):
    """Subset data by CR method code"""
    subset_df = data[(data['cr_multiple'] == 0) & (data['cr_sequential'] == -1) | (data['cr_multiple'] == 1) & (data['cr_sequential'] == 1)]
    results = pd.DataFrame(columns=['ID', 'cr_method', 'cr_method_name', 'sample_size', 'cr_amount', 'cr_detail'])
    for _, row in subset_df.iterrows():
        for i in range(1, 5):
            if row[f'cr_{i}_method'] == cr_method_code:
                cr_amount = row[f'cr_{i}_amount']
                cr_method_detail = row[f'cr_{i}_method_detail']
                cr_method_name = codebook['cr_method'][cr_method_code]
                results = pd.concat([results, pd.DataFrame({
                    'ID': [row['ID']],
                    'sample_size': [row['sample_size']],
                    'cr_method': [cr_method_code],
                    'cr_amount': [cr_amount],
                    'cr_detail': [cr_method_detail],
                    'cr_method_name': [cr_method_name]
                })])
    return results

def compute_cr_method_proportions(data, cr_method_code):
    """Compute proportion of CR for a given CR method"""
    subset_df = subset_data_by_cr_method(data, cr_method_code)
    subset_df['proportions_cr_method'] = (subset_df['cr_amount'] / subset_df['sample_size']).apply(lambda x: round(x, 4))
    return subset_df

def subset_data_by_cr_method_type(data, cr_method_type):
    """Subset data by CR method type"""
    data = add_cr_method_type(data)
    subset_df = data[(data['cr_multiple'] == 0) & (data['cr_sequential'] == -1) | (data['cr_multiple'] == 1) & (data['cr_sequential'] == 1)]
    results = pd.DataFrame(columns=['ID', 'cr_method', 'cr_method_name', 'cr_method_type', 'sample_size', 'cr_amount', 'cr_detail'])
    for _, row in subset_df.iterrows():
        for i in range(1, 5):
            if row[f'cr_{i}_method_type'] == cr_method_type:
                cr_amount = row[f'cr_{i}_amount']
                cr_method_detail = row[f'cr_{i}_method_detail']
                cr_method_code = row[f'cr_{i}_method']
                cr_method_name = codebook['cr_method'][cr_method_code]
                results = pd.concat([results, pd.DataFrame({
                    'ID': [row['ID']],
                    'sample_size': [row['sample_size']],
                    'cr_method': [cr_method_code],
                    'cr_amount': [cr_amount],
                    'cr_detail': [cr_method_detail],
                    'cr_method_name': [cr_method_name],
                    'cr_method_type': [cr_method_type]
                })])
    return results

def compute_cr_method_type_proportions(data, cr_method_type):
    """Compute proportion of CR for a given CR method type"""
    subset_df = subset_data_by_cr_method_type(data, cr_method_type)
    subset_df['proportions_cr_type'] = (subset_df['cr_amount'] / subset_df['sample_size']).apply(lambda x: round(x, 4))
    return subset_df

def process_stats_for_group(process_func, data, items, dfs_dict, result_key):
    """Process statistics for a group of items and add to results dictionary"""
    group_dfs = []
    for item in items:
        try:
            df = process_func(data, item).copy()
            if not df.empty:
                # Get the proportion column name from the last column
                proportion_col = df.columns[-1]
                df = add_stats_to_df(df, proportion_col)
                group_dfs.append(df)
        except Exception as e:
            print(f"Error processing {result_key} with item {item}: {e}")
            continue
    
    if group_dfs:
        dfs_dict[result_key] = pd.concat(group_dfs)
    else:
        # Create empty DataFrame with the correct structure
        dfs_dict[result_key] = pd.DataFrame()
    return dfs_dict

def main():
    data = pd.read_csv('data/careless_data.csv')
    data['journal_code'] = data['journal'].map({v: k for k, v in codebook['journal'].items()})
    dfs = {}

    # Compute total cr proportions
    proportions_total_df = compute_proportions(data, 'cr_total_amount')[['ID', 'sample_size', 'cr_total_amount', 'proportions_total']].copy()
    proportions_total_df = add_stats_to_df(proportions_total_df, 'proportions_total')
    dfs['proportions_total'] = proportions_total_df

    # Define ranges
    years = range(2000, 2025)
    journal_codes = range(len(codebook['journal']))
    source_codes = range(len(codebook['sample_source']) - 1) #include minus one to exclude unspecified
    method_codes = range(len(codebook['sample_method']) - 1) #include minus one to exclude unspecified
    platform_codes = range(len(codebook['sample_platform']) - 1) #include minus one to exclude unspecified
    cr_method_codes = range(len(codebook['cr_method']) - 1) #include minus one to exclude unspecified
    cr_method_types = list(codebook['cr_method_type'].keys())

    # Process each group using the helper function
    dfs = process_stats_for_group(compute_proportions_by_year, data, years, dfs, 'proportions_year')
    dfs = process_stats_for_group(compute_proportions_by_journal, data, journal_codes, dfs, 'proportions_journal')
    dfs = process_stats_for_group(compute_proportions_by_sample_source, data, source_codes, dfs, 'proportions_sample_source')
    dfs = process_stats_for_group(compute_proportions_by_sample_method, data, method_codes, dfs, 'proportions_sample_method')
    dfs = process_stats_for_group(compute_proportions_by_sample_platform, data, platform_codes, dfs, 'proportions_platform')
    dfs = process_stats_for_group(compute_cr_method_proportions, data, cr_method_codes, dfs, 'proportions_cr_method')
    dfs = process_stats_for_group(compute_cr_method_type_proportions, data, cr_method_types, dfs, 'proportions_cr_type')

    # Write results to excel
    with pd.ExcelWriter('output/proportions.xlsx') as writer:
        for name, df in dfs.items():
            df.to_excel(writer, sheet_name=name, index=False)

if __name__ == '__main__':
    main()