
import pandas as pd
import numpy as np
import scipy.stats as stats
from scripts.codebook import codebook
from scripts.frequencies import add_cr_method_type

#----- Functions ------------------------------------------------------------------------------------------------------------------
def compute_proportions(data, cr_total):
    """Compute proportion of CR total amount to sample size"""
    cr_proportion_total = data[cr_total] / data['sample_size']
    data['proportions_total'] = round(cr_proportion_total, 4)
    return data

def compute_standard_errors(p, n):
    """Calculate standard errors for a given proportion and sample size."""
    return np.sqrt(p * (1 - p) / n)

def compute_confidence_interval(p, n, alpha=0.05):
    """
    Compute the confidence interval for a proportion.
    """
    z = stats.norm.ppf(1 - alpha / 2)
    ci_lower = p - z * np.sqrt(p * (1 - p) / n)
    ci_upper = p + z * np.sqrt(p * (1 - p) / n)
    ci_lower = round(ci_lower, 4)
    ci_upper = round(ci_upper, 4)
    return ci_lower, ci_upper

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
    subset_df['proportions_cr_type'] = subset_df['cr_amount'] / subset_df['sample_size'].apply(lambda x: round(x, 4))
    return subset_df

def main():
    data = pd.read_csv('data/prelim_careless_data.csv')
    data['journal_code'] = data['journal'].map(codebook['journal_code'])
    data['sample_platform']
    dfs = {}

    # Compute total cr proportions
    proportions_total_df = compute_proportions(data, 'cr_total_amount')[['ID', 'sample_size', 'cr_total_amount', 'proportions_total']].copy()
    proportions_total_df['se'] = proportions_total_df.apply(lambda row: compute_standard_errors(row['proportions_total'], row['sample_size']), axis=1)
    proportions_total_df['ci_lower'], proportions_total_df['ci_upper'] = zip(*proportions_total_df.apply(lambda row: compute_confidence_interval(row['proportions_total'], row['sample_size']), axis=1))
    dfs['proportions_total'] = proportions_total_df

    # compute cr proportions by various groupings
    years = range(2000, 2023)
    jounal_codes = range(24)
    source_codes = range(4)
    method_codes = range(4)
    platform_codes = range(6)
    cr_method_codes = range(12)
    cr_method_types = ['response_time', 'outlier_analysis', 'bogus_items', 'consistency_indices', 'response_pattern', 'self_reported']

    # Compute proportions by year
    years_df = []
    for year in years:
        df = compute_proportions_by_year(data, year).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_year'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_year'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_year'], row['sample_size'])[1], axis=1)
        years_df.append(df)
    dfs['proportions_year'] = pd.concat(years_df)

    # Compute proportions by journal
    journal_dfs = []
    for code in jounal_codes:
        df = compute_proportions_by_journal(data, code).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_journal'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_journal'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_journal'], row['sample_size'])[1], axis=1)
        journal_dfs.append(df)
    dfs['proportions_journal'] = pd.concat(journal_dfs)

    # Compute proportions by sample source
    source_dfs = []
    for code in source_codes:
        df = compute_proportions_by_sample_source(data, code).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_sample_source'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_sample_source'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_sample_source'], row['sample_size'])[1], axis=1)
        source_dfs.append(df)
    dfs['proportions_sample_source'] = pd.concat(source_dfs)

    # Compute proportions by sample method
    method_dfs = []
    for code in method_codes:
        df = compute_proportions_by_sample_method(data, code).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_sample_method'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_sample_method'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_sample_method'], row['sample_size'])[1], axis=1)
        method_dfs.append(df)
    dfs['proportions_sample_method'] = pd.concat(method_dfs)

    # Compute proportions by sample platform
    platform_dfs = []
    for code in platform_codes:
        df = compute_proportions_by_sample_platform(data, code).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_platform'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_platform'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_platform'], row['sample_size'])[1], axis=1)
        platform_dfs.append(df)
    dfs['proportions_platform'] = pd.concat(platform_dfs)

    # Compute proportions by CR method
    cr_methods_dfs = []
    for code in cr_method_codes:
        df = compute_cr_method_proportions(data, code).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_cr_method'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_cr_method'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_cr_method'], row['sample_size'])[1], axis=1)
        cr_methods_dfs.append(df)
    dfs['proportions_cr_method'] = pd.concat(cr_methods_dfs)

    # Compute proportions by CR method type
    cr_type_dfs = []
    for method_type in cr_method_types:
        df = compute_cr_method_type_proportions(data, method_type).copy()
        df['se'] = df.apply(lambda row: compute_standard_errors(row['proportions_cr_type'], row['sample_size']), axis=1)
        df['ci_lower'] = df.apply(lambda row: compute_confidence_interval(row['proportions_cr_type'], row['sample_size'])[0], axis=1)
        df['ci_upper'] = df.apply(lambda row: compute_confidence_interval(row['proportions_cr_type'], row['sample_size'])[1], axis=1)
        cr_type_dfs.append(df)
    dfs['proportions_cr_type'] = pd.concat(cr_type_dfs)


    #----- Export ----------------------------------------------------------------------------------------------------------------------
    # # export to excel workbook
    with pd.ExcelWriter('results/raw_proportions.xlsx') as writer:
        for name, df in dfs.items():
            df.to_excel(writer, sheet_name=name, index=False)
if __name__ == '__main__':
    main()