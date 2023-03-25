
import pandas as pd
import matplotlib.pyplot as plt
from scripts.codebook import codebook
from scripts.util import add_cr_method_type

#----- Functions ------------------------------------------------------------------------------------------------------------------
def compute_rates(data, cr_total):
    cr_rate_total = data[cr_total] / data['sample_size']
    data['cr_rate_total'] = cr_rate_total
    return data

def compute_rates_by_year(data, year):
    subset_df = data[data['year'] == year].copy()
    subset_df['cr_rate_year'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['year'] = year
    subset_df = subset_df[['ID', 'year', 'sample_size', 'cr_total_amount', 'cr_rate_year']]
    return subset_df

def compute_rates_by_journal(data, journal_code):
    subset_df = data[data['journal_code'] == journal_code].copy()
    subset_df['cr_rate_journal'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['journal_name'] = codebook['journal'][journal_code]
    subset_df = subset_df[['ID', 'journal_code', 'journal_name', 'sample_size', 'cr_total_amount', 'cr_rate_journal']]
    return subset_df

def compute_rates_by_sample_source(data, source_code):
    subset_df = data[data['sample_source'] == source_code].copy()
    subset_df['cr_rate_source'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['sample_source_name'] = codebook['sample_source'][source_code]
    subset_df = subset_df[['ID', 'sample_source', 'sample_source_name', 'sample_size', 'cr_total_amount', 'cr_rate_source']]
    return subset_df

def compute_rates_by_sample_method(data, method_code):
    subset_df = data[data['sample_method'] == method_code].copy()
    subset_df['cr_rate_method'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['sample_method_name'] = codebook['sample_method'][method_code]
    subset_df = subset_df[['ID', 'sample_method', 'sample_method_name', 'sample_size', 'cr_total_amount', 'cr_rate_method']]
    return subset_df

def compute_rates_by_sample_platform(data, platform_code):
    try:
        platform_name = codebook['sample_platform'][platform_code]
    except KeyError:
        platform_name = None
        return None # return None if the platform_code is not found in the codebook
    
    subset_df = data[data['sample_platform'] == platform_code].copy()
    subset_df['cr_rate_platform'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['sample_platform_name'] = codebook['sample_platform'][platform_code]
    subset_df = subset_df[['ID', 'sample_platform', 'sample_platform_name', 'sample_size', 'cr_total_amount', 'cr_rate_platform']]
    return subset_df

def subset_data_by_cr_method(data, cr_method_code):
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

def compute_cr_method_rates(data, cr_method_code):
    subset_df = subset_data_by_cr_method(data, cr_method_code)
    subset_df['cr_rate'] = subset_df['cr_amount'] / subset_df['sample_size']
    return subset_df

def subset_data_by_cr_method_type(data, cr_method_type):
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

def compute_cr_method_type_rates(data, cr_method_type):
    subset_df = subset_data_by_cr_method_type(data, cr_method_type)
    subset_df['cr_rate'] = subset_df['cr_amount'] / subset_df['sample_size']
    return subset_df


#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_csv('data/prelim_careless_data.csv')
data['journal_code'] = data['journal'].map(codebook['journal_code'])

# compute total cr rates
cr_rates_total = compute_rates(data, 'cr_total_amount')
cr_rates_total = cr_rates_total[['ID', 'sample_size', 'cr_total_amount', 'cr_rate_total']]

# compute cr rates by various groupings
years = range(2000, 2023)
jounal_codes = range(24)
source_codes = range(4)
method_codes = range(4)
platform_codes = range(7)
cr_method_codes = range(12)
cr_method_types = ['response_time', 'outlier_analysis', 'bogus_items', 'consistency_indices', 'response_pattern', 'self_reported']

years_df = [compute_rates_by_year(data, year) for year in years]
rates_by_year = pd.concat(years_df)

journal_dfs = [compute_rates_by_journal(data, code) for code in jounal_codes]
rates_by_journal = pd.concat(journal_dfs)

source_dfs = [compute_rates_by_sample_source(data, code) for code in source_codes]
rates_by_source = pd.concat(source_dfs)

method_dfs = [compute_rates_by_sample_method(data, code) for code in method_codes]
rates_by_method = pd.concat(method_dfs)

platform_dfs = [compute_rates_by_sample_platform(data, code) for code in platform_codes]
rates_by_platform = pd.concat(platform_dfs)

cr_methods_dfs = [compute_cr_method_rates(data, code) for code in cr_method_codes]
rates_by_cr_method = pd.concat(cr_methods_dfs)

cr_type_dfs = [compute_cr_method_type_rates(data, code) for code in cr_method_types]
rates_by_cr_type = pd.concat(cr_type_dfs)


#----- Export ----------------------------------------------------------------------------------------------------------------------
writer = pd.ExcelWriter('results/careless_response_rates.xlsx')

cr_rates_total.to_excel(writer, 'cr_rates_total', index=False)
rates_by_year.to_excel(writer, 'cr_rates_by_year', index=False)
rates_by_journal.to_excel(writer, 'cr_rates_by_journal', index=False)
rates_by_source.to_excel(writer, 'cr_rates_by_sample_source', index=False)
rates_by_method.to_excel(writer, 'cr_rates_by_sample_method', index=False)
rates_by_platform.to_excel(writer, 'cr_rates_by_sample_platform', index=False)
rates_by_cr_method.to_excel(writer, 'cr_rates_by_method', index=False)
rates_by_cr_type.to_excel(writer, 'cr_rates_by_method_type', index=False)

writer.save()

