
import pandas as pd
from scripts.codebook import codebook
from scripts.frequencies import add_cr_method_type

#----- Functions ------------------------------------------------------------------------------------------------------------------
def compute_proportions(data, cr_total):
    """Compute proportion of CR total amount to sample size"""
    cr_proportion_total = data[cr_total] / data['sample_size']
    data['proportions_total'] = cr_proportion_total
    return data

def compute_proportions_by_year(data, year):
    """Compute proportion of CR for a given year"""
    subset_df = data[data['year'] == year].copy()
    subset_df['proportions_year'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['year'] = year
    subset_df = subset_df[['ID', 'year', 'sample_size', 'cr_total_amount', 'proportions_year']]
    return subset_df

def compute_proportions_by_journal(data, journal_code):
    """Compute proportion of CR for a given journal"""
    subset_df = data[data['journal_code'] == journal_code].copy()
    subset_df['proportions_journal'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['journal_name'] = codebook['journal'][journal_code]
    subset_df = subset_df[['ID', 'journal_code', 'journal_name', 'sample_size', 'cr_total_amount', 'proportions_journal']]
    return subset_df

def compute_proportions_by_sample_source(data, source_code):
    """Compute proportion of CR for a given sample source"""
    subset_df = data[data['sample_source'] == source_code].copy()
    subset_df['proportions_sample_source'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['sample_source_name'] = codebook['sample_source'][source_code]
    subset_df = subset_df[['ID', 'sample_source', 'sample_source_name', 'sample_size', 'cr_total_amount', 'proportions_sample_source']]
    return subset_df

def compute_proportions_by_sample_method(data, method_code):
    """Compute proportion of CR for a given sample method"""
    subset_df = data[data['sample_method'] == method_code].copy()
    subset_df['proportions_sample_method'] = subset_df['cr_total_amount'] / subset_df['sample_size']
    subset_df['sample_method_name'] = codebook['sample_method'][method_code]
    subset_df = subset_df[['ID', 'sample_method', 'sample_method_name', 'sample_size', 'cr_total_amount', 'proportions_sample_method']]
    return subset_df

def compute_proportions_by_sample_platform(data, platform_code):
    """Compute proportion of CR for a given sample platform"""
    try:
        platform_name = codebook['sample_platform'][platform_code]
    except KeyError:
        platform_name = None
        return None # return None if the platform_code is not found in the codebook
    subset_df = data[data['sample_platform'] == platform_code].copy()
    subset_df['proportions_platform'] = subset_df['cr_total_amount'] / subset_df['sample_size']
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
    subset_df['proportions_cr_method'] = subset_df['cr_amount'] / subset_df['sample_size']
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
    subset_df['proportions_cr_type'] = subset_df['cr_amount'] / subset_df['sample_size']
    return subset_df


#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_csv('data/prelim_careless_data.csv')
data['journal_code'] = data['journal'].map(codebook['journal_code'])

# compute total cr proportions
cr_proportions_total = compute_proportions(data, 'cr_total_amount')
cr_proportions_total = cr_proportions_total[['ID', 'sample_size', 'cr_total_amount', 'proportions_total']]

# compute cr proportions by various groupings
years = range(2000, 2023)
jounal_codes = range(24)
source_codes = range(4)
method_codes = range(4)
platform_codes = range(7)
cr_method_codes = range(12)
cr_method_types = ['response_time', 'outlier_analysis', 'bogus_items', 'consistency_indices', 'response_pattern', 'self_reported']

years_df = [compute_proportions_by_year(data, year) for year in years]
proportions_year = pd.concat(years_df)

journal_dfs = [compute_proportions_by_journal(data, code) for code in jounal_codes]
proportions_journal = pd.concat(journal_dfs)

source_dfs = [compute_proportions_by_sample_source(data, code) for code in source_codes]
proportions_sample_source = pd.concat(source_dfs)

method_dfs = [compute_proportions_by_sample_method(data, code) for code in method_codes]
proportions_sample_method = pd.concat(method_dfs)

platform_dfs = [compute_proportions_by_sample_platform(data, code) for code in platform_codes]
proportions_platform = pd.concat(platform_dfs)

cr_methods_dfs = [compute_cr_method_proportions(data, code) for code in cr_method_codes]
proportions_cr_method = pd.concat(cr_methods_dfs)

cr_type_dfs = [compute_cr_method_type_proportions(data, code) for code in cr_method_types]
proportions_cr_type = pd.concat(cr_type_dfs)


#----- Export ----------------------------------------------------------------------------------------------------------------------
writer = pd.ExcelWriter('results/careless_response_proportions.xlsx')

cr_proportions_total.to_excel(writer, 'proportions_total', index=False)
proportions_year.to_excel(writer, 'proportions_year', index=False)
proportions_journal.to_excel(writer, 'proportions_journal', index=False)
proportions_sample_source.to_excel(writer, 'proportions_sample_source', index=False)
proportions_sample_method.to_excel(writer, 'proportions_sample_method', index=False)
proportions_platform.to_excel(writer, 'proportions_platform', index=False)
proportions_cr_method.to_excel(writer, 'proportions_cr_method', index=False)
proportions_cr_type.to_excel(writer, 'proportions_cr_type', index=False)

writer.save()

