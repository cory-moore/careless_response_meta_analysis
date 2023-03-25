
import pandas as pd
import matplotlib.pyplot as plt
from scripts.codebook import codebook
from scripts.util import add_cr_method_type

#----- Functions ------------------------------------------------------------------------------------------------------------------
def compute_cr_rates(data, cr_type):
    cr_rate_total = data[cr_type] / data['sample_size']
    data['cr_rate_total'] = cr_rate_total
    return data

def subset_data_by_cr_method(df: pd.DataFrame, cr_method_code: int) -> pd.DataFrame:
    subset_df = df[(df['cr_multiple'] == 0) & (df['cr_sequential'] == -1) | (df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)]
    results_df = pd.DataFrame(columns=['ID', 'cr_method', 'cr_method_name', 'sample_size', 'cr_amount', 'cr_detail'])
    for _, row in subset_df.iterrows():
        for i in range(1, 5):
            if row[f'cr_{i}_method'] == cr_method_code:
                cr_amount = row[f'cr_{i}_amount']
                cr_method_detail = row[f'cr_{i}_method_detail']
                cr_method_name = codebook['cr_method'][cr_method_code]
                results_df = pd.concat([results_df, pd.DataFrame({
                    'ID': [row['ID']],
                    'sample_size': [row['sample_size']],
                    'cr_method': [cr_method_code],
                    'cr_amount': [cr_amount],
                    'cr_detail': [cr_method_detail],
                    'cr_method_name': [cr_method_name]
                })])
    return results_df

def compute_cr_method_rates(df: pd.DataFrame, cr_method_code: int) -> pd.DataFrame:
    subset_df = subset_data_by_cr_method(df, cr_method_code)
    cr_method_name = codebook['cr_method'][cr_method_code]
    results_df = subset_df.assign(cr_rate=subset_df['cr_amount'] / subset_df['sample_size'], cr_method_name=cr_method_name)
    return results_df




#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_csv('data/prelim_careless_data.csv')
subset_df = data[(data['cr_multiple'] == 0) & (data['cr_sequential'] == -1) | (data['cr_multiple'] == 1) & (data['cr_sequential'] == 1)]

# compute total cr rates
cr_rates_total = compute_cr_rates(data, 'cr_total_amount')

# compute cr rates for each method
dfs = []
for cr_method_code in range(12):
    cr_method_rates = compute_cr_method_rates(data, cr_method_code)
    dfs.append(cr_method_rates)

cr_rates_methods = pd.concat(dfs)


