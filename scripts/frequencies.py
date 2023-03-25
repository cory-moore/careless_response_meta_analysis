
import pandas as pd

from collections import Counter
from scripts.codebook import codebook
from scripts.util import add_cr_method_type


#----- Functions ------------------------------------------------------------------------------------------------------------------
def get_frequencies(data, variable):
    counter = Counter(data[variable])
    total = sum(counter.values())
    # Create dataframe with code, count, and percentage
    df = pd.DataFrame({
        'code': list(counter.keys()),
        'count': list(counter.values()),
        'percentage': [round(value / total * 100, 2) for value in counter.values()]
    })
    # Map code values to sample source names from codebook
    df[variable] = df['code'].map(codebook[variable])
    df = df[[variable, 'code', 'count', 'percentage']]
    df = df.sort_values(by='code')
    return df

def get_cr_method_frequencies(data):
    cr_columns = ['cr_1_method', 'cr_2_method', 'cr_3_method', 'cr_4_method']
    cr_methods = data[cr_columns].stack()
    cr_methods = cr_methods[cr_methods != -1]
    cr_freq = cr_methods.value_counts().reset_index()
    cr_freq['cr_method'] = cr_freq['index'].map(codebook['cr_method'])
    total_studies = len(data)
    cr_freq['percentage'] = round(cr_freq[0] / total_studies * 100, 2)
    cr_freq = cr_freq[['cr_method', 'index', 0, 'percentage']].sort_values('index')
    cr_freq.columns = ['cr_method', 'code', 'count', 'percentage']
    return cr_freq

def get_cr_method_type_frequencies(data):
    cr_columns = ['cr_1_method_type', 'cr_2_method_type', 'cr_3_method_type', 'cr_4_method_type']
    cr_method_types = data[cr_columns].stack()
    cr_method_types = cr_method_types[cr_method_types.notnull()]
    cr_freq = cr_method_types.value_counts().reset_index()
    total_studies = len(data)
    cr_freq['percentage'] = round(cr_freq[0] / total_studies * 100, 2)
    cr_freq.columns = ['cr_method_type', 'count', 'percentage']
    cr_freq = cr_freq.sort_values('count', ascending=False)
    return cr_freq


#----- Main ------------------------------------------------------------------------------------------------------------------
data = pd.read_csv('data/prelim_careless_data.csv')

# List of variables by type to get frequencies for
variables = ['sample_source', 'sample_method', 'sample_platform', 'sample_level',
             'sample_incentive', 'sample_country', 'cr_multiple', 'cr_sequential',
             'design_time', 'design_method', 'design_location']

# get frequencies for sample variables
for variable in variables:
    df = get_frequencies(data, variable)
    print(f"Frequencies for variable {variable}:")
    print(df)
    print()

# get frequencies for cr_method and cr_method_type
data = add_cr_method_type(data)
get_cr_method_frequencies(data)
get_cr_method_type_frequencies(data)
