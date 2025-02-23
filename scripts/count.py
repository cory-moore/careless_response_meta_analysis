import pandas as pd
from collections import Counter
from codebook import codebook

def get_frequencies(data, variable):
    counter = Counter(data[variable])
    total = sum(counter.values())
   
    df = pd.DataFrame({
        'code': list(counter.keys()),
        'count': list(counter.values()),
        'percentage': [round(value / total * 100, 2) for value in counter.values()]
    })
   
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

def add_cr_method_type(data):
    cr_columns = ['cr_1_method', 'cr_2_method', 'cr_3_method', 'cr_4_method']
    # Use the cr_method_type mapping from the codebook
    cr_method_type_map = codebook['cr_method_type']
    # Loop over the cr_columns and add a new column for each cr_method type
    for col in cr_columns:
        new_col = col + '_type'
        data[new_col] = data[col].apply(lambda x: next((k for k, v in cr_method_type_map.items() if x in v), None))
    return data

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

def main():

    data = pd.read_csv('data/careless_data.csv')

    variables = ['sample_source', 'sample_recruitment', 'sample_method', 'sample_platform', 'sample_level',
                'sample_incentive', 'sample_country', 'cr_multiple', 'cr_sequential',
                'design_time', 'design_method', 'design_location']

    dfs = {}
    for variable in variables:
        df = get_frequencies(data, variable)
        dfs[variable] = df

    cr_subset = add_cr_method_type(data)
    dfs['cr_method'] = get_cr_method_frequencies(cr_subset)
    dfs['cr_type'] = get_cr_method_type_frequencies(cr_subset)

    for variable, df in dfs.items():
        print(f"Frequencies for {variable}:")
        print(df)
        print()

    with pd.ExcelWriter('results/frequencies.xlsx') as writer:
        for variable, df in dfs.items():
            df.to_excel(writer, sheet_name=variable, index=False)
            
if __name__ == '__main__':
    main()
