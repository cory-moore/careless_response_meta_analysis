import pandas as pd
import pingouin as pg
import numpy as np
from statsmodels.stats.inter_rater import fleiss_kappa

def generate_unique_id(row, id_dict):
    """Generate a unique ID for a study based on first author and year"""
    first_name = row['authors'].split(',')[0].split(' ')[0]

    try:
        year = int(row['year'])
    except ValueError:  
        year = 'UnknownYear'

    base_id = f"{first_name}{year}"
    id_dict[base_id] = id_dict.get(base_id, -1) + 1
    new_id = base_id + chr(ord('a') + id_dict[base_id])
    return new_id

def add_unique_id(df, id_dict):
    """Add unique IDs to a dataframe"""
    df['ID'] = df.apply(lambda row: generate_unique_id(row, id_dict), axis=1)
    return df

def combine_coder_sheets(file_path):
    """
    Combines individual coder sheets while maintaining consistent IDs across raters.
    Each rater's sheet gets sequential IDs (a, b, c...) in the same order.
    """
    excel_file = pd.ExcelFile(file_path)
    dfs = []
    for sheet_name in excel_file.sheet_names:
        df = pd.read_excel(file_path, sheet_name=sheet_name)
        df = df.sort_values('title').reset_index(drop=True)
        df = add_unique_id(df, {})
        df['rater'] = sheet_name
        dfs.append(df)
    combined_df = pd.concat(dfs, ignore_index=True)
    cols = combined_df.columns.tolist()
    cols = ['rater'] + [col for col in cols if col != 'rater']
    combined_df = combined_df[cols]
    return combined_df

def check_rater_coverage(rater_data):
    """
    Check coverage of studies across raters.
    """
    rater_counts = rater_data['rater'].value_counts()
    print("########################################################")
    print("\nRATER COVERAGE:")
    print("\nNumber of studies coded by each rater:")
    print(rater_counts)
    
    # Check study coverage using standardized IDs
    study_coverage = rater_data.groupby('ID')['rater'].agg(['unique', 'count'])
    missing_studies = study_coverage[study_coverage['count'] < len(rater_data['rater'].unique())]
    
    if len(missing_studies) > 0:
        print("\nWarning: The following studies were not coded by all raters:")
        print(missing_studies)
    else:
        print("\nAll studies were coded by all raters")
    print("\n########################################################")

def calculate_reliability(data):
    """Calculate appropriate reliability statistics based on variable type"""
    exclude_cols = ['rater', 'title', 'authors', 'journal', 'Notes', 'year', 'ID', 'mturk_acceptance',
                    'sample_incentive_amount', 'sample_mturk', 'cr_1_method_detail', 'cr_2_method_detail',
                    'cr_3_method_detail', 'cr_4_method_detail', 'total_items']
    
    continuous_vars = [
        'sample_size', 
        'sample_age',
        'cr_total_amount',
        'cr_1_amount',
        'cr_2_amount',
        'cr_3_amount',
        'cr_4_amount'
    ]
    
    results = []
    for col in data.columns:
        if col not in exclude_cols:
            try:
                if col in continuous_vars:
                    # Handle continuous variables with ICC
                    icc = pg.intraclass_corr(
                        data=data,
                        targets='ID',
                        raters='rater',
                        ratings=col,
                        nan_policy='omit'
                    )
                    reliability = icc.loc[icc['Type'] == 'ICC2k', 'ICC'].iloc[0]
                    method = 'ICC2k'
                else:
                    # Handle categorical variables with Fleiss' Kappa
                    unique_vals = sorted(data[col].unique())
                    val_map = {val: i for i, val in enumerate(unique_vals)}
                    
                    pivot_data = data.pivot(index='ID', columns='rater', values=col)
                    n_subjects = len(pivot_data)
                    n_categories = len(val_map)
                    ratings_matrix = np.zeros((n_subjects, n_categories))
                    
                    for idx, row in pivot_data.iterrows():
                        for rating in row:
                            if pd.notna(rating):
                                category_idx = val_map[rating]
                                ratings_matrix[pivot_data.index.get_loc(idx), category_idx] += 1
                    
                    reliability = fleiss_kappa(ratings_matrix)
                    method = "Fleiss' Kappa"
                
                results.append({
                    'Variable': col,
                    'Type': 'continuous' if col in continuous_vars else 'categorical',
                    'Method': method,
                    'Reliability': reliability,
                    'N_unique': len(unique_vals)
                })
                
            except Exception as e:
                print(f"Error calculating reliability for {col}: {str(e)}")
    
    return pd.DataFrame(results)

def main():
    # Process rater data directly from the source Excel file
    rater_data_path = 'data/shared_studies_coded.xlsx'
    rater_data = combine_coder_sheets(rater_data_path)
    check_rater_coverage(rater_data)
    
    # Calculate reliability using the processed rater data
    results = calculate_reliability(rater_data)
    
    print("\nResults Summary:")
    results = results.sort_values('Reliability', ascending=False)
    print(results)
    results.to_csv('output/python_results/reliability_results.csv', index=False)

if __name__ == "__main__":
    main()
