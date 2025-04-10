import pandas as pd
import pingouin as pg
import numpy as np
from statsmodels.stats.inter_rater import fleiss_kappa
import xlsxwriter

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
                # Get number of studies (k) and raters for this variable
                valid_data = data[['ID', 'rater', col]].dropna()
                k = len(valid_data['ID'].unique())
                n_raters = len(valid_data['rater'].unique())
                
                if k < 2 or n_raters < 2:
                    print(f"Warning: Insufficient data for {col} (k={k}, n_raters={n_raters})")
                    continue
                
                if col in continuous_vars:
                    # Handle continuous variables with ICC
                    icc = pg.intraclass_corr(
                        data=valid_data,
                        targets='ID',
                        raters='rater',
                        ratings=col,
                        nan_policy='omit'
                    )
                    reliability = icc.loc[icc['Type'] == 'ICC2k', 'ICC'].iloc[0]
                    method = 'ICC2k'
                else:
                    # Handle categorical variables with Fleiss' Kappa
                    pivot_data = valid_data.pivot(index='ID', columns='rater', values=col)
                    unique_vals = sorted(valid_data[col].unique())
                    val_map = {val: i for i, val in enumerate(unique_vals)}
                    
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
                    'k': k,
                    'n_raters': n_raters
                })
                
            except Exception as e:
                print(f"Error calculating reliability for {col}: {str(e)}")
                continue
    
    if not results:
        print("Warning: No reliability calculations were successful!")
        return pd.DataFrame(columns=['Variable', 'Type', 'Method', 'Reliability', 'k', 'n_raters'])
    
    return pd.DataFrame(results)

def create_descriptive_stats(results_df):
    """Create descriptive statistics for reliability metrics by variable type"""
    if results_df.empty:
        print("Warning: No data available for descriptive statistics!")
        return pd.DataFrame(columns=['Type', 'Mean Reliability', 'Std Dev', 'Min', 'Max', 'Avg k', 'Avg Raters', 'Interpretation'])
    
    # Group by variable type and calculate statistics
    desc_stats = results_df.groupby('Type').agg({
        'Reliability': ['mean', 'std', 'min', 'max'],
        'k': 'mean',
        'n_raters': 'mean'
    }).round(3)
    
    # Flatten column names
    desc_stats.columns = ['Mean Reliability', 'Std Dev', 'Min', 'Max', 'Avg k', 'Avg Raters']
    desc_stats = desc_stats.reset_index()
    
    # Add interpretation guidelines
    desc_stats['Interpretation'] = desc_stats['Mean Reliability'].apply(
        lambda x: 'Excellent' if x >= 0.75 else 
                 'Good' if x >= 0.60 else 
                 'Fair' if x >= 0.40 else 'Poor'
    )
    
    return desc_stats

def main():
    # Process rater data directly from the source Excel file
    rater_data_path = 'data/shared_studies_coded.xlsx'
    rater_data = combine_coder_sheets(rater_data_path)
    check_rater_coverage(rater_data)
    
    # Calculate reliability using the processed rater data
    results = calculate_reliability(rater_data)
    
    if not results.empty:
        print("\nResults Summary:")
        results = results.sort_values('Reliability', ascending=False)
        print(results)
        
        # Create Excel workbook with multiple sheets
        output_path = 'output/python_results/reliability_results.xlsx'
        with pd.ExcelWriter(output_path, engine='xlsxwriter') as writer:
            # Write main results to first sheet
            results.to_excel(writer, sheet_name='Detailed Results', index=False)
            
            # Create and write descriptive statistics to second sheet
            desc_stats = create_descriptive_stats(results)
            desc_stats.to_excel(writer, sheet_name='Descriptive Statistics', index=False)
            
            # Get workbook and worksheet objects
            workbook = writer.book
            worksheet = writer.sheets['Descriptive Statistics']
            
            # Add formatting
            header_format = workbook.add_format({
                'bold': True,
                'bg_color': '#D9E1F2',
                'border': 1
            })
            
            # Format headers
            for col_num, value in enumerate(desc_stats.columns.values):
                worksheet.write(0, col_num, value, header_format)
            
            # Auto-adjust column widths
            for idx, col in enumerate(desc_stats):
                max_length = max(
                    desc_stats[col].astype(str).apply(len).max(),
                    len(str(col))
                )
                worksheet.set_column(idx, idx, max_length + 2)
    else:
        print("\nNo results were generated. Please check the data and error messages above.")

if __name__ == "__main__":
    main()
