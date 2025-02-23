import pandas as pd
import pingouin as pg
import numpy as np
from statsmodels.stats.inter_rater import fleiss_kappa

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
    data = pd.read_csv('data/rater_data.csv')
    results = calculate_reliability(data)
    
    print("\nResults Summary:")
    print(results.sort_values('Reliability', ascending=False))
    results.to_csv('results/reliability_results.csv', index=False)

if __name__ == "__main__":
    main()
