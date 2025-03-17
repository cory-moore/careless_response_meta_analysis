import pandas as pd
import os
import glob
from utils import load_codebook

def count_studies(data_path):
    """Generate frequency counts and sample sizes for a dataset"""
    data = pd.read_csv(data_path)
    dataset_name = os.path.basename(data_path).replace('.csv', '')
    
    study_count = len(data)
    sample_size = data['sample_size'].sum()
    overall = pd.DataFrame({
        'Dataset': [dataset_name],
        'Studies (k)': [study_count],
        'Participants (N)': [sample_size]
    })
    
    codebook = load_codebook('codebook.json')
    
    variables = [
        'sample_source', 'sample_recruitment', 'sample_method', 'sample_platform', 
        'sample_level', 'sample_incentive', 'sample_country', 'cr_multiple', 
        'cr_sequential', 'design_time', 'design_method', 'design_location'
    ]
    
    results = {'overall': overall}
    
    for variable in variables:
        if variable in data.columns:
            counts = data[variable].value_counts().reset_index()
            if len(counts) > 0:
                var_df = pd.DataFrame({
                    'code': counts['index'],
                    'k': counts[variable],
                    'percentage': counts[variable] / study_count * 100
                })
                
                if variable in codebook:
                    var_df[variable] = var_df['code'].astype(str).map(
                        {str(k): v for k, v in codebook[variable].items()})
                    var_df = var_df[[variable, 'code', 'k', 'percentage']]
                
                size_by_value = data.groupby(variable)['sample_size'].sum().reset_index()
                var_df = var_df.merge(
                    size_by_value.rename(columns={'sample_size': 'N'}), 
                    left_on='code', right_on=variable, how='left'
                )
                var_df = var_df.drop(columns=[variable+'_y'] if variable+'_y' in var_df.columns else []).rename(
                    columns={variable+'_x': variable} if variable+'_x' in var_df.columns else {}
                )
                
                var_df = var_df.sort_values('code')
                results[variable] = var_df
    
    if 'cr_method' in data.columns or ('method_code' in data.columns and 'sequential' in dataset_name):
        method_col = 'method_code' if 'method_code' in data.columns else 'cr_method'
        method_counts = data[method_col].value_counts().reset_index()
        
        method_df = pd.DataFrame({
            'code': method_counts['index'],
            'k': method_counts[method_col],
            'percentage': method_counts[method_col] / study_count * 100
        })
        
        method_df['method'] = method_df['code'].astype(str).map(
            {str(k): v for k, v in codebook['cr_method'].items()})
        
        size_by_method = data.groupby(method_col)['sample_size'].sum().reset_index()
        method_df = method_df.merge(
            size_by_method.rename(columns={'sample_size': 'N'}),
            left_on='code', right_on=method_col, how='left'
        )
        
        method_df = method_df.sort_values('code')
        method_df = method_df[['method', 'code', 'k', 'percentage', 'N']]
        results['method'] = method_df
    
    if 'method_type' in data.columns:
        type_counts = data['method_type'].value_counts().reset_index()
        
        type_df = pd.DataFrame({
            'method_type': type_counts['index'],
            'k': type_counts['method_type'],
            'percentage': type_counts['method_type'] / study_count * 100
        })
        
        size_by_type = data.groupby('method_type')['sample_size'].sum().reset_index()
        type_df = type_df.merge(
            size_by_type.rename(columns={'sample_size': 'N'}),
            on='method_type', how='left'
        )
        
        type_df = type_df.sort_values('k', ascending=False)
        results['method_type'] = type_df
    
    return results, dataset_name

def main():
    """Process all datasets and generate count workbooks"""
    data_dir = 'data/processed'
    
    if not os.path.exists(data_dir):
        print(f"Directory {data_dir} not found.")
        return
    
    all_datasets = glob.glob(f"{data_dir}/*.csv")
    
    if not all_datasets:
        print(f"No datasets found in {data_dir}.")
        return
    
    os.makedirs('output/python_results/counts', exist_ok=True)
    
    all_overall_counts = []
    
    for dataset_path in all_datasets:
        print(f"Processing {os.path.basename(dataset_path)}...")
        try:
            results, dataset_name = count_studies(dataset_path)
            
            with pd.ExcelWriter(f'output/python_results/counts/{dataset_name}_counts.xlsx') as writer:
                results['overall'].to_excel(writer, sheet_name='overall', index=False)
                
                for name, df in results.items():
                    if name != 'overall':
                        df.to_excel(writer, sheet_name=name[:31], index=False)
            
            all_overall_counts.append(results['overall'])
            
            print(f"  Counted {results['overall']['Studies (k)'].iloc[0]} studies with {results['overall']['Participants (N)'].iloc[0]} participants")
            
        except Exception as e:
            print(f"Error processing {dataset_path}: {str(e)}")
    
    if all_overall_counts:
        summary = pd.concat(all_overall_counts)
        summary.to_csv('output/python_results/counts/all_datasets_summary.csv', index=False)
        print(f"\nSummary of all {len(all_overall_counts)} datasets saved to output/python_results/counts/all_datasets_summary.csv")

if __name__ == '__main__':
    main()