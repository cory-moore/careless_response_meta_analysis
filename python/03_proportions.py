import pandas as pd
import numpy as np
import os
from utils import load_codebook

def compute_se(p, n):
    """Calculate standard error for a proportion"""
    if pd.isna(p) or pd.isna(n) or n == 0:
        return np.nan
    p = np.clip(p, 0, 1)
    return np.sqrt(p * (1 - p) / n)

def compute_ci(p, n, alpha=0.05):
    """Compute confidence interval for a proportion"""
    if pd.isna(p) or pd.isna(n) or n == 0:
        return np.nan, np.nan
    p = np.clip(p, 0, 1)
    z = 1.96  # 95% CI
    se = compute_se(p, n)
    return np.clip(p - z * se, 0, 1), np.clip(p + z * se, 0, 1)

def add_stats(df, prop_col='proportion'):
    """Add standard error and CI to a dataframe"""
    df['se'] = df.apply(lambda row: compute_se(row[prop_col], row['sample_size']), axis=1)
    ci_values = df.apply(lambda row: compute_ci(row[prop_col], row['sample_size']), axis=1)
    if len(ci_values) > 0:
        df['ci_lower'], df['ci_upper'] = zip(*ci_values)
    return df

def add_dimension_labels(data):
    """Add dimension labels from codebook"""
    codebook = load_codebook('codebook.json')
    
    # Add dimension labels using codebook mappings
    dimension_mappings = {
        'journal': 'journal_name',
        'sample_source': 'sample_source_name',
        'sample_recruitment': 'sample_recruitment_name',
        'sample_method': 'sample_method_name',
        'sample_platform': 'sample_platform_name',
        'sample_level': 'sample_level_name',
        'sample_incentive': 'sample_incentive_name',
        'sample_country': 'sample_country_name',
        'cr_method': 'cr_method_name',
        'design_method': 'design_method_name',
        'design_location': 'design_location_name',
        'method_code': 'method_name'  # For sequential dataset
    }
    
    for col, name_col in dimension_mappings.items():
        if col in data.columns and col in codebook:
            # Check if values are already strings (journal names) or need conversion
            first_value = data[col].iloc[0] if len(data) > 0 else None
            
            # Only attempt mapping if values appear to be numeric
            if isinstance(first_value, (int, float)) or (isinstance(first_value, str) and first_value.isdigit()):
                # Convert codes to names using codebook
                data[name_col] = data[col].map({int(k): v for k, v in codebook[col].items()})
            else:
                # Values are already strings, just copy to the name column
                data[name_col] = data[col]
    
    # Map method type if applicable
    if 'method_type' not in data.columns:
        method_col = 'cr_method' if 'cr_method' in data.columns else ('method_code' if 'method_code' in data.columns else None)
        if method_col and method_col in data.columns:
            # Create method_type mapping function
            def get_method_type(method_code):
                if pd.isna(method_code):
                    return "unknown"
                for type_name, methods in codebook['cr_method_type'].items():
                    if int(method_code) in methods:
                        return type_name
                return "other"
            
            data['method_type'] = data[method_col].apply(get_method_type)
    
    return data

def process_dataset(data, dataset_type):
    """Prepare data for meta-analysis, maintaining individual study data"""
    print(f"\nPreparing data for {dataset_type} meta-analysis:")
    
    # Add labels for all dimensions
    data = add_dimension_labels(data)
    
    # Determine which proportion columns to use based on dataset type
    if dataset_type == 'sequential':
        print("  Processing sequential dataset with raw and adjusted proportions")
        # For sequential data, calculate stats for both raw and adjusted proportions
        if 'raw_proportion' in data.columns:
            data = add_stats(data, 'raw_proportion')
            
        if 'adjusted_proportion' in data.columns and 'remaining_sample' in data.columns:
            # Calculate SE and CI for adjusted proportions (use remaining sample as n)
            data['adj_se'] = data.apply(lambda row: compute_se(row['adjusted_proportion'], 
                                                          row['remaining_sample']), axis=1)
            
            adj_ci_values = data.apply(lambda row: compute_ci(row['adjusted_proportion'], 
                                                         row['remaining_sample']), axis=1)
            if len(adj_ci_values) > 0:
                data['adj_ci_lower'], data['adj_ci_upper'] = zip(*adj_ci_values)
    else:
        # For regular datasets, use the standard proportion column
        data = add_stats(data)
    
    print(f"  Processed {len(data)} studies with individual proportions")
    
    # Report proportion ranges and statistics
    prop_col = 'proportion' if 'proportion' in data.columns else ('raw_proportion' if 'raw_proportion' in data.columns else None)
    if prop_col:
        print(f"  {prop_col} range: {data[prop_col].min():.4f} - {data[prop_col].max():.4f}")
        print(f"  Mean {prop_col}: {data[prop_col].mean():.4f} (SD = {data[prop_col].std():.4f})")
    
    return data

def save_processed_data(data, output_path):
    """Save processed data for R meta-analysis"""
    # Ensure output directory exists
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    # Save the data
    data.to_csv(output_path, index=False)
    print(f"  Saved processed data to {output_path}")

def main():
    """Prepare data for meta-analysis"""
    os.makedirs("data/for_meta", exist_ok=True)
    
    print("\nPREPARING DATA FOR META-ANALYSIS")
    print("===============================")
        
    # Process First-Method dataset (PRIMARY ANALYSIS)
    first_method_path = "data/processed/first_method_data.csv"
    if os.path.exists(first_method_path):
        data = pd.read_csv(first_method_path)
        print(f"  Loaded First-Method dataset: {len(data)} studies")
        
        processed_data = process_dataset(data, 'first_method')
        save_processed_data(processed_data, "data/for_meta/first_method_data.csv")
    
    # Process Single-Method dataset (SECONDARY ANALYSIS)
    single_method_path = "data/processed/single_method_data.csv"
    if os.path.exists(single_method_path):
        data = pd.read_csv(single_method_path)
        print(f"  Loaded Single-Method dataset: {len(data)} studies")
        
        processed_data = process_dataset(data, 'single_method')
        save_processed_data(processed_data, "data/for_meta/single_method_data.csv")
    
    # Process Overall dataset (TERTIARY ANALYSIS)
    overall_path = "data/processed/overall_data.csv"
    if os.path.exists(overall_path):
        data = pd.read_csv(overall_path)
        print(f"  Loaded Overall dataset: {len(data)} studies")
        
        processed_data = process_dataset(data, 'overall')
        save_processed_data(processed_data, "data/for_meta/overall_data.csv")
    else:
        print(f"  Warning: Overall dataset not found at {overall_path}")
    
    # Process Sequential dataset (POSITIONAL EFFECTS ANALYSIS)
    sequential_path = "data/processed/sequential_data.csv"
    if os.path.exists(sequential_path):
        data = pd.read_csv(sequential_path)
        print(f"  Loaded Sequential dataset: {len(data)} method-position combinations")
        
        processed_data = process_dataset(data, 'sequential')
        save_processed_data(processed_data, "data/for_meta/sequential_data.csv")

    print("\nData preparation complete - ready for R meta-analysis")

if __name__ == "__main__":
    main()