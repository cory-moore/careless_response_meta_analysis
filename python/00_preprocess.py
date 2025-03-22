import pandas as pd
import numpy as np
import os
from utils import load_codebook

codebook = load_codebook('codebook.json')

def generate_unique_id(row, id_dict):
    first_name = row['authors'].split(',')[0].split(' ')[0]
    try:
        year = int(row['year'])
    except ValueError:  
        year = 'UnknownYear'
    base_id = f"{first_name}{year}"
    id_dict[base_id] = id_dict.get(base_id, -1) + 1
    return base_id + chr(ord('a') + id_dict[base_id])

def add_unique_id(df, id_dict):
    df['ID'] = df.apply(lambda row: generate_unique_id(row, id_dict), axis=1)
    return df

def check_basic_info(df):
    """Show basic dataset information."""
    print("\nBASIC INFORMATION:")
    print(f"  Studies: {len(df)} rows ({df['ID'].nunique()} unique IDs, {df['title'].nunique()} unique papers)")

def check_coding_distribution(df):
    """Show distribution of studies across coders."""
    print("\nCODING DISTRIBUTION:")
    for coder, count in df['coder'].value_counts().items():
        print(f"  {coder}: {count} studies")

def check_missing_values(df):
    """Check for missing values in the dataset."""
    print(f"\nMISSING VALUES:")
    missing_found = False
    for col in df.columns:
        missing = df[col].isna().sum()
        if col != 'Notes' and missing > 0:
            missing_found = True
            print(f"    {col}: {missing}")
    if not missing_found:
        print("    None found")

def check_invalid_codes(df):
    """Check for invalid coding values."""
    print("\nINVALID CODE CHECKS:")
    no_missing_allowed = {
        'sample_size': 'Sample size cannot be unknown (-1)',
        'cr_total_amount': 'CR amount cannot be unknown (-1)',
        'cr_1_method': 'CR method cannot be unknown (-1)'
    }
    
    invalid_found = False
    for var, message in no_missing_allowed.items():
        invalid = df[df[var] == -1]
        if len(invalid) > 0:
            invalid_found = True
            print(f"\n  {message}: {len(invalid)} studies")
    
    if not invalid_found:
        print("    None found")

def clean_wave_data(df):
    """Remove later waves in longitudinal studies."""
    print("\nCLEANING WAVE DATA:")
    clean_df = df.copy()
    
    # Step 1: Remove final wave screen studies
    final_wave_screen = clean_df[clean_df['sample_time'] > 1].copy()
    removed_step1 = []
    
    if len(final_wave_screen) > 0:
        for title in final_wave_screen['title'].unique():
            study_data = clean_df[clean_df['title'] == title]
            if 1 not in study_data['sample_time'].values:
                rows_to_remove = len(study_data)
                clean_df = clean_df[clean_df['title'] != title]
                removed_step1.append(title)
    
    removed_first = len(df) - len(clean_df)
    if removed_first > 0:
        print(f"  Removed {removed_first} rows from {len(removed_step1)} studies (final wave only)")
    
    # Step 2: For remaining studies with multiple waves, keep only first wave
    wave_studies = clean_df[clean_df['title'].duplicated(keep=False)]
    removed_step2 = []
    
    if len(wave_studies) > 0:
        for title in wave_studies['title'].unique():
            waves = wave_studies[wave_studies['title'] == title].copy()
            base_numbers = waves['study_number'].astype(str).str.extract('(\d+)')[0]
            
            for base_num in base_numbers.unique():
                study_waves = waves[base_numbers == base_num]
                
                if (study_waves['sample_time'].nunique() == 1 and 
                    study_waves['sample_time'].iloc[0] == 1 and 
                    1 not in study_waves['design_time'].values):
                    continue
                
                if (1 in study_waves['design_time'].values or study_waves['sample_time'].nunique() > 1):
                    removed_step2.append(title)
                
                if 1 in study_waves['design_time'].values:
                    clean_df = clean_df[
                        ~((clean_df['title'] == title) & 
                          (clean_df['study_number'].astype(str).str.startswith(base_num)) &
                          (clean_df['sample_time'] > study_waves['sample_time'].min()))
                    ]
                elif study_waves['sample_time'].nunique() > 1:
                    clean_df = clean_df[
                        ~((clean_df['title'] == title) & 
                          (clean_df['study_number'].astype(str).str.startswith(base_num)) &
                          (clean_df['sample_time'] > study_waves['sample_time'].min()))
                    ]
    
    removed_second = len(df) - len(clean_df) - removed_first
    if removed_second > 0:
        print(f"  Removed {removed_second} rows from {len(set(removed_step2))} studies (later waves)")
    
    return clean_df

def check_logical_rules(df):
    """Check for logical inconsistencies in the data."""
    print("\nLOGICAL CONSISTENCY CHECKS:")
    codebook_errors = False
    for col, valid_values in codebook.items():
        if col != 'journal' and col in df.columns:
            invalid = df[~df[col].isin([int(k) for k in valid_values.keys()])]
            if len(invalid) > 0:
                codebook_errors = True
                print(f"  Invalid {col} values: {len(invalid)} instances")
    if not codebook_errors:
        print("  All codebook values are valid")
    
    rules = [
        {
            'name': 'Single Method Studies',
            'condition': lambda x: (x['cr_multiple'] == 0) & (
                (x['cr_sequential'] != -1) |
                (x['cr_2_method'] != -1) |
                (x['cr_3_method'] != -1) |
                (x['cr_4_method'] != -1) |
                (x['cr_2_amount'] != -1) |
                (x['cr_3_amount'] != -1) |
                (x['cr_4_amount'] != -1)
            ),
            'message': 'Single method studies with additional method data'
        },
        {
            'name': 'Multiple Methods Missing Second Method',
            'condition': lambda x: (x['cr_multiple'] == 1) & (x['cr_2_method'] == -1),
            'message': 'Multiple methods indicated but no second method'
        },
        {
            'name': 'Sequential Reporting Missing Amounts',
            'condition': lambda x: (x['cr_sequential'] == 1) & (
                ((x['cr_1_method'] != -1) & (x['cr_1_amount'] == -1)) |
                ((x['cr_2_method'] != -1) & (x['cr_2_amount'] == -1)) |
                ((x['cr_3_method'] != -1) & (x['cr_3_amount'] == -1)) |
                ((x['cr_4_method'] != -1) & (x['cr_4_amount'] == -1))
            ),
            'message': 'Sequential reporting missing amounts'
        },
        {
            'name': 'Non-sequential Missing Total',
            'condition': lambda x: (x['cr_multiple'] == 1) & 
                                 (x['cr_sequential'] == 0) & 
                                 (x['cr_total_amount'] == -1),
            'message': 'Non-sequential missing total CR amount'
        },
        {
            'name': 'Impossible CR Proportions',
            'condition': lambda x: (x['cr_total_amount'] > x['sample_size']) & 
                                 (x['cr_total_amount'] != -1) & 
                                 (x['sample_size'] != -1),
            'message': 'CR amount exceeds sample size'
        },
        {
            'name': 'Implausible CR Proportions',
            'condition': lambda x: ((x['cr_total_amount'] / x['sample_size']) > 0.50) & 
                                 (x['cr_total_amount'] <= x['sample_size']) &
                                 (x['cr_total_amount'] != -1) & 
                                 (x['sample_size'] != -1),
            'message': 'CR proportion exceeds 50%'
        },
        {
            'name': 'Wave Indicator Consistency',
            'condition': lambda x: (
                ((x['design_time'] == 1) & 
                 (x['sample_time'] == 1) & 
                 ~x.groupby('title')['sample_time'].transform(lambda x: x.max() > 1)) |
                ((x['sample_time'] > 1) & 
                 (x['design_time'] != 1) & 
                 ~x.groupby('title')['design_time'].transform(lambda x: 1 in x.values))
            ),
            'message': 'Inconsistent wave indicators'
        },
        {
            'name': 'Study Number Format',
            'condition': lambda x: (
                (x['sample_time'] > 1) & 
                ~x['study_number'].astype(str).str.contains('[a-zA-Z]')
            ),
            'message': 'Multiple waves without letter-based study numbers'
        }
    ]
    
    for rule in rules:
        violations = df[rule['condition'](df)]
        if len(violations) > 0:
            print(f"  {rule['message']}: {len(violations)} instances")
        else:
            print(f"  ✓ {rule['name']} check passed")

def add_method_position_coding(df):
    """Add position coding for all methods in each study."""
    print("\nADDING METHOD POSITION CODING:")
    result_df = df.copy()
    result_df['method_1_position'] = np.nan
    result_df['method_2_position'] = np.nan
    result_df['method_3_position'] = np.nan
    result_df['method_4_position'] = np.nan
    
    single_method = result_df['cr_multiple'] == 0
    result_df.loc[single_method, 'method_1_position'] = 1
    
    sequential = (result_df['cr_multiple'] == 1) & (result_df['cr_sequential'] == 1)
    result_df.loc[sequential, 'method_1_position'] = 1
    
    has_method_2 = (result_df['cr_2_method'] != -1) & ~result_df['cr_2_method'].isna()
    has_method_3 = (result_df['cr_3_method'] != -1) & ~result_df['cr_3_method'].isna()
    has_method_4 = (result_df['cr_4_method'] != -1) & ~result_df['cr_4_method'].isna()
    
    result_df.loc[sequential & has_method_2, 'method_2_position'] = 2
    result_df.loc[sequential & has_method_3, 'method_3_position'] = 3
    result_df.loc[sequential & has_method_4, 'method_4_position'] = 4
    
    print(f"  Single method studies: {sum(single_method)}")
    print(f"  Sequential studies: {sum(sequential)} (with 2+ methods: {sum(sequential & has_method_2)})")
    
    return result_df

def calculate_remaining_sample(df):
    """Calculate remaining sample size at each screening position."""
    print("\nCALCULATING REMAINING SAMPLE SIZES:")
    result_df = df.copy()
    
    sequential = (result_df['cr_multiple'] == 1) & (result_df['cr_sequential'] == 1)
    
    if sum(sequential) == 0:
        print("  No sequential screening studies found")
        return result_df
    
    result_df['remaining_sample_1'] = result_df['sample_size']
    result_df['remaining_sample_2'] = np.nan
    result_df['remaining_sample_3'] = np.nan
    result_df['remaining_sample_4'] = np.nan
    
    seq_studies = result_df[sequential].copy()
    
    has_method_2 = (seq_studies['cr_2_method'] != -1) & ~seq_studies['cr_2_method'].isna()
    seq_studies.loc[has_method_2, 'remaining_sample_2'] = \
        seq_studies.loc[has_method_2, 'sample_size'] - seq_studies.loc[has_method_2, 'cr_1_amount']
    
    has_method_3 = (seq_studies['cr_3_method'] != -1) & ~seq_studies['cr_3_method'].isna()
    seq_studies.loc[has_method_3, 'remaining_sample_3'] = \
        seq_studies.loc[has_method_3, 'remaining_sample_2'] - seq_studies.loc[has_method_3, 'cr_2_amount']
    
    has_method_4 = (seq_studies['cr_4_method'] != -1) & ~seq_studies['cr_4_method'].isna()
    seq_studies.loc[has_method_4, 'remaining_sample_4'] = \
        seq_studies.loc[has_method_4, 'remaining_sample_3'] - seq_studies.loc[has_method_4, 'cr_3_amount']
    
    for col in ['remaining_sample_1', 'remaining_sample_2', 'remaining_sample_3', 'remaining_sample_4']:
        if col in seq_studies.columns:
            result_df[col].update(seq_studies[col])
    
    return result_df

def clean_count_variables(df, count_vars=None, id_var='ID'):
    """
    Clean count variables by handling -1 values and invalid entries.
    
    This function performs several data cleaning steps:
    1. Replaces -1 values with NaN in specified count variables
    2. Removes rows with invalid values (non-positive sample sizes or missing counts)
    3. Reports on any data removed during cleaning
    """
    if count_vars is None:
        count_vars = ['cr_amount', 'sample_size']
    
    cleaned_df = df.copy()
    initial_count = len(cleaned_df)
    initial_studies = cleaned_df[id_var].nunique()
    
    # Replace -1 with NaN in count variables
    for var in count_vars:
        if var in cleaned_df.columns:
            invalid_count = (cleaned_df[var] == -1).sum()
            if invalid_count > 0:
                print(f"    Converting {invalid_count} '-1' values to NaN in {var}")
                cleaned_df[var] = cleaned_df[var].replace(-1, np.nan)
    
    # Remove invalid entries
    if 'sample_size' in count_vars and 'sample_size' in cleaned_df.columns:
        cleaned_df = cleaned_df[cleaned_df['sample_size'] > 0].copy()
    
    for var in count_vars:
        if var in cleaned_df.columns and var != 'sample_size':
            cleaned_df = cleaned_df[~cleaned_df[var].isna()].copy()
    
    # Report changes
    removed_rows = initial_count - len(cleaned_df)
    removed_studies = initial_studies - cleaned_df[id_var].nunique()
    
    if removed_rows > 0:
        print(f"    Removed {removed_rows} rows ({removed_studies} unique studies) with invalid values")
    
    return cleaned_df

def create_first_method_dataset(df):
    """Create dataset following the First-Method approach."""
    print("\nCREATING FIRST-METHOD DATASET:")
    unique_studies = df['ID'].nunique()
    
    single_method_count = df[df['cr_multiple'] == 0]['ID'].nunique()
    multi_sequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)]['ID'].nunique()
    multi_nonsequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 0)]['ID'].nunique()
    
    print(f"  Starting with {unique_studies} unique studies")
    print(f"  Study types: {single_method_count} single-method, {multi_sequential_count} sequential, "
          f"{multi_nonsequential_count} non-sequential")
    
    first_method_filter = (df['cr_multiple'] == 0) | \
                         ((df['cr_multiple'] == 1) & 
                          (df['cr_sequential'] == 1))
    
    first_method_data = df[first_method_filter].copy()
    
    included_ids = set(first_method_data['ID'].unique())
    all_ids = set(df['ID'].unique())
    excluded_count = len(all_ids - included_ids)
    
    print(f"  Excluded {excluded_count} studies (primarily non-sequential)")
    
    multi_method = (first_method_data['cr_multiple'] == 1) & (first_method_data['cr_sequential'] == 1)
    single_method = (first_method_data['cr_multiple'] == 0)
    
    first_method_data['cr_method'] = np.where(single_method, first_method_data['cr_1_method'], first_method_data['cr_1_method'])
    first_method_data['cr_amount'] = np.where(single_method, first_method_data['cr_1_amount'], first_method_data['cr_1_amount'])
    first_method_data['cr_position'] = 1
    first_method_data = clean_count_variables(first_method_data, count_vars=['cr_amount', 'sample_size'])
    first_method_data['proportion'] = np.clip(first_method_data['cr_amount'] / first_method_data['sample_size'], 0, 1)
    
    # Add method type and timing
    if 'cr_method' in first_method_data.columns:
        def get_method_type(method_code):
            for type_name, methods in codebook['cr_method_type'].items():
                if method_code in methods:
                    return type_name
            return "other"
        
        first_method_data['method_type'] = first_method_data['cr_method'].apply(get_method_type)
        
        def get_method_timing(method_code):
            a_priori_methods = codebook['cr_method_timing_map']['a_priori']
            post_hoc_methods = codebook['cr_method_timing_map']['post_hoc']
            
            if method_code in a_priori_methods:
                return 'a_priori'
            elif method_code in post_hoc_methods:
                return 'post_hoc'
            return None
        
        first_method_data['method_timing'] = first_method_data['cr_method'].apply(get_method_timing)
    
    problematic = first_method_data[
        (first_method_data['proportion'] < 0) | 
        (first_method_data['proportion'] > 1)]
    if len(problematic) > 0:
        print("    WARNING: Problematic proportion calculations found:")
        print(problematic[['ID', 'cr_amount', 'sample_size', 'proportion']])
    
    final_unique_studies = first_method_data['ID'].nunique()
    print(f"  First-Method dataset: {final_unique_studies} studies "
          f"({sum(single_method)} single-method, {sum(multi_method)} sequential first methods)")
    
    return first_method_data

def create_single_method_dataset(df):
    """Create dataset with only single-method studies."""
    print("\nCREATING SINGLE-METHOD DATASET:")
    unique_studies = df['ID'].nunique()
    
    single_method_count = df[df['cr_multiple'] == 0]['ID'].nunique()
    multi_sequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)]['ID'].nunique()
    multi_nonsequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 0)]['ID'].nunique()
    
    single_method_filter = (df['cr_multiple'] == 0)
    single_method_data = df[single_method_filter].copy()
    
    single_method_data['cr_method'] = single_method_data['cr_1_method']
    single_method_data['cr_amount'] = single_method_data['cr_1_amount']
    single_method_data['cr_position'] = 1
    
    single_method_data = clean_count_variables(single_method_data, count_vars=['cr_amount', 'sample_size'])
    
    single_method_data['proportion'] = np.clip(single_method_data['cr_amount'] / single_method_data['sample_size'], 0, 1)
    
    # Add method type and timing
    if 'cr_method' in single_method_data.columns:
        def get_method_type(method_code):
            for type_name, methods in codebook['cr_method_type'].items():
                if method_code in methods:
                    return type_name
            return "other"
        
        single_method_data['method_type'] = single_method_data['cr_method'].apply(get_method_type)
        
        def get_method_timing(method_code):
            a_priori_methods = codebook['cr_method_timing_map']['a_priori']
            post_hoc_methods = codebook['cr_method_timing_map']['post_hoc']
            
            if method_code in a_priori_methods:
                return 'a_priori'
            elif method_code in post_hoc_methods:
                return 'post_hoc'
            return None
        
        single_method_data['method_timing'] = single_method_data['cr_method'].apply(get_method_timing)
    
    excluded = unique_studies - single_method_count
    print(f"  Starting with {unique_studies} unique studies")
    print(f"  Excluded {excluded} studies ({multi_sequential_count} sequential, {multi_nonsequential_count} non-sequential)")
    print(f"  Final dataset: {single_method_data['ID'].nunique()} single-method studies")
    
    return single_method_data

def create_sequential_dataset(df):
    """Create dataset with sequential screening studies in long format."""
    print("\nCREATING SEQUENTIAL DATASET:")
    unique_studies = df['ID'].nunique()
    
    single_method_count = df[df['cr_multiple'] == 0]['ID'].nunique()
    multi_seq_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)]['ID'].nunique()
    multi_nonseq_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 0)]['ID'].nunique()
    
    sequential_filter = (df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)
    sequential_data = df[sequential_filter].copy()
    
    excluded = unique_studies - multi_seq_count
    print(f"  Starting with {unique_studies} unique studies")
    print(f"  Excluded {excluded} studies ({single_method_count} single-method, {multi_nonseq_count} non-sequential)")
    
    if len(sequential_data) == 0:
        print("  No sequential screening studies found")
        return pd.DataFrame()
    
    sequential_data = clean_count_variables(sequential_data, count_vars=['sample_size', 'cr_1_amount', 'cr_2_amount', 'cr_3_amount', 'cr_4_amount'])
    
    # Define sample columns to preserve
    sample_columns = [
        'ID', 'title', 'year', 'authors', 'journal',
        'sample_size', 'sample_source', 'sample_recruitment', 
        'sample_platform', 'sample_method', 'sample_level',
        'sample_incentive', 'sample_country', 
        'design_method', 'design_location'
    ]
    
    # Only keep columns that actually exist in the dataframe
    sample_columns = [col for col in sample_columns if col in sequential_data.columns]
    
    method_positions = []
    
    for _, row in sequential_data.iterrows():
        # Get all sample characteristics to preserve
        sample_info = {col: row[col] for col in sample_columns if col in row}
        
        # Position 1
        pos1_data = sample_info.copy()
        pos1_data.update({
            'method_position': 1,
            'method_code': row['cr_1_method'],
            'cr_amount': row['cr_1_amount'],
            'remaining_sample': row['remaining_sample_1'],
            'raw_proportion': np.clip(row['cr_1_amount'] / row['sample_size'], 0, 1),
            'adjusted_proportion': np.clip(row['cr_1_amount'] / row['remaining_sample_1'], 0, 1)
        })
        method_positions.append(pos1_data)
        
        # Position 2
        if row['cr_2_method'] != -1 and not pd.isna(row['cr_2_method']) and not pd.isna(row['cr_2_amount']):
            pos2_data = sample_info.copy()
            pos2_data.update({
                'method_position': 2,
                'method_code': row['cr_2_method'],
                'cr_amount': row['cr_2_amount'],
                'remaining_sample': row['remaining_sample_2'],
                'raw_proportion': np.clip(row['cr_2_amount'] / row['sample_size'], 0, 1),
                'adjusted_proportion': np.clip(row['cr_2_amount'] / row['remaining_sample_2'], 0, 1)
            })
            method_positions.append(pos2_data)
        
        # Position 3
        if row['cr_3_method'] != -1 and not pd.isna(row['cr_3_method']) and not pd.isna(row['cr_3_amount']):
            pos3_data = sample_info.copy()
            pos3_data.update({
                'method_position': 3,
                'method_code': row['cr_3_method'],
                'cr_amount': row['cr_3_amount'],
                'remaining_sample': row['remaining_sample_3'],
                'raw_proportion': np.clip(row['cr_3_amount'] / row['sample_size'], 0, 1),
                'adjusted_proportion': np.clip(row['cr_3_amount'] / row['remaining_sample_3'], 0, 1)
            })
            method_positions.append(pos3_data)
        
        # Position 4
        if row['cr_4_method'] != -1 and not pd.isna(row['cr_4_method']) and not pd.isna(row['cr_4_amount']):
            pos4_data = sample_info.copy()
            pos4_data.update({
                'method_position': 4,
                'method_code': row['cr_4_method'],
                'cr_amount': row['cr_4_amount'],
                'remaining_sample': row['remaining_sample_4'],
                'raw_proportion': np.clip(row['cr_4_amount'] / row['sample_size'], 0, 1),
                'adjusted_proportion': np.clip(row['cr_4_amount'] / row['remaining_sample_4'], 0, 1)
            })
            method_positions.append(pos4_data)
    
    method_pos_df = pd.DataFrame(method_positions)
    
    # Add method name and method type
    if 'method_code' in method_pos_df.columns:
        method_pos_df['method_name'] = method_pos_df['method_code'].map(
            {int(k): v for k, v in codebook['cr_method'].items()}
        )
        
        def get_method_type(method_code):
            for type_name, methods in codebook['cr_method_type'].items():
                if method_code in methods:
                    return type_name
            return "other"
        
        method_pos_df['method_type'] = method_pos_df['method_code'].apply(get_method_type)
        
        def get_method_timing(method_code):
            a_priori_methods = codebook['cr_method_timing_map']['a_priori']
            post_hoc_methods = codebook['cr_method_timing_map']['post_hoc']
            
            if method_code in a_priori_methods:
                return 'a_priori'
            elif method_code in post_hoc_methods:
                return 'post_hoc'
            return None
        
        method_pos_df['method_timing'] = method_pos_df['method_code'].apply(get_method_timing)
    
    method_position_count = len(method_pos_df)
    pos_counts = method_pos_df['method_position'].value_counts().sort_index()
    
    print(f"  Final dataset: {multi_seq_count} studies with {method_position_count} method-position combinations")
    print(f"  Method positions: " + ", ".join([f"Pos {pos}: {count}" for pos, count in pos_counts.items()]))
    
    return method_pos_df

def create_overall_dataset(df):
    """
    Create dataset including all studies, using the total CR amount.
    This captures all available data regardless of method configuration
    (single method, sequential, non-sequential).
    """
    print("\nCREATING OVERALL DATASET:")
    total_studies = len(df)
    unique_studies = df['ID'].nunique()
    print(f"  Including all {unique_studies} unique studies")
    
    single_method_count = df[df['cr_multiple'] == 0]['ID'].nunique()
    multi_sequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 1)]['ID'].nunique()
    multi_nonsequential_count = df[(df['cr_multiple'] == 1) & (df['cr_sequential'] == 0)]['ID'].nunique()
    other_count = unique_studies - single_method_count - multi_sequential_count - multi_nonsequential_count
    
    print(f"  Study composition:")
    print(f"    Single-method studies: {single_method_count} ({round(single_method_count/unique_studies*100, 1)}%)")
    print(f"    Sequential multiple-method studies: {multi_sequential_count} ({round(multi_sequential_count/unique_studies*100, 1)}%)")
    print(f"    Non-sequential multiple-method studies: {multi_nonsequential_count} ({round(multi_nonsequential_count/unique_studies*100, 1)}%)")
    if other_count > 0:
        print(f"    Other studies: {other_count} ({round(other_count/unique_studies*100, 1)}%)")
    
    overall_data = df.copy()
    
    # Add method information for single-method studies
    single_method_mask = (overall_data['cr_multiple'] == 0)
    if 'cr_1_method' in overall_data.columns:
        overall_data.loc[single_method_mask, 'cr_method'] = overall_data.loc[single_method_mask, 'cr_1_method']
    
    # Add method type and timing for studies with method information
    if 'cr_method' in overall_data.columns:
        def get_method_type(method_code):
            if pd.isna(method_code):
                return None
                
            for type_name, methods in codebook['cr_method_type'].items():
                if method_code in methods:
                    return type_name
            return "other"
        
        overall_data['method_type'] = overall_data['cr_method'].apply(get_method_type)
        
        def get_method_timing(method_code):
            if pd.isna(method_code):
                return None
                
            a_priori_methods = codebook['cr_method_timing_map']['a_priori']
            post_hoc_methods = codebook['cr_method_timing_map']['post_hoc']
            
            if method_code in a_priori_methods:
                return 'a_priori'
            elif method_code in post_hoc_methods:
                return 'post_hoc'
            return None
        
        overall_data['method_timing'] = overall_data['cr_method'].apply(get_method_timing)
    
    overall_data = clean_count_variables(overall_data, count_vars=['cr_total_amount', 'sample_size'])
    
    overall_data['cr_amount'] = overall_data['cr_total_amount']
    overall_data['proportion'] = np.clip(overall_data['cr_amount'] / overall_data['sample_size'], 0, 1)
    
    print(f"  Careless responding proportions:")
    print(f"    Mean: {round(overall_data['proportion'].mean(), 4)}")
    print(f"    Median: {round(overall_data['proportion'].median(), 4)}")
    print(f"    Range: {round(overall_data['proportion'].min(), 4)} to {round(overall_data['proportion'].max(), 4)}")
    
    overall_data['analysis_approach'] = 'overall'
    
    return overall_data

def process_meta_data(df, diagnostics=None):
    """Process meta-analysis dataset: run checks and clean data."""
    print("\n== META-ANALYSIS DATA PROCESSING ==")
    
    if diagnostics is None:
        diagnostics = [
            check_basic_info,
            check_coding_distribution,
            check_missing_values,
            check_invalid_codes,
            check_logical_rules,
            clean_wave_data
        ]

    processed_df = df.copy()
    for diagnostic in diagnostics:
        if diagnostic.__name__ == 'clean_wave_data':
            processed_df = diagnostic(processed_df)
        else:
            diagnostic(processed_df)
    
    return processed_df

def process_data(data_path):
    """Process meta-analysis data and create datasets for analysis."""
    os.makedirs("data/processed", exist_ok=True)
    
    print(f"Loading data from {data_path}")
    data = pd.read_excel(data_path)
    data = add_unique_id(data, {})
    cleaned_data = process_meta_data(data)
    
    cleaned_data.to_csv("data/careless_data.csv", index=False)
    
    print("\n== PREPARING DATASETS FOR ANALYSIS ==")
    position_data = add_method_position_coding(cleaned_data)
    position_data = calculate_remaining_sample(position_data)
    
    first_method_data = create_first_method_dataset(position_data)
    single_method_data = create_single_method_dataset(position_data)
    sequential_data = create_sequential_dataset(position_data)
    overall_data = create_overall_dataset(position_data)
    
    first_method_data.to_csv("data/processed/first_method_data.csv", index=False)
    single_method_data.to_csv("data/processed/single_method_data.csv", index=False)
    overall_data.to_csv("data/processed/overall_data.csv", index=False) 
    
    if len(sequential_data) > 0:
        sequential_data.to_csv("data/processed/sequential_data.csv", index=False)
    
    print("\nAll datasets saved to data/processed/")
    print("  - first_method_data.csv: Primary analysis dataset (First-Method approach)")
    print("  - single_method_data.csv: Secondary analysis dataset (Single-Method only)")
    print("  - sequential_data.csv: Positional effects analysis dataset")
    print("  - overall_data.csv: Complete dataset using total CR amounts")
    
    return cleaned_data, first_method_data, single_method_data, sequential_data, overall_data

def main():
    data_path = "data/coded_study_data_all.xlsx"
    process_data(data_path)

if __name__ == "__main__":
    main()