import pandas as pd
from codebook import codebook

def generate_unique_id(row, id_dict):
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
    df['ID'] = df.apply(lambda row: generate_unique_id(row, id_dict), axis=1)
    return df

def check_basic_info(df):
    """Basic dataset information."""
    print("\nBASIC INFORMATION:")
    print(f"  Total Studies: {len(df)}")
    print(f"  Unique Papers: {df['title'].nunique()}")

def check_coding_distribution(df):
    """Distribution of studies across coders."""
    print("\nCODING DISTRIBUTION:")
    for coder, count in df['coder'].value_counts().items():
        print(f"  {coder}: {count} studies")

def check_missing_values(df):
    """Dataset structure and missing values."""
    print(f"\nMISSING VALUES:")
    missing_found = False
    for col in df.columns:
        if col != 'Notes' and df[col].isna().sum() > 0:
            missing_found = True
            print(f"    {col} (missing: {df[col].isna().sum()})")
    if not missing_found:
        print("    No missing values found!")

def check_invalid_codes(df):
    """
    Check for invalid coding values, considering variable-specific rules.
    """
    print("\nINVALID CODE CHECKS:")
    
    # Variables where -1 is NOT acceptable
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
            print(f"\n  {message}:")
            for _, row in invalid.iterrows():
                print(f"    Study: {row['title']}")
                print(f"      Current value: {row[var]}")
    
    if not invalid_found:
        print("    No invalid codes found!")

def clean_wave_data(df):
    """
    Clean longitudinal studies based on CR screening approach.
    """
    print("\nCLEANING WAVE DATA:")
    clean_df = df.copy()
    
    # Step 1: Identify and remove final wave screen studies
    final_wave_screen = clean_df[clean_df['sample_time'] > 1].copy()
    removed_step1 = []
    if len(final_wave_screen) > 0:
        for title in final_wave_screen['title'].unique():
            study_data = clean_df[clean_df['title'] == title]
            # If we don't have a wave 1, it's a final wave screen
            if 1 not in study_data['sample_time'].values:
                rows_to_remove = len(study_data)
                print(f"\n  Removing {rows_to_remove} rows (final wave screening only):")
                print(f"    {title}")
                # Remove this study
                clean_df = clean_df[clean_df['title'] != title]
                removed_step1.append(title)
    
    removed_first = len(df) - len(clean_df)
    if removed_first > 0:
        print(f"\n  Step 1: Removed {removed_first} total rows from {len(removed_step1)} studies")
    
    # Step 2: For remaining studies with multiple waves, keep only first wave
    wave_studies = clean_df[clean_df['title'].duplicated(keep=False)]
    removed_step2 = []
    if len(wave_studies) > 0:
        for title in wave_studies['title'].unique():
            # Create a proper copy for modification
            waves = wave_studies[wave_studies['title'] == title].copy()
            
            # Extract base study number without modifying the DataFrame
            base_numbers = waves['study_number'].astype(str).str.extract('(\d+)')[0]
            
            # Process each base study number separately
            for base_num in base_numbers.unique():
                study_waves = waves[base_numbers == base_num]
                
                # If all sample_times are 1 and design_time is not 1,
                # these are likely separate samples, not waves
                if (study_waves['sample_time'].nunique() == 1 and 
                    study_waves['sample_time'].iloc[0] == 1 and 
                    1 not in study_waves['design_time'].values):
                    continue
                
                # If we're going to remove waves, print info
                if (1 in study_waves['design_time'].values or study_waves['sample_time'].nunique() > 1):
                    rows_to_remove = len(study_waves) - 1  # keeping first wave
                    print(f"\n  Removing {rows_to_remove} rows (later waves):")
                    print(f"    {title} (Study {base_num})")
                    removed_step2.append(title)
                
                # If design_time = 1, treat as waves regardless of sample_time
                if 1 in study_waves['design_time'].values:
                    clean_df = clean_df[
                        ~((clean_df['title'] == title) & 
                          (clean_df['study_number'].astype(str).str.startswith(base_num)) &
                          (clean_df['sample_time'] > study_waves['sample_time'].min()))
                    ]
                # Otherwise, use sample_time to determine waves
                elif study_waves['sample_time'].nunique() > 1:
                    clean_df = clean_df[
                        ~((clean_df['title'] == title) & 
                          (clean_df['study_number'].astype(str).str.startswith(base_num)) &
                          (clean_df['sample_time'] > study_waves['sample_time'].min()))
                    ]
    
    removed_second = len(df) - len(clean_df) - removed_first
    if removed_second > 0:
        print(f"\n  Step 2: Removed {removed_second} total rows from {len(removed_step2)} studies")
    
    total_removed = removed_first + removed_second
    if total_removed > 0:
        print(f"\n  Summary: Removed {total_removed} total rows")
        print(f"           ({len(removed_step1) + len(removed_step2)} studies affected)")
    
    return clean_df

def check_logical_rules(df):
    """
    Check for violations of logical relationships between variables.
    """
    print("\nLOGICAL CONSISTENCY CHECKS:")
    
    # First check codebook values
    codebook_errors = False
    for col, valid_values in codebook.items():
        if col != 'journal' and col in df.columns:
            invalid = df[~df[col].isin(valid_values.keys())]
            if len(invalid) > 0:
                codebook_errors = True
                print(f"\n  Invalid {col} values:")
                for _, row in invalid.iterrows():
                    print(f"    Study: {row['title']}")
                    print(f"      Value: {row[col]}")
    if not codebook_errors:
        print("\n  ✓ All codebook values are valid")
    
    # Then check other logical rules
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
            'message': 'Single method studies should have -1 for sequential flag and all additional methods/amounts'
        },
        {
            'name': 'Multiple Methods Missing Second Method',
            'condition': lambda x: (x['cr_multiple'] == 1) & (x['cr_2_method'] == -1),
            'message': 'Multiple methods indicated but no second method recorded'
        },
        {
            'name': 'Sequential Reporting Missing Amounts',
            'condition': lambda x: (x['cr_sequential'] == 1) & (
                ((x['cr_1_method'] != -1) & (x['cr_1_amount'] == -1)) |
                ((x['cr_2_method'] != -1) & (x['cr_2_amount'] == -1)) |
                ((x['cr_3_method'] != -1) & (x['cr_3_amount'] == -1)) |
                ((x['cr_4_method'] != -1) & (x['cr_4_amount'] == -1))
            ),
            'message': 'Sequential reporting requires amounts for each method used'
        },
        {
            'name': 'Non-sequential Missing Total',
            'condition': lambda x: (x['cr_multiple'] == 1) & 
                                 (x['cr_sequential'] == 0) & 
                                 (x['cr_total_amount'] == -1),
            'message': 'Total CR amount must be specified even for non-sequential reporting'
        },
        # Add CR proportion rules
        {
            'name': 'Impossible CR Proportions',
            'condition': lambda x: (x['cr_total_amount'] > x['sample_size']) & 
                                 (x['cr_total_amount'] != -1) & 
                                 (x['sample_size'] != -1),
            'message': 'CR amount cannot exceed sample size'
        },
        {
            'name': 'Implausible CR Proportions',
            'condition': lambda x: ((x['cr_total_amount'] / x['sample_size']) > 0.50) & 
                                 (x['cr_total_amount'] <= x['sample_size']) &
                                 (x['cr_total_amount'] != -1) & 
                                 (x['sample_size'] != -1),
            'message': 'CR proportion exceeds 50% of sample'
        },
        # Wave-related consistency checks
        {
            'name': 'Wave Indicator Consistency',
            'condition': lambda x: (
                # Only flag if there's no evidence of waves in related studies
                ((x['design_time'] == 1) & 
                 (x['sample_time'] == 1) & 
                 ~x.groupby('title')['sample_time'].transform(lambda x: x.max() > 1)) |
                # Only flag if there's no repeated measures design in any related study
                ((x['sample_time'] > 1) & 
                 (x['design_time'] != 1) & 
                 ~x.groupby('title')['design_time'].transform(lambda x: 1 in x.values))
            ),
            'message': 'Inconsistent wave indicators found:'
        },
        
        # Study number format check
        {
            'name': 'Study Number Format',
            'condition': lambda x: (
                (x['sample_time'] > 1) & 
                ~x['study_number'].astype(str).str.contains('[a-zA-Z]')
            ),
            'message': 'Multiple waves should have letter-based study numbers (e.g., 1a, 1b)'
        }
    ]
    
    for rule in rules:
        if rule['name'] == 'Wave Indicator Consistency':
            violations = df[rule['condition'](df)]
            if len(violations) > 0:
                print(f"\n  {rule['name']} violation:")
                print(f"    {rule['message']}")
                
                for _, row in violations.iterrows():
                    # Get all studies from this paper
                    related_studies = df[df['title'] == row['title']]
                    print(f"\n    Study: {row['title']}")
                    print(f"      Current study:")
                    print(f"        Sample time: {row['sample_time']}")
                    print(f"        Study number: {row['study_number']}")
                    print(f"        Design time: {row['design_time']}")
                    print(f"      All related studies (sample_time, study_number):")
                    for _, rel_row in related_studies.iterrows():
                        print(f"        {rel_row['sample_time']}, {rel_row['study_number']}")
                    
                    # Explain the specific inconsistency
                    if row['design_time'] == 1 and row['sample_time'] == 1:
                        print("      Issue: Study is marked as repeated measures but no later waves "
                              "found in any related studies (no sample_time > 1)")
                    
                    elif row['sample_time'] > 1 and row['design_time'] != 1:
                        print("      Issue: Study has later waves but no repeated measures design "
                              "found in any related studies (no design_time = 1)")
            else:
                print(f"\n  ✓ Wave indicators are consistent")
        
        else:
            violations = df[rule['condition'](df)]
            if len(violations) > 0:
                print(f"\n  {rule['name']} violation:")
                print(f"    Rule: {rule['message']}")
                for _, row in violations.iterrows():
                    print(f"    Study: {row['title']}")
            else:
                print(f"\n  ✓ {rule['name']} check passed")

def process_meta_data(df, diagnostics=None):
    """
    Process meta-analysis dataset: describe, check, and clean data.
    
    Parameters:
    -----------
    df : pandas.DataFrame
        The meta-analysis dataset
    diagnostics : list, optional
        List of diagnostic/cleaning functions to run. If None, runs all.
    
    Returns:
    --------
    pandas.DataFrame
        Processed and cleaned dataset
    """
    print("\n########################################################")
    print("\nMETA-ANALYSIS DATA PROCESSING")
    print("============================")
    
    # Default diagnostics if none specified
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
    # Run specified diagnostics/cleaning
    for diagnostic in diagnostics:
        if diagnostic.__name__ == 'clean_wave_data':
            processed_df = diagnostic(processed_df)
        else:
            diagnostic(processed_df)
    
    print("\n########################################################")
    return processed_df

def process_data(data_path):
    """Main data processing function for meta-analysis data only"""
    data = pd.read_excel(data_path)
    data = add_unique_id(data, {})
    cleaned_data = process_meta_data(data)
    cleaned_data.to_csv("data/careless_data.csv", index=False)
    
    return cleaned_data

def main():
    data_path = "data/coded_study_data_all.xlsx"
    process_data(data_path)

if __name__ == "__main__":
    main()