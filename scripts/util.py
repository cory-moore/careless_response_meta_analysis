


def add_cr_method_type(data):
    cr_columns = ['cr_1_method', 'cr_2_method', 'cr_3_method', 'cr_4_method']
    cr_method_type_map = {
        'response_time': [1],
        'outlier_analysis': [4, 5],
        'bogus_items': [0],
        'consistency_indices': [9, 10, 11],
        'response_pattern': [2, 3],
        'self-reported': [6, 7, 8]
    }
    # Loop over the cr_columns and add a new column for each cr_method type
    for col in cr_columns:
        new_col = col + '_type'
        data[new_col] = data[col].apply(lambda x: next((k for k, v in cr_method_type_map.items() if x in v), None))