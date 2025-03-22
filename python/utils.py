import json

def load_codebook(path):

    with open(path, 'r') as f:
        codebook = json.load(f)
    
    # Convert string keys back to integers for compatibility with existing code
    for category, mapping in codebook.items():
        if category not in ['cr_method_type', 'cr_method_timing_map']:  # Skip these since they have string keys
            codebook[category] = {int(k): v for k, v in mapping.items()}
    
    return codebook