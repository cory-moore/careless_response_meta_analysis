
import pandas as pd

#----- Functions ------------------------------------------------------------------------------------------------------------------
def generate_unique_id(row, id_dict):
    first_name = row['authors'].split(',')[0].split(' ')[0]
    year = row['year']
    base_id = f"{first_name}{year}"
    id_dict[base_id] = id_dict.get(base_id, -1) + 1
    new_id = base_id + chr(ord('a') + id_dict[base_id])
    return new_id

#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_csv("data/raw_prelim_careless_data.csv")
rater1_data = pd.read_csv("data/rater1_data.csv", dtype={'ID': str})

# Add ID column
id_dict = {}
data['ID'] = data.apply(lambda row: generate_unique_id(row, id_dict), axis=1)

id_dict = {}
rater1_data['ID'] = rater1_data.apply(lambda row: generate_unique_id(row, id_dict), axis=1)

# Export
data.to_csv("data/prelim_careless_data.csv", index=False)
rater1_data.to_csv("data/rater1_data.csv", index=False)