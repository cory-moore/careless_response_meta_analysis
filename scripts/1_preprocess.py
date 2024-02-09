
import pandas as pd

def generate_unique_id(row, id_dict):
    first_name = row['authors'].split(',')[0].split(' ')[0]
    year = row['year']
    base_id = f"{first_name}{year}"
    id_dict[base_id] = id_dict.get(base_id, -1) + 1
    new_id = base_id + chr(ord('a') + id_dict[base_id])
    return new_id

def add_unique_id(df, id_dict):
    df['ID'] = df.apply(lambda row: generate_unique_id(row, id_dict), axis=1)
    return df


def main():

    data = pd.read_csv("data/raw_prelim_careless_data.csv")
    rater_data = pd.read_csv("data/rater1_data.csv", dtype={'ID': str})

    data = add_unique_id(data, {})
    rater_data = add_unique_id(rater_data, {})

    data.to_csv("data/prelim_careless_data.csv", index=False)
    rater_data.to_csv("data/rater1_data.csv", index=False)

if __name__ == "__main__":
    main()