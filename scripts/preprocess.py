
import pandas as pd

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


def main():

    data = pd.read_excel("/Users/corymoore/Library/CloudStorage/GoogleDrive-cmoore24@ncsu.edu/.shortcut-targets-by-id/1vWDqJojoig5JxYelOR5O7o-U-YbiHeqr/Professional work/doctoral_work/2_projects/Careless Meta/Data/coded_study_data.xlsx")
    
    # TODO: will need to expand this handle multiple raters
    rater_data = pd.read_excel("/Users/corymoore/Library/CloudStorage/GoogleDrive-cmoore24@ncsu.edu/.shortcut-targets-by-id/1vWDqJojoig5JxYelOR5O7o-U-YbiHeqr/Professional work/doctoral_work/2_projects/Careless Meta/Data/5_data extraction and coding/Madelyn/madelyn_code.xlsx")

    data = add_unique_id(data, {})
    rater_data = add_unique_id(rater_data, {})

    data.to_csv("data/careless_data.csv", index=False)
    rater_data.to_csv("data/rater_data.csv", index=False)

if __name__ == "__main__":
    main()