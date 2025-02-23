
import pandas as pd

from src.data_processing import *

def main():

    data_path = "/Users/corymoore/Library/CloudStorage/GoogleDrive-cmoore24@ncsu.edu/.shortcut-targets-by-id/1vWDqJojoig5JxYelOR5O7o-U-YbiHeqr/Professional work/doctoral_work/2_projects/Careless Meta/Data/coded_study_data_all.xlsx"
    data = pd.read_excel(data_path)
    data = add_unique_id(data, {})
    cleaned_data = process_meta_data(data)
    cleaned_data.to_csv("data/careless_data.csv", index=False)

    rater_data_path = "/Users/corymoore/Library/CloudStorage/GoogleDrive-cmoore24@ncsu.edu/.shortcut-targets-by-id/1vWDqJojoig5JxYelOR5O7o-U-YbiHeqr/Professional work/doctoral_work/2_projects/Careless Meta/Data/shared_studies_coded.xlsx"
    rater_data = combine_excel_sheets(rater_data_path)
    check_rater_coverage(rater_data)
    rater_data = add_unique_id(rater_data, {})
    rater_data.to_csv("data/rater_data.csv", index=False)

if __name__ == "__main__":
    main()