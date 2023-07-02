
import pandas as pd 
import pingouin as pg

import warnings
warnings.filterwarnings("ignore", category=RuntimeWarning)


#----- Functions ------------------------------------------------------------------------------------------------------------------
def calculate_icc2k(data):
    """
    Calculate ICC2k for every column in data except ID and rater
    icc['ICC'][4] == ICC2k
    """
    icc2k_results = pd.DataFrame(columns=['column', 'ICC2k'])
    for column in data.columns:
        if column not in ['ID', 'rater']:
            icc = pg.intraclass_corr(data=data, targets='ID', raters='rater', ratings=column)
            icc2k = pd.DataFrame({'column': [column], 'ICC2k': [icc["ICC"][4]]})
            icc2k_results = pd.concat([icc2k_results, icc2k])
    icc2k_mean = icc2k_results['ICC2k'].mean()
    return icc2k_results, icc2k_mean


def main():
    rater0_data = pd.read_csv('data/prelim_careless_data.csv')
    rater0_data['rater'] = 0

    rater1_data = pd.read_csv('data/rater1_data.csv')
    rater1_data['rater'] = 1

    # combine data from both raters and only keep rows with a duplicate value in ID column
    # duplicate values correspond to studies that were coded by both raters
    data = pd.concat([rater0_data, rater1_data], ignore_index=True)
    data = (data[data
            .duplicated(subset='ID', keep=False)]
            .sort_values('ID')
            .drop(columns=['title', 'year','authors', 'journal', 'doi', 'Notes'])
            )

    # calculate ICC2k
    icc2k_results, icc2k_mean = calculate_icc2k(data)
if __name__ == "__main__":
    main()