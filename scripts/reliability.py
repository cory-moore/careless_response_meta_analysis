
import pandas as pd 

#----- Functions ------------------------------------------------------------------------------------------------------------------







#----- Main ------------------------------------------------------------------------------------------------------------------------
rater0_data = pd.read_csv('data/prelim_careless_data.csv')
rater0_data['rater'] = 0

rater1_data = pd.read_csv('data/rater1_code.csv')
rater1_data['rater'] = 1

data = pd.concat([rater0_data, rater1_data], ignore_index=True)

# only retain rows with a duplicate value in ID column
data = (data[data
        .duplicated(subset='ID', keep=False)]
        .sort_values('ID')
        .drop(columns=['title', 'year','authors', 'journal', 'doi', 'Notes','total_items'])
        )

