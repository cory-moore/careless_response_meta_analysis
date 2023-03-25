
import meta
import pandas as pd

#----- Functions ------------------------------------------------------------------------------------------------------------------




#----- Main ------------------------------------------------------------------------------------------------------------------------
# read in each sheet from the excel as an individual dataframe
rates_total = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_total')
rates_by_year = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_year')
rates_by_journal = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_journal')
rates_by_source = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_source')
rates_by_method = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_method')
rates_by_platform = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_platform')
rates_by_cr_method = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_method')
rates_by_cr_type = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_method_type')

rates_total.head(50)




