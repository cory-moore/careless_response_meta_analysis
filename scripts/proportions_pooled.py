
import math
import pandas as pd
import numpy as np
from scipy.stats import norm


def calculate_logit_transform(p):
    """ Takes a proportion p as input and calculates the logit transformation of the proportion."""
    return np.log(p / (1 - p))

def calculate_logit_variance(p, n):
    """ 
    Takes a raw proportion p and a sample size n as input and calculates the variance of the logit transformed proportion. 
    The formula is derived from the properties of the logit transformation. 
    The logit variance formula calculates the variance of the logit-transformed proportion without needing to input the logit-transformed proportions directly.
    """
    return 1 / (n * p) + 1 / (n * (1 - p))

def calculate_pooled_prevalence(logit_p, n, p):
    """ Takes a logit transformed proportion logit_p, a sample size n, and a proportion p as input and calculates the pooled prevalence """
    weights = 1 / calculate_logit_variance(p, n) # inverse variance method
    numerator = (logit_p * weights).sum()
    denominator = weights.sum()
    pooled_p = numerator / denominator
    return pooled_p

def back_transform(pooled_p):
    """ Takes a pooled prevalence pooled_p as input and back transforms it to a proportion """
    back_transformed_p = np.exp(pooled_p) / (1 + np.exp(pooled_p))
    back_transformed_p = round(back_transformed_p, 4)
    return back_transformed_p 

def calculate_pooled_standard_error(p_values, n_values):
    #NOTE: I believe this is the correct approach
    """ Takes a list of proportions p_values and a list of sample sizes n_values and calculates the pooled standard error """
    weights = [1 / calculate_logit_variance(p, n) for p, n in zip(p_values, n_values)]
    sum_weights = sum(weights)
    pooled_var = 1 / sum_weights
    se_pooled = math.sqrt(pooled_var)
    se_pooled = round(se_pooled, 4)
    return se_pooled

def calculate_pooled_confidence_interval(pooled_p, pooled_se, alpha=0.05):
    """ Takes a pooled prevalence pooled_p, a pooled standard error pooled_se, and an optional alpha value and calculates the pooled confidence interval """
    z_crit = norm.ppf(1 - alpha / 2)
    me = z_crit * pooled_se
    lower_bound = pooled_p - me
    upper_bound = pooled_p + me
    return lower_bound, upper_bound

def back_transform_confidence_interval(lower_bound, upper_bound):
    """ Takes the lower and upper bounds of a confidence interval as input and back transforms them to proportions """
    lower_ci = np.exp(lower_bound) / (1 + np.exp(lower_bound))
    upper_ci = np.exp(upper_bound) / (1 + np.exp(upper_bound))
    lower_ci = round(lower_ci, 4)
    upper_ci = round(upper_ci, 4)
    return lower_ci, upper_ci

def get_pooled_statistics(p, n):
    """ Takes a list of proportions p and a list of sample sizes n and calculates the pooled prevalence and its confidence interval. """
    logit_p = calculate_logit_transform(p)
    pooled_p_logit = calculate_pooled_prevalence(logit_p, n, p) #NOTE: have to adjust this according to correct pooled functions above
    pooled_p = back_transform(pooled_p_logit)
    pooled_se = calculate_pooled_standard_error(p, n)
    lower_bound, upper_bound = calculate_pooled_confidence_interval(pooled_p_logit, pooled_se)  # Use logit transformed pooled_p
    lower_ci, upper_ci = back_transform_confidence_interval(lower_bound, upper_bound)
    n_sum = sum(n)
    return pooled_p, pooled_se, lower_ci, upper_ci, n_sum


def main():
    # # read in careless response proportions for total studies
    proportions_total = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_total')
    p = proportions_total['proportions_total']
    n = proportions_total['sample_size']

    # create an empty DataFrame with the desired columns
    columns = ['Group', 'Subgroup', 'Pooled Prevalence', 'Pooled Standard Error',
            'Lower CI', 'Upper CI', 'Pooled Sample Size', 'Number of Studies']
    pooled_results = pd.DataFrame(columns=columns)

    # # calculate the pooled statistics for the total careless response proportions
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    total_row = ['Total', '', pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_total)]
    pooled_results.loc[len(pooled_results)] = total_row

    # # read in careless response proportions by year
    proportions_year = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_year')

    # create a subset df for each year and calculate the pooled statistics
    for year in proportions_year['year'].unique():
        proportions_year_subset = proportions_year[proportions_year['year'] == year]
        p = proportions_year_subset['proportions_year']
        n = proportions_year_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        year_row = ['Year', year, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_year_subset)]
        pooled_results.loc[len(pooled_results)] = year_row

    # # read in careless response proportions by journal
    proportions_journal = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_journal')
    # create a subset df for each journal and calculate the pooled statistics
    for journal in proportions_journal['journal_name'].unique():
        proportions_journal_subset = proportions_journal[proportions_journal['journal_name'] == journal]
        p = proportions_journal_subset['proportions_journal']
        n = proportions_journal_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        journal_row = ['Journal', journal, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_journal_subset)]
        pooled_results.loc[len(pooled_results)] = journal_row

    # # read in careless response proportions by sample source
    proportions_sample_source = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_sample_source')
    # create a subset df for each sample source and calculate the pooled statistics
    for source in proportions_sample_source['sample_source_name'].unique():
        proportions_source_subset = proportions_sample_source[proportions_sample_source['sample_source_name'] == source]
        p = proportions_source_subset['proportions_sample_source']
        n = proportions_source_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        source_row = ['Sample Source', source, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_source_subset)]
        pooled_results.loc[len(pooled_results)] = source_row

    # # read in careless response proportions by sample method
    proportions_sample_method = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_sample_method')
    # create a subset df for each sample method and calculate the pooled statistics
    for method in proportions_sample_method['sample_method_name'].unique():
        proportions_method_subset = proportions_sample_method[proportions_sample_method['sample_method_name'] == method]
        p = proportions_method_subset['proportions_sample_method']
        n = proportions_method_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        method_row = ['Sample Method', method, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_method_subset)]
        pooled_results.loc[len(pooled_results)] = method_row

    # # read in careless response proportions by sample platform
    proportions_sample_platform = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_platform')
    # create a subset df for each sample platform and calculate the pooled statistics
    for platform in proportions_sample_platform['sample_platform_name'].unique():
        proportions_platform_subset = proportions_sample_platform[proportions_sample_platform['sample_platform_name'] == platform]
        p = proportions_platform_subset['proportions_platform']
        n = proportions_platform_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        platform_row = ['Sample Platform', platform, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_platform_subset)]
        pooled_results.loc[len(pooled_results)] = platform_row

    # # read in careless response proportions by careless response method
    proportions_cr_method = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_cr_method')
    # create a subset df for each careless response method and calculate the pooled statistics
    for cr_method in proportions_cr_method['cr_method_name'].unique():
        proportions_cr_method_subset = proportions_cr_method[proportions_cr_method['cr_method_name'] == cr_method]
        p = proportions_cr_method_subset['proportions_cr_method']
        n = proportions_cr_method_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        cr_method_row = ['Careless Response Method', cr_method, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_cr_method_subset)]
        pooled_results.loc[len(pooled_results)] = cr_method_row

    # # read in careless response proportions by careless response type
    proportions_cr_type = pd.read_excel('results/proportions.xlsx', sheet_name='proportions_cr_type')
    # create a subset df for each careless response type and calculate the pooled statistics
    for cr_type in proportions_cr_type['cr_method_type'].unique():
        proportions_cr_type_subset = proportions_cr_type[proportions_cr_type['cr_method_type'] == cr_type]
        p = proportions_cr_type_subset['proportions_cr_type']
        n = proportions_cr_type_subset['sample_size']
        pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
        cr_type_row = ['Careless Response Type', cr_type, pooled_p, pooled_se, lower_ci, upper_ci, n_sum, len(proportions_cr_type_subset)]
        pooled_results.loc[len(pooled_results)] = cr_type_row


    # # export the pooled results to a csv
    pooled_results.to_csv('results/pooled_proportions.csv', index=False)
    
if __name__ == '__main__':
    main()