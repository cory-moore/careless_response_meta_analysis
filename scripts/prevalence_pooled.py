import math
import pandas as pd
import numpy as np
from scipy.stats import norm

#TODO: fix pooled CIs, currently the range is incorrect

#----- Functions ------------------------------------------------------------------------------------------------------------------
def calculate_standard_errors(p, n):
    """Calculate standard errors for a given proportion and sample size."""
    return np.sqrt(p * (1 - p) / n)

def calculate_logit_transform(p):
    """Calculate the logit transform for a given proportion."""
    return np.log(p / (1 - p))

def calculate_sampling_variances(p, n):
    """Calculate sampling variances for a given proportion and sample size."""
    return p * (1 - p) / n

def calculate_pooled_prevalence(logit_p, n):
    """Calculate the pooled prevalence for a given logit transformed proportion and sample size."""
    weights = [1 / (p * (1 - p) / n) for p, n in zip(logit_p, n)]
    numerator = sum([p / (p * (1 - p) / n) for p, n in zip(logit_p, n)])
    denominator = sum(weights)
    pooled_p = numerator / denominator
    return pooled_p

def back_transform(pooled_p):
    """Back transform a pooled prevalence to a proportion."""
    return np.exp(pooled_p) / (1 + np.exp(pooled_p))

def calculate_pooled_standard_error(p_values, n_values):
    """Calculate the pooled standard error for a given list of proportions and sample sizes."""
    weights = [1 / (p * (1 - p) / n) for p, n in zip(p_values, n_values)]
    sum_weights = sum(weights)
    pooled_var = 1 / sum_weights
    se_pooled = math.sqrt(pooled_var)
    return se_pooled

def calculate_pooled_confidence_interval(pooled_p, pooled_se, alpha=0.05):
    """Calculate the pooled confidence interval for a given pooled prevalence and standard error."""
    z_crit = norm.ppf(1 - alpha / 2)
    me = z_crit * pooled_se
    lower_bound = pooled_p - me
    upper_bound = pooled_p + me
    return lower_bound, upper_bound

def back_transform_confidence_interval(lower_bound, upper_bound):
    """Back transform a confidence interval to proportions."""
    lower_ci = np.exp(lower_bound) / (1 + np.exp(lower_bound))
    upper_ci = np.exp(upper_bound) / (1 + np.exp(upper_bound))
    return lower_ci, upper_ci

def get_pooled_statistics(p, n):
    """Calculate the pooled prevalence and its confidence interval for a given list of proportions and sample sizes."""
    logit_p = calculate_logit_transform(p)
    pooled_p = calculate_pooled_prevalence(logit_p, n)
    pooled_p = back_transform(pooled_p)
    pooled_se = calculate_pooled_standard_error(p, n)
    lower_bound, upper_bound = calculate_pooled_confidence_interval(pooled_p, pooled_se)
    lower_ci, upper_ci = back_transform_confidence_interval(lower_bound, upper_bound)
    n_sum = sum(n)
    return pooled_p, pooled_se, lower_ci, upper_ci, n_sum
    

#----- Main ------------------------------------------------------------------------------------------------------------------------
# # read in careless response rates for total studies
rates_total = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_total')
p = rates_total['cr_rate_total']
n = rates_total['sample_size']

# calculate the pooled statistics for the total careless response rates
pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
print('Pooled Prevalence: {:.2%}'.format(pooled_p))
print('Pooled Standard Error: {:.2%}'.format(pooled_se))
print('Pooled Confidence Interval: {:.2%} - {:.2%}'.format(lower_ci, upper_ci))
print('Pooled Sample Size: {}'.format(n_sum))


# # read in careless response rates by year
rates_year = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_year')

# create a subset df for each year and calculate the pooled statistics
for year in rates_year['year'].unique():
    rates_year_subset = rates_year[rates_year['year'] == year]
    p = rates_year_subset['cr_rate_year']
    n = rates_year_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(year, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(year, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(year, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(year, n_sum))
    print('Number of studies for {}: {}'.format(year, len(rates_year_subset)))
    print('')


# # read in careless response rates by journal
rates_journal = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_journal')
# create a subset df for each journal and calculate the pooled statistics
for journal in rates_journal['journal_name'].unique():
    rates_journal_subset = rates_journal[rates_journal['journal_name'] == journal]
    p = rates_journal_subset['cr_rate_journal']
    n = rates_journal_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(journal, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(journal, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(journal, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(journal, n_sum))
    print('Number of studies for {}: {}'.format(journal, len(rates_journal_subset)))
    print('')

# # read in careless response rates by sample source
rates_sample_source = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_source')
# create a subset df for each sample source and calculate the pooled statistics
for source in rates_sample_source['sample_source'].unique():
    rates_source_subset = rates_sample_source[rates_sample_source['sample_source'] == source]
    p = rates_source_subset['cr_rate_source']
    n = rates_source_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(source, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(source, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(source, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(source, n_sum))
    print('Number of studies for {}: {}'.format(source, len(rates_source_subset)))
    print('')

# # read in careless response rates by sample method
rates_sample_method = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_method')
# create a subset df for each sample method and calculate the pooled statistics
for method in rates_sample_method['sample_method_name'].unique():
    rates_method_subset = rates_sample_method[rates_sample_method['sample_method_name'] == method]
    p = rates_method_subset['cr_rate_method']
    n = rates_method_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(method, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(method, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(method, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(method, n_sum))
    print('Number of studies for {}: {}'.format(method, len(rates_method_subset)))
    print('')

# # read in careless response rates by sample platform
rates_sample_platform = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_sample_platform')
# create a subset df for each sample platform and calculate the pooled statistics
for platform in rates_sample_platform['sample_platform_name'].unique():
    rates_platform_subset = rates_sample_platform[rates_sample_platform['sample_platform_name'] == platform]
    p = rates_platform_subset['cr_rate_platform']
    n = rates_platform_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(platform, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(platform, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(platform, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(platform, n_sum))
    print('Number of studies for {}: {}'.format(platform, len(rates_platform_subset)))
    print('')

# # read in careless response rates by careless response method
rates_cr_method = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_method')
# create a subset df for each careless response method and calculate the pooled statistics
for cr_method in rates_cr_method['cr_method_name'].unique():
    rates_cr_method_subset = rates_cr_method[rates_cr_method['cr_method_name'] == cr_method]
    p = rates_cr_method_subset['cr_rate']
    n = rates_cr_method_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(cr_method, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(cr_method, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(cr_method, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(cr_method, n_sum))
    print('Number of studies for {}: {}'.format(cr_method, len(rates_cr_method_subset)))
    print('')

# # read in careless response rates by careless response type
rates_cr_type = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_by_method_type')
# create a subset df for each careless response type and calculate the pooled statistics
for cr_type in rates_cr_type['cr_method_type'].unique():
    rates_cr_type_subset = rates_cr_type[rates_cr_type['cr_method_type'] == cr_type]
    p = rates_cr_type_subset['cr_rate']
    n = rates_cr_type_subset['sample_size']
    pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)
    print('Pooled Prevalence for {}: {:.2%}'.format(cr_type, pooled_p))
    print('Pooled Standard Error for {}: {:.2%}'.format(cr_type, pooled_se))
    print('Pooled Confidence Interval for {}: {:.2%} - {:.2%}'.format(cr_type, lower_ci, upper_ci))
    print('Pooled Sample Size for {}: {}'.format(cr_type, n_sum))
    print('Number of studies for {}: {}'.format(cr_type, len(rates_cr_type_subset)))
    print('')