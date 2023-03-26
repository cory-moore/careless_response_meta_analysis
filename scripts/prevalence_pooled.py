import math
import pandas as pd
import numpy as np
from scipy.stats import norm

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

#----- Main ------------------------------------------------------------------------------------------------------------------------
# read in each sheet from the rates files as an individual dataframe
rates_total = pd.read_excel('results/careless_response_rates.xlsx', sheet_name='cr_rates_total')

# Step 1: Calculate effect sizes (proportions) and their standard errors
p = rates_total['cr_rate_total']
n = rates_total['sample_size']

# Step 2: Calculate the Standard Error
se = calculate_standard_errors(p, n)

# Step 3: Calculate the Logit Transform
logit_p = calculate_logit_transform(p)

# Step 4: Calculate the Sampling Variance
sampling_variances = calculate_sampling_variances(logit_p, n)

# Step 5: Calculate Pooled Prevalence
pooled_p = calculate_pooled_prevalence(logit_p, n)

# Step 6: Back Transform Pooled Prevalence
pooled_p = back_transform(pooled_p)

# Step 7: Calculate Pooled Standard Error
pooled_se = calculate_pooled_standard_error(p, n)

# Step 8: Calculate Pooled Confidence Interval
lower_bound, upper_bound = calculate_pooled_confidence_interval(pooled_p, pooled_se)

# Step 9: Back Transform Pooled Confidence Interval
lower_ci, upper_ci = back_transform_confidence_interval(lower_bound, upper_bound)

# Step 10: Print Results
print('Pooled Prevalence: {:.2%}'.format(pooled_p))
print('Pooled Standard Error: {:.2%}'.format(pooled_se))
print('Pooled Confidence Interval: {:.2%} - {:.2%}'.format(lower_ci, upper_ci))

