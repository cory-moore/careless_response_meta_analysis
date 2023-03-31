
import numpy as np
import pandas as pd
from scipy.stats import chi2
import matplotlib.pyplot as plt
from scripts.proportions_pooled import calculate_logit_transform
from scripts.proportions_pooled import get_pooled_statistics
from scripts.proportions_pooled import calculate_standard_errors
from scripts.proportions_pooled import calculate_pooled_confidence_interval

#----- Functions ------------------------------------------------------------------------------------------------------------------
#TODO: confirm these formulas are correct

def calculate_Q(p, w):
    """Calculate the weighted Q statistic for a given list of values and weights."""
    k = len(p)
    p_w = np.average(p, weights=w)
    Q = np.sum(w * np.square(p - p_w))
    df = k - 1
    p_value = 1 - chi2.cdf(Q, df)
    return Q, df, p_value

def calculate_i_squared(q_statistic, df, ci=0.95):
    alpha = 1 - ci
    quantile = chi2.ppf(1 - alpha, df)
    i_squared = (q_statistic - df) / q_statistic
    i_squared_lower = (q_statistic - df * quantile / (q_statistic + quantile)) / q_statistic
    i_squared_upper = (q_statistic - df * quantile / (q_statistic + quantile)) / q_statistic
    return i_squared, i_squared_lower, i_squared_upper

def calculate_h_squared(q_statistic, k):
    return q_statistic / (k - 1)

#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_excel('results/raw_proportions.xlsx', sheet_name='proportions_total')
p = data['proportions_total']
n = data['sample_size']

# calculate the pooled statistics for the total careless response proportions
pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)

# calculate the logit-transformed proportions for the total studies
logit_p_values = calculate_logit_transform(p)

# calculate the weighted Q statistic for the total studies
Q, df, p_value = calculate_Q(logit_p_values, n)
print(f'Q statistic: {Q:.4f}')
print(f'Degrees of freedom: {df}')
print(f'P-value: {p_value:.4f}')

# calculate I^2
i_squared, i_squared_lower, i_squared_upper = calculate_i_squared(Q, df)
print('I-squared: {:.2%}'.format(i_squared))
print('I-squared Confidence Interval: {:.2%} - {:.2%}'.format(i_squared_lower, i_squared_upper))

# calculate H^2
h_squared = calculate_h_squared(Q, len(p))
