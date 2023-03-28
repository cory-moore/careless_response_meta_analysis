
import numpy as np
import pandas as pd
from scipy.stats import chi2
from scipy.stats import iqr

#----- Functions ------------------------------------------------------------------------------------------------------------------
def calculate_cochran_q(p_values, n_values):
    weights = [1 / (p * (1 - p) / n) for p, n in zip(p_values, n_values)]
    sum_weights = sum(weights)
    pooled_var = 1 / sum_weights
    residuals = [(p - np.mean(p_values))**2 for p in p_values]
    sum_resid = sum(residuals)
    q_statistic = sum_resid / (pooled_var * (len(p_values) - 1))
    df = len(p_values) - 1
    p_value = 1 - chi2.cdf(q_statistic, df)
    return q_statistic, df, p_value

def calculate_i_squared(q_statistic, df, ci=0.95):
    alpha = 1 - ci
    quantile = chi2.ppf(1 - alpha, df)
    i_squared = (q_statistic - df) / q_statistic
    i_squared_lower = (q_statistic - df * quantile / (q_statistic + quantile)) / q_statistic
    i_squared_upper = (q_statistic - df * quantile / (q_statistic + quantile)) / q_statistic
    return i_squared, i_squared_lower, i_squared_upper

#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_excel('results/careless_response_proportions.xlsx', sheet_name='proportions_total')
p_values = data['proportions_total']
n_values = data['sample_size']

# calculate Cochran's Q
q_statistic, df, p_value = calculate_cochran_q(p_values, n_values)
df = len(p_values) - 1

# calculate I^2
i_squared, i_squared_lower, i_squared_upper = calculate_i_squared(q_statistic, df)

# print results
print('Cochrans Q test statistic: {:.2f}'.format(q_statistic))
print('Cochrans Q Degrees of Freedom: {}'.format(df))
print('Cochrans Q p-value: {:.4f}'.format(p_value))
print('I-squared: {:.2%}'.format(i_squared))
print('I-squared Confidence Interval: {:.2%} - {:.2%}'.format(i_squared_lower, i_squared_upper))
