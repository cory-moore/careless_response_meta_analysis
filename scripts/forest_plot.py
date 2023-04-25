
import pandas as pd
from scipy.stats import norm
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.table import Table
matplotlib.rcParams.update({'text.usetex': True})

from scripts.proportions_pooled import get_pooled_statistics


#----- Functions ------------------------------------------------------------------------------------------------------------------
def calculate_sampling_variances(p, n):
    """ Takes a proportion p and a sample size n as input and calculates the sampling variance. """
    return p * (1 - p) / n

def normalize_weights(weights, min_size=2, max_size=8):
    """Normalize weights to a given range."""
    norm_weights = (weights - min(weights)) / (max(weights) - min(weights))
    return min_size + norm_weights * (max_size - min_size)

def create_forest_plot(data, pooled_p, lower_ci, upper_ci):
    # Reverse the data
    data = data.iloc[::-1]
    fig, ax = plt.subplots(figsize=(9, len(data) * 0.2))  # Add extra space for the pooled results
    # y axis
    for i, row in data.iterrows():
        ax.errorbar(row['proportions_total'], len(data) - i, xerr=(row['ci_upper'] - row['ci_lower']) / 2, fmt='o', markersize=row['normalized_weight'], capsize=2, elinewidth=1.25, ecolor='black', color='black')
    ax.axvline(0, color='gray', linestyle='--', lw=0.5)
    yticklabels = ['{0:<2}: \\textbf{{{1:7.3f}}} ({2:7.3f} -{3:7.3f})'.format(row['ID'], row['proportions_total'], row['ci_lower'], row['ci_upper']) for _, row in data.iterrows()]
    ax.set_yticks(list(range(len(data) + 1)))  # Add an extra tick for the pooled results
    ax.set_yticklabels([r'Pooled Result: \textbf{{{:7.3f}}} ({:7.3f} - {:7.3f})'.format(pooled_p, lower_ci, upper_ci)] + yticklabels)
    ax.text(-0.35, len(data) + 1.5, r'\underline{\textbf{Study}}', fontsize=10, verticalalignment='top')
    ax.text(-0.25, len(data) + 1.5, r'\underline{\textbf{Proportions (95\%CI)}}', fontsize=10, verticalalignment='top')
    ax.yaxis.grid(True, linestyle='--', alpha=0.5, which='both') # Add grid to y-axis
    # Add the pooled results
    ax.errorbar(pooled_p, 0, xerr=(upper_ci - lower_ci) / 2, fmt='o', markersize=7, capsize=2, elinewidth=1.25, ecolor='red', color='red')
    ax.axvline(pooled_p, color='red', linestyle='--', lw=0.5)  # Add a vertical dashed line through the red circle
    # x axis
    ax.set_xlabel(r'\textbf{Proportion}', fontsize=12)
    ax.tick_params(axis='x', labelsize=12)
    ax.margins(x=0.03, y=0.03)
    plt.tight_layout(pad=2)
    plt.show()
    return fig

def create_funnel_plot(p, se):
    plt.figure(figsize=(12, 8))
    sns.scatterplot(data=data, x=p, y=se)
    plt.xlabel('Proportion of Careless Responding')
    plt.ylabel('Standard Error')
    plt.title('Funnel Plot')
    plt.show()


#----- Forest plot ------------------------------------------------------------------------------------------------------------------------
# prepare data
data = pd.read_excel('results/raw_proportions.xlsx', sheet_name='proportions_total')
p = data['proportions_total']
n = data['sample_size']

data['weight'] = calculate_sampling_variances(p, n) #NOTE: need to figure out whether to place this in the denominator like so: 1 / calculate_sampling_variances(p, n)
data['normalized_weight'] = normalize_weights(data['weight'])

# get pooled statistics
pooled_p, _, lower_ci, upper_ci, _ = get_pooled_statistics(p, n)

# create forest plot
plot = create_forest_plot(data, pooled_p, lower_ci, upper_ci)

# save plot
#plot.savefig('results/forest_plot_left.png', dpi=300, bbox_inches='tight')


#------ Funnel plot -------------------------------------------------------------------------
# prepare data
# se = data['se']
# var = se**2

# Create the funnel plot
#create_funnel_plot(p, se)