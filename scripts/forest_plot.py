
import pandas as pd
from scipy.stats import norm
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt
matplotlib.rcParams.update({'text.usetex': True})

from scripts.proportions_pooled import get_pooled_statistics


#----- Functions ------------------------------------------------------------------------------------------------------------------
def normalize_weights(weights, min_size=3, max_size=7):
    """Normalize weights to a given range."""
    norm_weights = (weights - min(weights)) / (max(weights) - min(weights))
    return min_size + norm_weights * (max_size - min_size)

def create_forest_plot(data, pooled_p, lower_ci, upper_ci):
    # Reverse the data
    data = data.iloc[::-1]
    fig, ax = plt.subplots(figsize=(10, len(data) * 0.2))  # Add extra space for the pooled results
    #ax.set_title(r'\textbf{Forest Plot}', fontsize=16)
    # y axis
    for i, row in data.iterrows():
        ax.errorbar(row['proportions_total'], len(data) - i, xerr=(row['ci_upper'] - row['ci_lower']) / 2, fmt='o', markersize=row['normalized_weight'], capsize=2, elinewidth=1.25, ecolor='black', color='black')
    ax.axvline(0, color='gray', linestyle='--', lw=0.5)
    ax.set_yticks(list(range(len(data) + 1)))  # Add an extra tick for the pooled results
    ax.set_yticklabels(['Pooled Prevalence'] + data['ID'].tolist())
    ax.text(-0.067, len(data) + 1.5, r'\underline{\textbf{Study}}', fontsize=10, verticalalignment='top')
    ax.yaxis.grid(True, linestyle='--', alpha=0.5, which='both') # Add grid to y-axis
    # Add the pooled results
    ax.errorbar(pooled_p, 0, xerr=(upper_ci - lower_ci) / 2, fmt='o', markersize=7, capsize=2, elinewidth=1.25, ecolor='red', color='red')
    ax.axvline(pooled_p, color='red', linestyle='--', lw=0.5)  # Add a vertical dashed line through the red circle
    # y axis 2
    ax2 = ax.twinx()
    ax2.set_yticks(list(range(len(data) + 1)))  # Add an extra tick for the pooled results
    yticklabels = [] + (data['proportions_total'].round(3).astype(str) + ' (' + data['ci_lower'].round(3).astype(str) + '-' + data['ci_upper'].round(3).astype(str) + ')').tolist()
    yticklabels.insert(0, str(round(pooled_p, 3)) + ' (' + str(round(lower_ci, 3)) + '-' + str(round(upper_ci, 3)) + ')')
    ax2.set_yticklabels(yticklabels)
    ax2.yaxis.tick_right()
    ax2.set_ylim(ax.get_ylim())
    ax2.text(0.535, len(data) + 1.5, r'\underline{\textbf{Proportion (95\% CI)}}', fontsize=10, verticalalignment='top')
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

#----- Main ------------------------------------------------------------------------------------------------------------------------
# prepare data
data = pd.read_excel('results/raw_proportions.xlsx', sheet_name='proportions_total')
data['weight'] = 1 / (data['proportions_total'] * (1 - data['proportions_total']) / data['sample_size'])
data['normalized_weight'] = normalize_weights(data['weight'])
p = data['proportions_total']
n = data['sample_size']
se = data['se']
var = se**2

# get pooled statistics
pooled_p, _, lower_ci, upper_ci, _ = get_pooled_statistics(p, n)

# create forest plot
plot = create_forest_plot(data, pooled_p, lower_ci, upper_ci)

# save plot
plot.savefig('results/forest_plot.png', dpi=300, bbox_inches='tight')


# Create the funnel plot
create_funnel_plot(p, se)