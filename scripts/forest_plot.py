
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
matplotlib.rcParams.update({'text.usetex': True})

from scripts.proportions_pooled import get_pooled_statistics


#----- Functions ------------------------------------------------------------------------------------------------------------------
def normalize_weights(weights, min_size=3, max_size=7):
    norm_weights = (weights - min(weights)) / (max(weights) - min(weights))
    return min_size + norm_weights * (max_size - min_size)

def create_forest_plot(data):
    fig, ax = plt.subplots(figsize=(10, len(data) * 0.2))  # Add extra space for the pooled results
    # plot title
    ax.set_title(r'\textbf{Forest Plot}', fontsize=16)
    # y axis
    for i, row in data.iterrows():
        ax.errorbar(row['proportions_total'], i + 1, xerr=(row['ci_upper'] - row['ci_lower']) / 2, fmt='o', markersize=row['normalized_weight'], capsize=2, elinewidth=1.25, ecolor='black', color='black')
    ax.axvline(0, color='gray', linestyle='--', lw=0.5)
    ax.set_yticks([0] + list(data.index + 1))  # Add an extra tick for the pooled results
    ax.set_yticklabels(['Pooled Prevalence'] + data['ID'].tolist())
    ax.text(-0.067, len(data) + 1.5, r'\underline{\textbf{Study}}', fontsize=10, verticalalignment='top')
    ax.yaxis.grid(True, linestyle='--', alpha=0.5, which='both') # Add grid to y-axis    
    # Add the pooled results
    pooled_p, _, lower_ci, upper_ci, _ = get_pooled_statistics(p, n)
    ax.errorbar(pooled_p, 0, xerr=(upper_ci - lower_ci) / 2, fmt='o', markersize=7, capsize=2, elinewidth=1.25, ecolor='red', color='red')
    ax.axvline(pooled_p, color='red', linestyle='--', lw=0.5)  # Add a vertical dashed line through the red circle
    # y axis 2
    ax2 = ax.twinx()
    ax2.set_yticks([0] + list(data.index + 1))
    yticklabels = [''] + (data['proportions_total'].round(2).astype(str) + ' (' + data['ci_lower'].round(2).astype(str) + '-' + data['ci_upper'].round(2).astype(str) + ')').tolist()
    yticklabels[0] = str(round(pooled_p, 2)) + ' (' + str(round(lower_ci, 2)) + '-' + str(round(upper_ci, 2)) + ')'
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


#----- Main ------------------------------------------------------------------------------------------------------------------------
# prepare data
data = pd.read_excel('results/raw_proportions.xlsx', sheet_name='proportions_total')
data['weight'] = 1 / (data['proportions_total'] * (1 - data['proportions_total']) / data['sample_size'])
data['normalized_weight'] = normalize_weights(data['weight'])
data['marker_size'] = np.sqrt(data['weight'])
p = data['proportions_total']
n = data['sample_size']

# create forest plot
plot = create_forest_plot(data)

# save plot
plot.savefig('results/forest_plot.png', dpi=300, bbox_inches='tight')


