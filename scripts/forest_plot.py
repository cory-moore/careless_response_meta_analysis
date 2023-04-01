
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from scripts.proportions_pooled import get_pooled_statistics


#----- Functions ------------------------------------------------------------------------------------------------------------------

def create_forest_plot(df):
    # Create a figure with a width of 12 and a height of 0.5 times the number of studies
    fig, ax = plt.subplots(figsize=(16, len(df) * 0.2))

    ax.errorbar(df['proportions_total'], df.index, xerr=(df['ci_upper'] - df['ci_lower']) / 2, fmt='o', markersize=6, capsize=3, elinewidth=1.5, ecolor='black', color='black')
    ax.axvline(0, color='gray', linestyle='--', lw=0.5)
    ax.set_yticks(df.index)
    ax.set_yticklabels(df['ID'])

    # Add proportions and CIs on the right axis of the plot and make sure they are aligned with the ytick labels
    ax2 = ax.twinx()
    ax2.set_yticks(df.index)
    ax2.set_yticklabels(df['proportions_total'].round(2).astype(str) + ' (' + df['ci_lower'].round(2).astype(str) + '-' + df['ci_upper'].round(2).astype(str) + ')')
    ax2.yaxis.tick_right()
    ax2.set_ylim(ax.get_ylim())
    ax2.set_yticks(ax.get_yticks())


    # label the y axis with 'Studies
    # place the 'Studies' ylabel directly above the ytick labels 
    ax.set_ylabel('Studies', labelpad=20)

    # label the x axis with 'Prevalence'
    ax.set_xlabel('Prevalence')
    # label the right axis with 'Proportions (CI)'
    ax2.set_ylabel('Proportions (CI)')
    # set the title of the plot
    ax.set_title("Forest Plot")

    # tight layout but with some cushion around the plot
    plt.tight_layout(pad=2)
    plt.show()

    return fig


#----- Main ------------------------------------------------------------------------------------------------------------------------
data = pd.read_excel('results/raw_proportions.xlsx', sheet_name='proportions_total')
data['weight'] = 1 / (data['proportions_total'] * (1 - data['proportions_total']) / data['sample_size'])
data['marker_size'] = np.sqrt(data['weight'])
p = data['proportions_total']
n = data['sample_size']

# calculate the pooled statistics for the total careless response proportions
pooled_p, pooled_se, lower_ci, upper_ci, n_sum = get_pooled_statistics(p, n)


create_forest_plot(data)