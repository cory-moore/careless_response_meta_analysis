
from graphviz import Digraph

# Define the data
data = {
    'Studies identified from databases': ['Screening'],
    'Studies identified from other sources': ['Screening'],
    'Screening': ['Studies deemed eligible and assigned for coding'],
    'Studies deemed eligible and assigned for coding': ['Preliminary studies included in this presentation', 'Studies included in meta-analysis'],
    'Studies included in meta-analysis': [],
    'Preliminary studies included in this presentation': [],
}

# Create a new Digraph object
dot = Digraph()

# Set the default attributes for the nodes and edges
dot.attr('node', shape='rectangle', width='2.5', height='1.25', fontsize='16', style='rounded')
dot.attr('edge', fontsize='16')

# Add the nodes and edges to the graph
for node, children in data.items():
    dot.node(node)
    for child in children:
        dot.edge(node, child)

# Set the attributes for the end nodes
dot.node('Studies included in meta-analysis', shape='ellipse', width='1.5', height='1', style='filled', fillcolor='#59a14f', fontcolor='white')
dot.node('Preliminary studies included in this presentation', shape='ellipse', width='1.5', height='1', style='filled', fillcolor='#e15759', fontcolor='white')

# Add the labels for the nodes
dot.node('Studies identified from databases', label='Studies identified from databases\nPsycINFO (n = ),\nProQuest Dissertations & Theses Global (n = 85)', fontsize='14')
dot.node('Studies identified from other sources', label='Studies identified from other sources\nCitation Searches (n = )', fontsize='14')
dot.node('Screening', label='Screening\nIdentified studies were screened based on the following criteria:\n- Appeared in a journal article or dissertation\n- Published in an IO journal from Highhouse, Zickar, & Melick (2020)\n- Must report amount of careless responding detected in sample\n- Must report techniques used to detect careless responding\n- Must NOT have manipulated careless responding behavior', fontsize='14')
dot.node('Studies deemed eligible and assigned for coding', label='Studies deemed eligible and assigned for coding\n(n = )', fontsize='14')

# Render the graph
dot.render('results/prisma_flowchart', format='png', view=True)
