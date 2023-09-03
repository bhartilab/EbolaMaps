import pandas as pd
import numpy as np
import os
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from scipy.stats import pearsonr

##############################################################
# Perform a sensitivity analysis to examine the correlation  #
# between network length and outbreak metrics for different  #
# study area sizes.                                          #
##############################################################

# Set working directory
os.chdir('C:/Users/bzn5190/OneDrive - The Pennsylvania State University/Ebola/sensitivity/')

# Read the file that contains outbreak metrics and network length surrounding spillovers
df = pd.read_csv('sensitivity_analysis.csv')

# Initialize a dataframe to store correlations between network length and outbreak metrics
sens = pd.DataFrame(columns=['Diameter', 'Network', 'Correlation', 'Significance', 'Measure'])

# Compute the correlations and significance for each combination of network length and outbreak metrics across various distances
for r in df['Diameter'].unique():
    for m in ['Cases 100 days', 'Cases total', 'Days total']:
        for n in ['road_length', 'river_length', 'total_network']:
            mask = (df['Diameter'] == r)
            cor, sig = pearsonr(np.array(df.loc[mask, m]), np.array(df.loc[mask, n]))
            sens = pd.concat([sens, pd.DataFrame({'Diameter': [r], 'Network': [n], 'Correlation': [cor], 'Significance': [sig], 'Measure': [m]})])

# Create a new column to distinguish significant relationships
sens['Correlation2'] = sens['Correlation']
mask = sens['Significance'] > 0.05
sens.loc[mask, 'Correlation2'] = np.nan

# Visualize the correlations across various study area size
fig = make_subplots(rows=3, cols=1, subplot_titles=["a) Total cases in first 100 days", "b) Total cases", "c) Total duration of outbreak"])

i = 1
for m in sens['Measure'].unique():
    if i == 1:
        legend = True
    else:
        legend = False

    # Plot correlation lines for each network type
    for network_type, color, dash in zip(['road_length', 'river_length', 'total_network'], ["#2a19a8", "#a105b3", "#f00202"], ['dash', 'dash', 'dash']):
        fig.add_trace(
            go.Scatter(
                x=sens.loc[(sens['Network'] == network_type) & (sens['Measure'] == m), 'Diameter'],
                y=sens.loc[(sens['Network'] == network_type) & (sens['Measure'] == m), 'Correlation'],
                opacity=0.5,
                line=dict(color=color, dash=dash, width=1),
                name=f"Correlation: {network_type}",
                showlegend=False
            ),
            row=i,
            col=1
        )
        fig.add_trace(
            go.Scatter(
                x=sens.loc[(sens['Network'] == network_type) & (sens['Measure'] == m), 'Diameter'],
                y=sens.loc[(sens['Network'] == network_type) & (sens['Measure'] == m), 'Correlation2'],
                line=dict(color=color, width=1.5),
                name=f"{network_type} (P-values < 0.05)",
                showlegend=legend
            ),
            row=i,
            col=1
        )
    i += 1

for j in range(1, 4):
    fig.add_vline(x=150, line_width=0.5, line_color="black", row=j, col=1)

fig.update_layout(
    font=dict(size=8),
    font_family="Courier New",
    margin=dict(l=0, r=0, b=10, t=20),
    template='simple_white',
    showlegend=True,
    width=350,
    height=400
)
fig.update_annotations(font_size=8)
fig.update_layout(
    legend=dict(
        yanchor="bottom",
        y=-0.4,
        xanchor="center",
        x=0.5
    )
)
fig.update_yaxes(title="Correlation", showgrid=True, linewidth=0.1, range=[-0.5, 1.1], dtick=0.3, ticklen=1)
fig.update_xaxes(tickfont=dict(size=7), showgrid=True, linewidth=0.1, ticklen=2, row=1, col=1)
fig.update_xaxes(tickfont=dict(size=7), showgrid=True, linewidth=0.1, ticklen=2, row=2, col=1)
fig.update_xaxes(title="Study area (km)", tickfont=dict(size=7), showgrid=True, linewidth=0.1, ticklen=2, row=3, col=1)

fig.show()

# Save the figure as PDF
fig.write_image('Sensitivity_analysis.pdf', engine='orca', height=400, width=330, scale=1)
