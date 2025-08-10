import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

# Data load
df = pd.read_csv('data_wide (1).csv')

#Select the data
df = df[df['DaysBfOutcome'] <= 20]


numeric_cols = df.select_dtypes(include='number').columns.tolist()
for col in ['id', 'Aging','Outcome', 'DaysBfOutcome']:
    if col in numeric_cols:
        numeric_cols.remove(col)

# Medians
median_by_days_outcome = df.groupby(['DaysBfOutcome', 'Outcome'])[numeric_cols].median().reset_index()


df['Outcome'] = df['Outcome'].astype(int)


median_by_days_outcome = df.groupby(['DaysBfOutcome', 'Outcome'])[numeric_cols].median().reset_index()

# Plots creation
cols_per_row = 2
rows = (len(numeric_cols) + cols_per_row - 1) // cols_per_row

fig = make_subplots(
    rows=rows,
    cols=cols_per_row,
    subplot_titles=[col for col in numeric_cols],
    shared_xaxes=False,
    shared_yaxes=False,
    horizontal_spacing=0.08,
    vertical_spacing=0.05
)


colors = {0: 'blue', 1: 'red'}
labels = {0: 'Alive (0)', 1: 'Dead (1)'}


for i, col in enumerate(numeric_cols):
    row = (i // cols_per_row) + 1
    col_pos = (i % cols_per_row) + 1

    for outcome in sorted(median_by_days_outcome['Outcome'].unique()):
        data = median_by_days_outcome[median_by_days_outcome['Outcome'] == outcome]
        fig.add_trace(
            go.Scatter(
                x=data['DaysBfOutcome'],
                y=data[col],
                mode='lines+markers',
                name=labels[outcome],
                legendgroup=labels[outcome],
                showlegend=(i == 0),  # Легенда только один раз
                line=dict(color=colors[outcome]),
                marker=dict(size=6),
            ),
            row=row, col=col_pos
        )
    fig.update_xaxes(title_text="Days Before Outcome", row=row, col=col_pos)
    fig.update_yaxes(title_text="Median Value", row=row, col=col_pos)


fig.update_layout(
    title="Median Trends by Days Before Outcome (Grouped by Outcome)",
    title_x=0.5,
    height=300 * rows,
    width=1200,
    legend_title="Outcome",
    hovermode="x unified",
    template="plotly_white")

fig.write_html("median_trends_by_days_and_outcome_separate1.html")
