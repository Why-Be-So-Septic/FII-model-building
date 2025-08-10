import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from scipy.stats import mannwhitneyu
from plotly.subplots import make_subplots
import plotly.graph_objects as go
import plotly.express as px
from scipy.stats import chi2_contingency, ttest_ind, spearmanr
import warnings
warnings.filterwarnings("ignore")


df = pd.read_csv('data_wide (1).csv')

# 1. Save dataset description
description = df.describe(include='all').transpose()
description.to_csv('dataset_description.csv')


# 2. Count unique IDs

unique_ids = df['id'].nunique()
with open('unique_ids_count.txt', 'w') as f:
    f.write(f"Unique IDs: {unique_ids}\n")

# 3. Histograms for Aging and DaysBfOutcome
plt.figure(figsize=(10, 5))
plt.subplot(1, 2, 1)
sns.histplot(df['Aging'].dropna(), kde=True)
plt.title('Distribution of Aging')

plt.subplot(1, 2, 2)
sns.histplot(df['DaysBfOutcome'].dropna(), kde=True)
plt.title('Distribution of DaysBfOutcome')

plt.tight_layout()
plt.savefig('aging_daysbfoutcome_histograms.png')
plt.close()

# 4. Median distributions per ID for all numeric variables

numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()

for col in ['id', 'Outcome']:
    if col in numeric_cols:
        numeric_cols.remove(col)

median_per_id = df.groupby('id')[numeric_cols].median().reset_index()

cols_per_row = 3
rows = (len(numeric_cols) + cols_per_row - 1) // cols_per_row

plot_size = 300
fig_width = cols_per_row * plot_size
fig_height = rows * plot_size

# Subplots
fig_medians = make_subplots(
    rows=rows,
    cols=cols_per_row,
    subplot_titles=[f"Median {col}" for col in numeric_cols],
    horizontal_spacing=0.05,
    vertical_spacing=0.08
)

for i, col in enumerate(numeric_cols):
    row = (i // cols_per_row) + 1
    col_pos = (i % cols_per_row) + 1

    fig_medians.add_trace(
        go.Histogram(
            x=median_per_id[col],
            name=f"Median {col}",
            marker=dict(opacity=0.7),
            showlegend=True
        ),
        row=row, col=col_pos
    )

    fig_medians.update_xaxes(title_text="Value", row=row, col=col_pos)
    fig_medians.update_yaxes(title_text="Frequency", row=row, col=col_pos)

fig_medians.update_layout(
    width=fig_width,
    height=fig_height,
    title_text="Median Distributions per ID",
    showlegend=True,
    legend=dict(
        orientation="h",
        yanchor="bottom",
        y=-0.1,
        xanchor="center",
        x=0.5
    ),
    template="plotly_white",
    margin=dict(l=60, r=30, t=80, b=100),
    hovermode="closest"
)

fig_medians.write_html("median_distributions_per_id.html")


# 5. Boxplot & Violinplot: Aging vs Outcome with t-test
df_first = df.drop_duplicates(subset='id', keep='first')

t_stat, p_val = ttest_ind(
    df_first[df_first['Outcome'] == 0]['Aging'],
    df_first[df_first['Outcome'] == 1]['Aging'],
    nan_policy='omit'
)

plt.figure(figsize=(10, 6))
sns.boxplot(data=df_first, x='Outcome', y='Aging', showmeans=True, meanline=True)
sns.violinplot(data=df_first, x='Outcome', y='Aging', alpha=0.5, color='lightgray')

plt.title(f'Aging vs Outcome (p={p_val:.4f})\nT-test result: t={t_stat:.3f}')
plt.xlabel('Outcome (0=Alive, 1=Dead)')
plt.ylabel('Aging')
plt.savefig('aging_vs_outcome_box_violin.png')
plt.close()

# 6. Aging vs DaysBfOutcome: Spearman + CI

rho, p_spearman = spearmanr(df['Aging'], df['DaysBfOutcome'])

# confidence interval for Spearman
n_boot = 1000
boot_rhos = []
for _ in range(n_boot):
    sample = df.sample(frac=1, replace=True)
    r, _ = spearmanr(sample['Aging'], sample['DaysBfOutcome'])
    if not np.isnan(r):
        boot_rhos.append(r)
boot_rhos = np.array(boot_rhos)
ci_lower, ci_upper = np.percentile(boot_rhos, 2.5), np.percentile(boot_rhos, 97.5)

plt.figure(figsize=(8, 6))
sns.scatterplot(data=df, x='Aging', y='DaysBfOutcome', alpha=0.6)
plt.title(f'Spearman: ρ = {rho:.3f} (95% CI: [{ci_lower:.3f}, {ci_upper:.3f}]), p = {p_spearman:.4f}')
plt.xlabel('Aging')
plt.ylabel('DaysBfOutcome')
plt.grid(True, alpha=0.3)
plt.savefig('aging_vs_daysbfoutcome_spearman.png')
plt.close()


# 7. DaysBfOutcome vs Outcome
#    Mann-Whitney U (non-parametric)
u_stat, p_mw = stats.mannwhitneyu(
    df_first[df_first['Outcome'] == 0]['DaysBfOutcome'],
    df_first[df_first['Outcome'] == 1]['DaysBfOutcome'],
    alternative='two-sided'
)

plt.figure(figsize=(8, 6))
sns.boxplot(data=df_first, x='Outcome', y='DaysBfOutcome')
plt.title(f'DaysBfOutcome vs Outcome\nMann-Whitney U test: p = {p_mw:.4f}\n'
          f'U = {u_stat:.0f}\n'
          'Optimal criterion: Non-parametric comparison (median difference)')
plt.xlabel('Outcome (0=Alive, 1=Dead)')
plt.ylabel('DaysBfOutcome')
plt.savefig('daysbfoutcome_vs_outcome.png')
plt.close()


# 8. Outcome vs Gender: Chi-square test + stacked bar

ct = pd.crosstab(df_first['Gender'], df_first['Outcome'])
chi2, p_chi2, _, _ = chi2_contingency(ct)

plt.figure(figsize=(8, 6))
ct_norm = pd.crosstab(df_first['Gender'], df_first['Outcome'], normalize='index') * 100
ct_norm.plot(kind='bar', stacked=True)
plt.title(f'Outcome by Gender\nChi-square test: p = {p_chi2:.4f}')
plt.xlabel('Gender')
plt.ylabel('Percentage (%)')
plt.legend(title='Outcome', labels=['Alive (0)', 'Dead (1)'])
plt.xticks(rotation=0)
plt.grid(True, axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('outcome_vs_gender_chisquare.png')
plt.close()


# 9. DaysBfOutcome vs Gender
#    t-test or Mann-Whitney depending on normality

male_dbo = df_first[df_first['Gender'] == 'male']['DaysBfOutcome'].dropna()
female_dbo = df_first[df_first['Gender'] == 'female']['DaysBfOutcome'].dropna()


_, p_male = stats.shapiro(male_dbo) if len(male_dbo) > 3 else (np.nan, 1)
_, p_female = stats.shapiro(female_dbo) if len(female_dbo) > 3 else (np.nan, 1)

use_mw = True  # default to non-parametric

if use_mw:
    u_g, p_g = mannwhitneyu(male_dbo, female_dbo, alternative='two-sided')
    test_name = "Mann-Whitney U"
    p_val_gender = p_g
    stat_line = f"U = {u_g:.0f}"
else:
    t_g, p_g = ttest_ind(male_dbo, female_dbo, nan_policy='omit')
    test_name = "T-test"
    p_val_gender = p_g
    stat_line = f"t = {t_g:.3f}"

plt.figure(figsize=(8, 6))
sns.boxplot(data=df_first, x='Gender', y='DaysBfOutcome')
plt.title(f'DaysBfOutcome vs Gender\n{test_name}: p = {p_val_gender:.4f}\n{stat_line}\n'
          'Optimal criterion: Non-parametric (Mann-Whitney U)')
plt.xlabel('Gender')
plt.ylabel('DaysBfOutcome')
plt.grid(True, alpha=0.3)
plt.savefig('daysbfoutcome_vs_gender.png')
plt.close()


# group the subplots into panel

import base64
from plotly.subplots import make_subplots
import plotly.graph_objects as go
from plotly.offline import plot

def image_to_data_uri(filepath):
    try:
        with open(filepath, "rb") as img_file:
            # Detect file type from extension
            ext = filepath.split('.')[-1].lower()
            if ext in ['png']:
                mime = 'image/png'
            elif ext in ['jpg', 'jpeg']:
                mime = 'image/jpeg'
            else:
                mime = 'image/png'
            data = img_file.read()
            return f"data:{mime};base64,{base64.b64encode(data).decode()}"
    except Exception as e:
        print(f"Error reading image {filepath}: {e}")
        return None

# List of image files
image_files = [
    'aging_daysbfoutcome_histograms.png',
    'aging_daysbfoutcome_histograms.png',  # duplicate if needed
    'aging_vs_outcome_box_violin.png',
    'aging_vs_daysbfoutcome_spearman.png',
    'daysbfoutcome_vs_outcome.png',
    'outcome_vs_gender_chisquare.png',
    'daysbfoutcome_vs_gender.png'
]

# Titles
titles = [
    'Aging Histogram', 'DaysBfOutcome Histogram',
    'Aging vs Outcome', 'Aging vs DaysBfOutcome',
    'DaysBfOutcome vs Outcome', 'Outcome vs Gender',
    'DaysBfOutcome vs Gender'
]

# Create subplot layout
fig_panel = make_subplots(
    rows=3, cols=3,
    subplot_titles=titles + [''],
    specs=[
        [{}, {}, {}],
        [{}, {}, {}],
        [{}, {}, None]
    ]
)

# Define positions (row, col, x, y, sizex, sizey)
positions = [
    (1, 1, 0.02, 0.72, 6, 4),
    (1, 2, 0.37, 0.72, 6, 4),
    (1, 3, 0.72, 0.72, 6, 4),
    (2, 1, 0.02, 0.42, 6, 4),
    (2, 2, 0.37, 0.42, 6, 4),
    (2, 3, 0.72, 0.42, 6, 4),
    (3, 1, 0.02, 0.10, 6, 4),
]

for i, img_file in enumerate(image_files):
    uri = image_to_data_uri(img_file)
    if uri:
        row, col, x, y, sx, sy = positions[i]
        fig_panel.add_layout_image(
            dict(
                source=uri,
                x=x, y=y,
                xref="paper", yref="paper",
                sizex=sx, sizey=sy,
                xanchor="left", yanchor="bottom"
            ),
            row=row, col=col
        )

fig_panel.update_layout(
    height=900,
    title_text="All Analysis Results Panel",
    showlegend=False
)

# Save to HTML
plot(fig_panel, filename='all_results_panel.html', auto_open=True)