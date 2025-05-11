# Experimental Design Example


## Setup

``` python
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt
import seaborn as sns

sns.set(style="whitegrid")
```

## Data Simulation

We simulate data for three cases:

1.  **No multicollinearity**: x1 and x2 are independent.
2.  **Mild multicollinearity**: x2 is moderately correlated with x1.
3.  **Strong multicollinearity**: x2 is highly correlated with x1.

``` python
np.random.seed(123)
n = 100
b0, b1, b2 = 5, 3, -2
noise = np.random.normal(0, 1, n)

def simulate_data(correlation: float):
    x1 = np.random.normal(0, 1, n)
    x2 = correlation * x1 + np.sqrt(1 - correlation**2) * np.random.normal(0, 1, n)
    y = b0 + b1 * x1 + b2 * x2 + noise
    return pd.DataFrame({'x1': x1, 'x2': x2, 'y': y})

df_none = simulate_data(0.0)
df_mild = simulate_data(0.5)
df_strong = simulate_data(0.95)
```

## Visualizing Correlation

``` python
fig, axs = plt.subplots(1, 3, figsize=(15, 4))
for ax, df, title in zip(axs, [df_none, df_mild, df_strong],
                         ["No Correlation", "Mild Correlation", "Strong Correlation"]):
    sns.scatterplot(data=df, x="x1", y="x2", ax=ax)
    ax.set_title(title)
plt.tight_layout()
plt.show()
```

<figure id="visualize-correlation">
<img
src="DOE_examplePY_files/figure-markdown_strict/visualize-correlation-output-1.png"
alt="Scatterplots showing increasing correlation between x1 and x2" />
<figcaption aria-hidden="true">Scatterplots showing increasing
correlation between x1 and x2</figcaption>
</figure>

## Modeling Function

``` python
def fit_models(df):
    X = df[['x1', 'x2']].values
    y = df['y'].values

    models = {
        "0.Linear": LinearRegression(),
        "1.Ridge": Ridge(alpha=1.0),
        "2.Lasso": Lasso(alpha=0.1)
    }

    results = []
    for name, model in models.items():
        model.fit(X, y)
        y_pred = model.predict(X)
        mse = mean_squared_error(y, y_pred)
        coefs = model.coef_
        biasb1 = coefs[0]-3
        biasb2 = coefs[1]-(-2)
        results.append({
            "Model": name,
            "MSE": mse,
            "bias_b1": biasb1,
            "bias_b2": biasb2
        })

    return pd.DataFrame(results)
```

## Results for Each Scenario

``` python
res_none = fit_models(df_none)
res_mild = fit_models(df_mild)
res_strong = fit_models(df_strong)

results = pd.concat([
    res_none.assign(Scenario="0.None"),
    res_mild.assign(Scenario="1.Mild"),
    res_strong.assign(Scenario="2.Strong")
])

results.pivot(index="Scenario", columns="Model", values=["bias_b1", "bias_b2", "MSE"])
```

<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead tr th {
        text-align: left;
    }

    .dataframe thead tr:last-of-type th {
        text-align: right;
    }
</style>

<table class="dataframe" data-quarto-postprocess="true" data-border="1">
<thead>
<tr class="header">
<th data-quarto-table-cell-role="th"></th>
<th colspan="3" data-quarto-table-cell-role="th"
data-halign="left">bias_b1</th>
<th colspan="3" data-quarto-table-cell-role="th"
data-halign="left">bias_b2</th>
<th colspan="3" data-quarto-table-cell-role="th"
data-halign="left">MSE</th>
</tr>
<tr class="odd">
<th data-quarto-table-cell-role="th">Model</th>
<th data-quarto-table-cell-role="th">0.Linear</th>
<th data-quarto-table-cell-role="th">1.Ridge</th>
<th data-quarto-table-cell-role="th">2.Lasso</th>
<th data-quarto-table-cell-role="th">0.Linear</th>
<th data-quarto-table-cell-role="th">1.Ridge</th>
<th data-quarto-table-cell-role="th">2.Lasso</th>
<th data-quarto-table-cell-role="th">0.Linear</th>
<th data-quarto-table-cell-role="th">1.Ridge</th>
<th data-quarto-table-cell-role="th">2.Lasso</th>
</tr>
<tr class="header">
<th data-quarto-table-cell-role="th">Scenario</th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th"></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td data-quarto-table-cell-role="th">0.None</td>
<td>-0.020783</td>
<td>-0.052642</td>
<td>-0.129654</td>
<td>-0.069663</td>
<td>-0.047120</td>
<td>0.039339</td>
<td>1.267894</td>
<td>1.269295</td>
<td>1.289681</td>
</tr>
<tr class="even">
<td data-quarto-table-cell-role="th">1.Mild</td>
<td>-0.167702</td>
<td>-0.233075</td>
<td>-0.428306</td>
<td>0.025506</td>
<td>0.071857</td>
<td>0.231060</td>
<td>1.254669</td>
<td>1.257372</td>
<td>1.301292</td>
</tr>
<tr class="odd">
<td data-quarto-table-cell-role="th">2.Strong</td>
<td>0.317939</td>
<td>-0.183747</td>
<td>-1.794442</td>
<td>-0.073454</td>
<td>0.414340</td>
<td>2.000000</td>
<td>1.212836</td>
<td>1.234699</td>
<td>1.603450</td>
</tr>
</tbody>
</table>

</div>

## How do we know which numbers are statistically different?

-   We need repetitions!

-   In the **no correlation** case, all methods yield similar
    coefficients and MSE.

-   As multicollinearity increases, **Linear Regression** coefficients
    become unstable.

-   **Ridge** regression shrinks coefficients and handles
    multicollinearity better.

-   **Lasso** may set some coefficients to zero depending on the penalty
    strength.

------------------------------------------------------------------------
