# Monte Carlo Simulation: Comparing Linear, Ridge, and Lasso Regression


## Objective

This Monte Carlo simulation compares the performance of three regression
models: - Linear Regression - Ridge Regression - Lasso Regression

We will simulate 30 datasets in total: - 10 datasets with no correlation
between predictors *x*<sub>1</sub> and *x*<sub>2</sub> - 10 datasets
with mild correlation - 10 datasets with high correlation

The models will be evaluated on Mean Squared Error (MSE). The results
will be summarized using the mean and standard deviation of the MSE for
each combination of model and correlation structure.

## Setup

``` python
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split
import seaborn as sns
import matplotlib.pyplot as plt

np.random.seed(116)
```

## Data Generation Function

``` python
def generate_data(n=100, correlation=0.0):
    mean = [0, 0]
    cov = [[1, correlation], [correlation, 1]]
    X = np.random.multivariate_normal(mean, cov, size=n)
    x1, x2 = X[:, 0], X[:, 1]
    noise = np.random.normal(0, 1, size=n)
    y = 0 * x1 + 4 * x2 + noise
    return pd.DataFrame({'x1': x1, 'x2': x2, 'y': y})
```

## Simulation Loop

``` python
results = []
correlations = [("No Corr", 0.0), ("Mild Corr", 0.5), ("High Corr", 0.99)]
models = {
    "Linear": LinearRegression(),
    "Ridge": Ridge(alpha=0.1),
    "Lasso": Lasso(alpha=0.1)
}

for label, rho in correlations:
    for i in range(10):
        df = generate_data(correlation=rho)
        X = df[['x1', 'x2']]
        y = df['y']
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=i)

        for model_name, model in models.items():
            model.fit(X_train, y_train)
            y_pred = model.predict(X_test)
            mse = mean_squared_error(y_test, y_pred)
            results.append({
                'Model': model_name,
                'Correlation': label,
                'MSE': mse
            })
```

## Results Summary

``` python
results_df = pd.DataFrame(results)
summary = results_df.groupby(['Model', 'Correlation']).agg(
    Mean_MSE=('MSE', 'mean'),
    SD_MSE=('MSE', 'std')
).reset_index()

summary
```

<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>

<table class="dataframe" data-quarto-postprocess="true" data-border="1">
<thead>
<tr class="header" style="text-align: right;">
<th data-quarto-table-cell-role="th"></th>
<th data-quarto-table-cell-role="th">Model</th>
<th data-quarto-table-cell-role="th">Correlation</th>
<th data-quarto-table-cell-role="th">Mean_MSE</th>
<th data-quarto-table-cell-role="th">SD_MSE</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td data-quarto-table-cell-role="th">0</td>
<td>Lasso</td>
<td>High Corr</td>
<td>1.147515</td>
<td>0.368028</td>
</tr>
<tr class="even">
<td data-quarto-table-cell-role="th">1</td>
<td>Lasso</td>
<td>Mild Corr</td>
<td>1.081154</td>
<td>0.208798</td>
</tr>
<tr class="odd">
<td data-quarto-table-cell-role="th">2</td>
<td>Lasso</td>
<td>No Corr</td>
<td>1.042957</td>
<td>0.179906</td>
</tr>
<tr class="even">
<td data-quarto-table-cell-role="th">3</td>
<td>Linear</td>
<td>High Corr</td>
<td>1.143185</td>
<td>0.370820</td>
</tr>
<tr class="odd">
<td data-quarto-table-cell-role="th">4</td>
<td>Linear</td>
<td>Mild Corr</td>
<td>1.093659</td>
<td>0.233400</td>
</tr>
<tr class="even">
<td data-quarto-table-cell-role="th">5</td>
<td>Linear</td>
<td>No Corr</td>
<td>1.053493</td>
<td>0.195402</td>
</tr>
<tr class="odd">
<td data-quarto-table-cell-role="th">6</td>
<td>Ridge</td>
<td>High Corr</td>
<td>1.134172</td>
<td>0.369476</td>
</tr>
<tr class="even">
<td data-quarto-table-cell-role="th">7</td>
<td>Ridge</td>
<td>Mild Corr</td>
<td>1.092485</td>
<td>0.231788</td>
</tr>
<tr class="odd">
<td data-quarto-table-cell-role="th">8</td>
<td>Ridge</td>
<td>No Corr</td>
<td>1.051953</td>
<td>0.194410</td>
</tr>
</tbody>
</table>

</div>

## Visualization (Optional)

``` python
sns.boxplot(data=results_df, x='Correlation', y='MSE', hue='Model')
plt.title("Model MSE by Predictor Correlation")
plt.show()
```

![](MCsimPY_files/figure-markdown_strict/cell-6-output-1.png)
