# FOAG country profiles

General context. Recompilation of 30 indicators per country. Country data. Different data periods.

## Data management

### Rates of change (RoC) computation
- Computing consecutive relative change for the corresponding time series.
- When missing data are present. The corresponding RoC between two non-consecutive years is divided by the correspoding number of missing years under the assumption of constant change over the period. The resulting values replace the missing values.
- A RoC 10-years average is the final value reported.
- Additionally, a RoC 10-years average cleaned is computed. A two-steps outliers detection method was developed to identify and remove outliers within the RoC time series per country. The methods are the following:
-- Outlier detection using boxplot
-- Minimum Covariance Determinant (MCD) [OutliersO3 R package](https://cran.rstudio.com/web/packages/OutliersO3/index.html)

### Missing data management
- 
