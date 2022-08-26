# FOAG country profiles

General context. Recompilation of 30 indicators per country (Honduras, Bangladesh, and Ethiopia). Country data. Different data periods.

## Profile result
| Group                         | Initial | Recent | Mean_delta | Cleaned_mean_delta | Indicator                   | Order      | Period    | Last_year |
| :---                          | :----:  | :----: | :----:     | :----:             | :----:                      | :----:     | :----:    | ---:      |
| Countries with similar GDP pc | 55.8    | 65.4   | 13         | 0.2                | Nutrient nitrogen N (total) | Indicator1 | 2010-2019 | 2019      |
| Geographic neighbors          | 55.9    | 75.7   | 6.8        | 1                  | Nutrient nitrogen N (total) | Indicator1 | 2010-2019 | 2019      |
| Honduras                      | 37.4    | 54.0   | 7          | 9.1                | Nutrient nitrogen N (total) | Indicator1 | 2010-2019 | 2019      |
| Global average                | 56.9    | 61.3   | 72838      | 4.1                | Nutrient nitrogen N (total) | Indicator1 | 2010-2019 | 2019      |

## Data management

### Rates of change (RoC) computation
- Computing consecutive relative change for the corresponding time series.
- When missing data are present. The corresponding RoC between two non-consecutive years is divided by the correspoding number of missing years under the assumption of constant change over the period. The resulting values replace the missing values.
- A RoC 10-years average is the final value reported.
- Additionally, a RoC 10-years average cleaned is computed. A two-steps outliers detection method was developed to identify and remove outliers within the RoC time series per country. The methods are the following:
  * Outlier detection using boxplot
  * Minimum Covariance Determinant (MCD) [OutliersO3 R package](https://cran.rstudio.com/web/packages/OutliersO3/index.html)

### Missing data management
- When the target year (Initial or Recent) has missing values, a window of +- 2 years interval was used to report the target year value using the closest one.
