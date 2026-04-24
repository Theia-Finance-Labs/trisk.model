# data-output-description

``` r
library(trisk.model)
library(magrittr)
```

## Obtain outputs

### Load the test data

Load the internal datasets

``` r
assets_testdata <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios_testdata <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial_features_testdata <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
ngfs_carbon_price_testdata <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))
```

Define the scenarios to use

``` r
baseline_scenario <- "NGFS2023GCAM_CP"
target_scenario <- "NGFS2023GCAM_NZ2050"
scenario_geography <- "Global"
```

### Generate outputs

Run the model with the provided data

``` r
start_time <- Sys.time() # Measure execution time
st_results <- run_trisk_model(
  assets_data = assets_testdata,
  scenarios_data = scenarios_testdata,
  financial_data = financial_features_testdata,
  carbon_data = ngfs_carbon_price_testdata,
  baseline_scenario = baseline_scenario,
  target_scenario = target_scenario,
  scenario_geography = scenario_geography
)
#> -- Retyping Dataframes. 
#> -- Processing Assets and Scenarios. 
#> -- Transforming to Trisk model input. 
#> -- Calculating baseline, target, and shock trajectories. 
#> -- Applying zero-trajectory logic to production trajectories. 
#> -- Calculating net profits.
#> Joining with `by = join_by(asset_id, company_id, sector, technology)`
#> -- Calculating market risk. 
#> -- Calculating credit risk.
end_time <- Sys.time() # End time
```

``` r
time_taken <- end_time - start_time
print(paste("Done in ", time_taken))
#> [1] "Done in  0.738813877105713"
```

Get result dataframes from function output

``` r
npv_results <- st_results$npv_results
pd_results <- st_results$pd_results
company_trajectories <- st_results$company_trajectories
```

------------------------------------------------------------------------

#### NPV results

##### Data Description

The `npv_results` dataset includes the following columns:

- `run_id`: Unique identifier for the simulation run.
- `company_id`: Unique identifier for the company.
- `asset_id`: Unique identifier for the asset.
- `company_name`: Name of the company.
- `asset_name`: Name of the asset.
- `sector`: Sector in which the company operates (e.g., Oil&Gas, Coal,
  Power).
- `technology`: Type of technology used by the company (e.g., Gas,
  CoalCap, RenewablesCap).
- `net_present_value_baseline`: Net present value (NPV) under the
  baseline scenario.
- `net_present_value_shock`: Net present value (NPV) under the shock
  scenario.

##### Data Structure

``` r
str(npv_results)
#> tibble [7 × 12] (S3: tbl_df/tbl/data.frame)
#>  $ run_id                      : chr [1:7] "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" ...
#>  $ company_id                  : chr [1:7] "101" "102" "103" "104" ...
#>  $ asset_id                    : chr [1:7] "101" "102" "103" "104" ...
#>  $ company_name                : chr [1:7] "Company 1" "Company 2" "Company 3" "Company 4" ...
#>  $ asset_name                  : chr [1:7] "Company 1" "Company 2" "Company 3" "Company 4" ...
#>  $ sector                      : chr [1:7] "Oil&Gas" "Coal" "Oil&Gas" "Power" ...
#>  $ technology                  : chr [1:7] "Gas" "Coal" "Gas" "RenewablesCap" ...
#>  $ country_iso2                : chr [1:7] "DE" "DE" "DE" "DE" ...
#>  $ net_present_value_baseline  : num [1:7] 5.20e+04 1.36e+07 2.77e+07 1.42e+08 5.74e+07 ...
#>  $ net_present_value_shock     : num [1:7] 1.35e+04 4.32e+06 1.24e+07 2.03e+08 1.19e+07 ...
#>  $ net_present_value_difference: num [1:7] -38403 -9330413 -15304157 60919074 -45544705 ...
#>  $ net_present_value_change    : num [1:7] -0.739 -0.684 -0.552 0.43 -0.793 ...
```

##### Sample Data

``` r
knitr::kable(head(as.data.frame(npv_results))) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")
```

| run_id                               | company_id | asset_id | company_name | asset_name | sector  | technology    | country_iso2 | net_present_value_baseline | net_present_value_shock | net_present_value_difference | net_present_value_change |
|:-------------------------------------|:-----------|:---------|:-------------|:-----------|:--------|:--------------|:-------------|---------------------------:|------------------------:|-----------------------------:|-------------------------:|
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | 101      | Company 1    | Company 1  | Oil&Gas | Gas           | DE           |                   51951.82 |                13549.28 |                    -38402.54 |               -0.7391952 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 102        | 102      | Company 2    | Company 2  | Coal    | Coal          | DE           |                13648160.57 |              4317747.56 |                  -9330413.02 |               -0.6836389 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 103        | 103      | Company 3    | Company 3  | Oil&Gas | Gas           | DE           |                27724344.25 |             12420187.12 |                 -15304157.13 |               -0.5520115 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 104        | 104      | Company 4    | Company 4  | Power   | RenewablesCap | DE           |              141635910\.26 |           202554984\.40 |                  60919074.14 |                0.4301104 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 105        | 105      | Company 5    | Company 5  | Power   | CoalCap       | DE           |                57418851.27 |             11874146.56 |                 -45544704.71 |               -0.7932013 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 105        | 105      | Company 5    | Company 5  | Power   | OilCap        | DE           |                 6210907.85 |              1416673.16 |                  -4794234.69 |               -0.7719056 |

#### PD results

##### Data Description

The `pd_results` dataset includes the following columns:

- `run_id`: Unique identifier for the simulation run.
- `company_id`: Unique identifier for the company.
- `company_name`: Name of the company.
- `sector`: Sector in which the company operates (e.g., Oil&Gas, Coal).
- `term`: Time period for the probability of default (PD) calculation.
- `pd_baseline`: Probability of default (PD) under the baseline
  scenario.
- `pd_shock`: Probability of default (PD) under the shock scenario.

##### Data Structure

``` r
str(pd_results)
#> tibble [50 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ run_id      : chr [1:50] "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" ...
#>  $ company_id  : chr [1:50] "101" "101" "101" "101" ...
#>  $ company_name: chr [1:50] "Company 1" "Company 1" "Company 1" "Company 1" ...
#>  $ sector      : chr [1:50] "Oil&Gas" "Oil&Gas" "Oil&Gas" "Oil&Gas" ...
#>  $ term        : int [1:50] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ pd_baseline : num [1:50] 0.00 2.82e-09 1.14e-06 2.37e-05 1.50e-04 ...
#>  $ pd_shock    : num [1:50] 2.61e-09 2.14e-05 4.65e-04 2.25e-03 5.91e-03 ...
```

##### Sample Data

``` r
knitr::kable(head(as.data.frame(pd_results))) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")
```

| run_id                               | company_id | company_name | sector  | term | pd_baseline |  pd_shock |
|:-------------------------------------|:-----------|:-------------|:--------|-----:|------------:|----------:|
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    1 |   0.0000000 | 0.0000000 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    2 |   0.0000000 | 0.0000214 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    3 |   0.0000011 | 0.0004647 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    4 |   0.0000237 | 0.0022474 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    5 |   0.0001502 | 0.0059057 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101        | Company 1    | Oil&Gas |    6 |   0.0005218 | 0.0113956 |

#### Company trajectories results

##### Data Description

The `company_trajectories` dataset includes the following columns:

- `run_id`: Unique identifier for the simulation run.
- `asset_id`: Unique identifier for the asset.
- `asset_name`: Name of the asset.
- `company_id`: Unique identifier for the company.
- `company_name`: Name of the company.
- `year`: Year of the scenario data.
- `sector`: Sector in which the company operates (e.g., Oil&Gas, Coal).
- `technology`: Type of technology used by the company.
- `production_plan_company_technology`: Production plan for the
  company’s technology.
- `production_baseline_scenario`: Production output under the baseline
  scenario.
- `production_target_scenario`: Production output under the target
  scenario.
- `production_shock_scenario`: Production output under the shock
  scenario.
- `pd`: Probability of default for the company.
- `net_profit_margin`: Net profit margin for the company.
- `debt_equity_ratio`: Debt to equity ratio for the company.
- `volatility`: Volatility of the company’s asset values.
- `scenario_price_baseline`: Price under the baseline scenario.
- `price_shock_scenario`: Price under the shock scenario.
- `net_profits_baseline_scenario`: Net profits under the baseline
  scenario.
- `net_profits_shock_scenario`: Net profits under the shock scenario.
- `discounted_net_profits_baseline_scenario`: Discounted net profits
  under the baseline scenario.
- `discounted_net_profits_shock_scenario`: Discounted net profits under
  the shock scenario.

##### Data Structure

``` r
str(company_trajectories)
#> tibble [210 × 23] (S3: tbl_df/tbl/data.frame)
#>  $ run_id                                  : chr [1:210] "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" "9e28dd87-48dc-4e2b-8255-11609a628296" ...
#>  $ asset_id                                : chr [1:210] "101" "101" "101" "101" ...
#>  $ asset_name                              : chr [1:210] "Company 1" "Company 1" "Company 1" "Company 1" ...
#>  $ company_id                              : chr [1:210] "101" "101" "101" "101" ...
#>  $ company_name                            : chr [1:210] "Company 1" "Company 1" "Company 1" "Company 1" ...
#>  $ country_iso2                            : chr [1:210] "DE" "DE" "DE" "DE" ...
#>  $ sector                                  : chr [1:210] "Oil&Gas" "Oil&Gas" "Oil&Gas" "Oil&Gas" ...
#>  $ technology                              : chr [1:210] "Gas" "Gas" "Gas" "Gas" ...
#>  $ year                                    : num [1:210] 2022 2023 2024 2025 2026 ...
#>  $ production_plan_company_technology      : num [1:210] 5000 5423 6200 7400 7800 ...
#>  $ production_baseline_scenario            : num [1:210] 5000 5423 6200 7400 7800 ...
#>  $ production_target_scenario              : num [1:210] 5000 5001 5003 5004 4863 ...
#>  $ production_shock_scenario               : num [1:210] 5000 5423 6200 7400 7800 ...
#>  $ pd                                      : num [1:210] 0.00562 0.00562 0.00562 0.00562 0.00562 ...
#>  $ net_profit_margin                       : num [1:210] 0.0764 0.0764 0.0764 0.0764 0.0764 ...
#>  $ debt_equity_ratio                       : num [1:210] 0.13 0.13 0.13 0.13 0.13 ...
#>  $ volatility                              : num [1:210] 0.259 0.259 0.259 0.259 0.259 ...
#>  $ scenario_price_baseline                 : num [1:210] 5.87 5.9 5.93 5.96 5.95 ...
#>  $ price_shock_scenario                    : num [1:210] 5.87 5.9 5.93 5.96 5.95 ...
#>  $ net_profits_baseline_scenario           : num [1:210] 2240 2442 2807 3368 3541 ...
#>  $ net_profits_shock_scenario              : num [1:210] 2240 2442 2807 3368 3541 ...
#>  $ discounted_net_profits_baseline_scenario: num [1:210] 2240 2283 2452 2750 2701 ...
#>  $ discounted_net_profits_shock_scenario   : num [1:210] 2240 2283 2452 2750 2701 ...
```

##### Sample Data

``` r
knitr::kable(head(as.data.frame(company_trajectories))) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")
```

| run_id                               | asset_id | asset_name | company_id | company_name | country_iso2 | sector  | technology | year | production_plan_company_technology | production_baseline_scenario | production_target_scenario | production_shock_scenario |        pd | net_profit_margin | debt_equity_ratio | volatility | scenario_price_baseline | price_shock_scenario | net_profits_baseline_scenario | net_profits_shock_scenario | discounted_net_profits_baseline_scenario | discounted_net_profits_shock_scenario |
|:-------------------------------------|:---------|:-----------|:-----------|:-------------|:-------------|:--------|:-----------|-----:|-----------------------------------:|-----------------------------:|---------------------------:|--------------------------:|----------:|------------------:|------------------:|-----------:|------------------------:|---------------------:|------------------------------:|---------------------------:|-----------------------------------------:|--------------------------------------:|
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2022 |                               5000 |                         5000 |                   5000.000 |                      5000 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.867116 |             5.867116 |                      2239.895 |                   2239.895 |                                 2239.895 |                              2239.895 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2023 |                               5423 |                         5423 |                   5001.354 |                      5423 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.898569 |             5.898569 |                      2442.414 |                   2442.414 |                                 2282.630 |                              2282.630 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2024 |                               6200 |                         6200 |                   5002.708 |                      6200 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.930022 |             5.930022 |                      2807.250 |                   2807.250 |                                 2451.961 |                              2451.961 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2025 |                               7400 |                         7400 |                   5004.062 |                      7400 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.961475 |             5.961475 |                      3368.360 |                   3368.360 |                                 2749.585 |                              2749.585 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2026 |                               7800 |                         7800 |                   4862.620 |                      7800 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.945170 |             5.945170 |                      3540.723 |                   3540.723 |                                 2701.201 |                              2701.201 |
| 9e28dd87-48dc-4e2b-8255-11609a628296 | 101      | Company 1  | 101        | Company 1    | DE           | Oil&Gas | Gas        | 2027 |                               8600 |                         8600 |                   4721.178 |                      8600 | 0.0056224 |         0.0763542 |         0.1297317 |   0.259323 |                5.928866 |             5.928866 |                      3893.168 |                   3893.168 |                                 2775.775 |                              2775.775 |
