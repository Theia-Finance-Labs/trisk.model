# data-input-description

``` r
library(trisk.model)
library(magrittr)
```

Load the internal datasets

``` r
assets_testdata <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios_testdata <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial_features_testdata <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
ngfs_carbon_price_testdata <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))
```

------------------------------------------------------------------------

## Datasets

### Assets Test Data

This dataset contains data about company assets, including production,
technology, and geographical details.

#### Data Description

The `assets_testdata` dataset includes the following columns:

- `company_id`: Unique identifier for the company.
- `company_name`: Name of the company.
- `plan_sec_prod`: Secondary production plan.
- `country_name`: Country where the asset is located.
- `plant_age_years`: Age of the plant in years.
- `workforce_size`: Size of the workforce.
- `asset_id`: Unique identifier for the asset.
- `country_iso2`: ISO 3166-1 alpha-2 code for the country.
- `asset_name`: Name of the asset.
- `production_year`: Year of production data.
- `emission_factor`: Emissions from production.
- `technology`: Type of technology used.
- `sector`: Production sector.
- `capacity`: Asset capacity.
- `capacity_factor`: Asset utilization percentage.
- `production_unit`: Unit for production.

#### Data Structure

``` r
str(assets_testdata)
#> 'data.frame':    42 obs. of  12 variables:
#>  $ company_id     : int  101 101 101 101 101 101 102 102 102 102 ...
#>  $ company_name   : chr  "Company 1" "Company 1" "Company 1" "Company 1" ...
#>  $ asset_id       : int  101 101 101 101 101 101 102 102 102 102 ...
#>  $ country_iso2   : chr  "DE" "DE" "DE" "DE" ...
#>  $ asset_name     : chr  "Company 1" "Company 1" "Company 1" "Company 1" ...
#>  $ production_year: int  2022 2023 2024 2025 2026 2027 2022 2023 2024 2025 ...
#>  $ emission_factor: num  0.062 0.062 0.062 0.062 0.062 ...
#>  $ technology     : chr  "Gas" "Gas" "Gas" "Gas" ...
#>  $ sector         : chr  "Oil&Gas" "Oil&Gas" "Oil&Gas" "Oil&Gas" ...
#>  $ capacity       : num  8600 8600 8600 8600 8600 ...
#>  $ capacity_factor: num  0.581 0.631 0.721 0.86 0.907 ...
#>  $ production_unit: chr  "GJ" "GJ" "GJ" "GJ" ...
```

#### Sample Data

``` r
knitr::kable(head(assets_testdata)) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px") %>%
  kableExtra::column_spec(1:ncol(assets_testdata), width = "150px")
```

| company_id | company_name | asset_id | country_iso2 | asset_name | production_year | emission_factor | technology | sector  | capacity | capacity_factor | production_unit |
|-----------:|:-------------|---------:|:-------------|:-----------|----------------:|----------------:|:-----------|:--------|---------:|----------------:|:----------------|
|        101 | Company 1    |      101 | DE           | Company 1  |            2022 |       0.0620259 | Gas        | Oil&Gas |     8600 |       0.5813953 | GJ              |
|        101 | Company 1    |      101 | DE           | Company 1  |            2023 |       0.0620259 | Gas        | Oil&Gas |     8600 |       0.6305814 | GJ              |
|        101 | Company 1    |      101 | DE           | Company 1  |            2024 |       0.0620259 | Gas        | Oil&Gas |     8600 |       0.7209302 | GJ              |
|        101 | Company 1    |      101 | DE           | Company 1  |            2025 |       0.0620259 | Gas        | Oil&Gas |     8600 |       0.8604651 | GJ              |
|        101 | Company 1    |      101 | DE           | Company 1  |            2026 |       0.0620259 | Gas        | Oil&Gas |     8600 |       0.9069767 | GJ              |
|        101 | Company 1    |      101 | DE           | Company 1  |            2027 |       0.0620259 | Gas        | Oil&Gas |     8600 |       1.0000000 | GJ              |

------------------------------------------------------------------------

### Financial Features Test Data

This dataset contains financial metrics necessary for calculating stress
test outputs.

#### Data Description

The `financial_features_testdata` dataset includes the following
columns:

- `company_id`: Unique identifier for the company.
- `pd`: Probability of default for the company.
- `net_profit_margin`: Net profit margin for the company.
- `debt_equity_ratio`: Debt to equity ratio.
- `volatility`: Volatility of the company’s asset values.

#### Data Structure

``` r
str(financial_features_testdata)
#> 'data.frame':    5 obs. of  5 variables:
#>  $ company_id       : int  101 103 105 104 102
#>  $ pd               : num  0.00562 0.00398 0.00246 0.00298 0.00365
#>  $ net_profit_margin: num  0.0764 0.0717 0.0539 0.0539 0.1058
#>  $ debt_equity_ratio: num  0.13 0.128 0.119 0.11 0.104
#>  $ volatility       : num  0.259 0.251 0.236 0.251 0.317
```

#### Sample Data

``` r
knitr::kable(head(financial_features_testdata)) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")
```

| company_id |        pd | net_profit_margin | debt_equity_ratio | volatility |
|-----------:|----------:|------------------:|------------------:|-----------:|
|        101 | 0.0056224 |         0.0763542 |         0.1297317 |  0.2593230 |
|        103 | 0.0039782 |         0.0716949 |         0.1277164 |  0.2513500 |
|        105 | 0.0024568 |         0.0539341 |         0.1194000 |  0.2360043 |
|        104 | 0.0029792 |         0.0539341 |         0.1097633 |  0.2513500 |
|        102 | 0.0036483 |         0.1057878 |         0.1044025 |  0.3167116 |

------------------------------------------------------------------------

### NGFS Carbon Price Test Data

This dataset provides carbon pricing data used in the stress test
scenarios.

#### Data Description

The `ngfs_carbon_price_testdata` dataset includes the following columns:

- `year`: Year of the carbon price.
- `model`: Model used to generate the carbon price.
- `scenario`: Scenario name.
- `scenario_geography`: Geographic region for the scenario.
- `variable`: The variable measured (e.g., carbon price).
- `unit`: Unit of the variable.
- `carbon_tax`: The amount of carbon tax applied in the scenario.

#### Data Structure

``` r
str(ngfs_carbon_price_testdata)
#> 'data.frame':    1376 obs. of  7 variables:
#>  $ year              : int  2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 ...
#>  $ model             : chr  "GCAM 5.3+ NGFS" "GCAM 5.3+ NGFS" "GCAM 5.3+ NGFS" "GCAM 5.3+ NGFS" ...
#>  $ scenario          : chr  "B2DS" "B2DS" "B2DS" "B2DS" ...
#>  $ scenario_geography: chr  "Global" "Global" "Global" "Global" ...
#>  $ variable          : chr  "Price|Carbon" "Price|Carbon" "Price|Carbon" "Price|Carbon" ...
#>  $ unit              : chr  "US$2010/t CO2" "US$2010/t CO2" "US$2010/t CO2" "US$2010/t CO2" ...
#>  $ carbon_tax        : num  0 0 0 0 0 0 0 0 0 0 ...
```

#### Sample Data

``` r
knitr::kable(head(ngfs_carbon_price_testdata)) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")
```

| year | model          | scenario | scenario_geography | variable          | unit           | carbon_tax |
|-----:|:---------------|:---------|:-------------------|:------------------|:---------------|-----------:|
| 2015 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |
| 2016 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |
| 2017 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |
| 2018 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |
| 2019 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |
| 2020 | GCAM 5.3+ NGFS | B2DS     | Global             | Price&#124;Carbon | US\$2010/t CO2 |          0 |

------------------------------------------------------------------------

### Scenarios Test Data

This dataset contains scenario-specific data including price paths,
capacity factors, and other relevant information.

#### Data Description

The `scenarios_testdata` dataset includes the following columns:

- `scenario_geography`: Region relevant to the scenario.
- `scenario`: Scenario name.
- `scenario_pathway`: Specific pathway for the scenario.
- `scenario_type`: Type of scenario (e.g., baseline, shock).
- `sector`: Sector of production.
- `technology`: Type of technology.
- `scenario_year`: Year of the scenario data.
- `scenario_price`: Price in the scenario.
- `price_unit`: Unit for the price.
- `pathway_unit`: Unit of the pathway.
- `technology_type`: Type of technology involved.
- `capacity_factor_unit`: Unit for the capacity factor.
- `price_indicator`: Indicator for the price path.

#### Data Structure

``` r
str(scenarios_testdata)
#> 'data.frame':    1422 obs. of  14 variables:
#>  $ scenario                : chr  "NGFS2023GCAM_CP" "NGFS2023GCAM_CP" "NGFS2023GCAM_CP" "NGFS2023GCAM_CP" ...
#>  $ scenario_type           : chr  "baseline" "baseline" "baseline" "baseline" ...
#>  $ scenario_geography      : chr  "Global" "Global" "Global" "Global" ...
#>  $ sector                  : chr  "Coal" "Coal" "Coal" "Coal" ...
#>  $ technology              : chr  "Coal" "Coal" "Coal" "Coal" ...
#>  $ scenario_year           : int  2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 ...
#>  $ price_unit              : chr  "$/tonnes" "$/tonnes" "$/tonnes" "$/tonnes" ...
#>  $ scenario_price          : num  57 57.4 57.7 58 58.4 ...
#>  $ pathway_unit            : chr  "EJ/yr" "EJ/yr" "EJ/yr" "EJ/yr" ...
#>  $ scenario_pathway        : num  159 160 161 162 162 ...
#>  $ technology_type         : chr  "carbontech" "carbontech" "carbontech" "carbontech" ...
#>  $ scenario_capacity_factor: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ country_iso2_list       : logi  NA NA NA NA NA NA ...
#>  $ scenario_provider       : chr  "NGFS2023GCAM" "NGFS2023GCAM" "NGFS2023GCAM" "NGFS2023GCAM" ...
```

#### Sample Data

``` r
knitr::kable(head(scenarios_testdata, 50)) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::scroll_box(width = "200%", height = "400px")
```

| scenario        | scenario_type | scenario_geography | sector | technology | scenario_year | price_unit | scenario_price | pathway_unit | scenario_pathway | technology_type | scenario_capacity_factor | country_iso2_list | scenario_provider |
|:----------------|:--------------|:-------------------|:-------|:-----------|--------------:|:-----------|---------------:|:-------------|-----------------:|:----------------|-------------------------:|:------------------|:------------------|
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2022 | \$/tonnes  |       57.03917 | EJ/yr        |         159.4468 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2023 | \$/tonnes  |       57.35451 | EJ/yr        |         160.4324 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2024 | \$/tonnes  |       57.66985 | EJ/yr        |         161.4180 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2025 | \$/tonnes  |       57.98520 | EJ/yr        |         162.4035 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2026 | \$/tonnes  |       58.41776 | EJ/yr        |         162.4545 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2027 | \$/tonnes  |       58.85032 | EJ/yr        |         162.5055 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2028 | \$/tonnes  |       59.28289 | EJ/yr        |         162.5565 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2029 | \$/tonnes  |       59.71545 | EJ/yr        |         162.6075 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2030 | \$/tonnes  |       60.14802 | EJ/yr        |         162.6585 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2031 | \$/tonnes  |       60.53991 | EJ/yr        |         163.5647 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2032 | \$/tonnes  |       60.93181 | EJ/yr        |         164.4709 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2033 | \$/tonnes  |       61.32370 | EJ/yr        |         165.3771 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2034 | \$/tonnes  |       61.71560 | EJ/yr        |         166.2833 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2035 | \$/tonnes  |       62.10749 | EJ/yr        |         167.1895 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2036 | \$/tonnes  |       62.28684 | EJ/yr        |         167.6793 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2037 | \$/tonnes  |       62.46619 | EJ/yr        |         168.1691 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2038 | \$/tonnes  |       62.64553 | EJ/yr        |         168.6589 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2039 | \$/tonnes  |       62.82488 | EJ/yr        |         169.1487 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2040 | \$/tonnes  |       63.00422 | EJ/yr        |         169.6385 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2041 | \$/tonnes  |       63.11835 | EJ/yr        |         169.6492 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2042 | \$/tonnes  |       63.23248 | EJ/yr        |         169.6599 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2043 | \$/tonnes  |       63.34661 | EJ/yr        |         169.6706 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2044 | \$/tonnes  |       63.46074 | EJ/yr        |         169.6813 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2045 | \$/tonnes  |       63.57487 | EJ/yr        |         169.6920 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2046 | \$/tonnes  |       63.60577 | EJ/yr        |         169.3689 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2047 | \$/tonnes  |       63.63667 | EJ/yr        |         169.0457 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2048 | \$/tonnes  |       63.66757 | EJ/yr        |         168.7226 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2049 | \$/tonnes  |       63.69847 | EJ/yr        |         168.3994 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2050 | \$/tonnes  |       63.72937 | EJ/yr        |         168.0763 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2051 | \$/tonnes  |       63.73330 | EJ/yr        |         167.9941 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2052 | \$/tonnes  |       63.73723 | EJ/yr        |         167.9119 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2053 | \$/tonnes  |       63.74115 | EJ/yr        |         167.8298 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2054 | \$/tonnes  |       63.74508 | EJ/yr        |         167.7476 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2055 | \$/tonnes  |       63.74900 | EJ/yr        |         167.6655 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2056 | \$/tonnes  |       63.66008 | EJ/yr        |         167.3440 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2057 | \$/tonnes  |       63.57115 | EJ/yr        |         167.0226 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2058 | \$/tonnes  |       63.48223 | EJ/yr        |         166.7011 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2059 | \$/tonnes  |       63.39331 | EJ/yr        |         166.3796 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2060 | \$/tonnes  |       63.30438 | EJ/yr        |         166.0582 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2061 | \$/tonnes  |       63.18854 | EJ/yr        |         165.4385 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2062 | \$/tonnes  |       63.07270 | EJ/yr        |         164.8189 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2063 | \$/tonnes  |       62.95686 | EJ/yr        |         164.1992 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2064 | \$/tonnes  |       62.84101 | EJ/yr        |         163.5796 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2065 | \$/tonnes  |       62.72517 | EJ/yr        |         162.9599 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2066 | \$/tonnes  |       62.58408 | EJ/yr        |         161.8002 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2067 | \$/tonnes  |       62.44300 | EJ/yr        |         160.6404 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2068 | \$/tonnes  |       62.30191 | EJ/yr        |         159.4807 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2069 | \$/tonnes  |       62.16082 | EJ/yr        |         158.3210 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2070 | \$/tonnes  |       62.01973 | EJ/yr        |         157.1612 | carbontech      |                        1 | NA                | NGFS2023GCAM      |
| NGFS2023GCAM_CP | baseline      | Global             | Coal   | Coal       |          2071 | \$/tonnes  |       61.87448 | EJ/yr        |         156.1628 | carbontech      |                        1 | NA                | NGFS2023GCAM      |

------------------------------------------------------------------------

## Data Preparation

Before running the model, ensure that your datasets are correctly
formatted and contain the necessary information. Use the provided test
datasets as templates for your own data.

For the scenarios, make sure that the `baseline_scenario` and
`target_scenario` you plan to use are present in your `scenarios`
dataframe, and that they correspond to the `scenario_geography` you are
analyzing.
