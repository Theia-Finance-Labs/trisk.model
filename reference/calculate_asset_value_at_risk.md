# Calculate percentage value change between scenarios for equity (and temporarily other asset types) on the company-technology level

Calculate percentage value change between scenarios for equity (and
temporarily other asset types) on the company-technology level

## Usage

``` r
calculate_asset_value_at_risk(
  data,
  shock_year,
  start_year,
  div_netprofit_prop_coef = NULL,
  flat_multiplier = 1,
  crispy = FALSE
)
```

## Arguments

- data:

  A dataframe containing the (discounted) annual profits

- shock_year:

  The shock_year

- start_year:

  the start_year

- div_netprofit_prop_coef:

  Numeric. A coefficient that determines how strongly the future
  dividends propagate to the company value

- flat_multiplier:

  Numeric. A ratio that determines for the asset type if how strongly
  the DCF should propagate to value changes.

- crispy:

  Boolean. Indicates if the output should be used for the CRISPY
  database or for standard portfolio calculation (default).
