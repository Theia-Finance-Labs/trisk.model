# Calculate scenario prices

Function generates prices for baseline and late and sudden shock
scenario. Price for baseline scenario correspond to prices of
`baseline_scenario`. Prices for the late sudden scenario also correspond
to `baseline_scenario` until the `year_of_shock`. From then on they
linearly approach the price level of the `shock_scenario` during the
`duration_of_shock`.

## Usage

``` r
apply_scenario_prices(data, shock_year)
```

## Arguments

- data:

  A tibble holding price data.

- shock_year:

  Year of shock.

## Value

A tibble holding late_and_sudden_prices
