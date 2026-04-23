# Calculate annual profits

Wrapper function to calculate discounted annual profits and terminal
value.

## Usage

``` r
calculate_annual_profits(
  data,
  baseline_scenario,
  shock_scenario,
  start_year,
  end_year,
  discount_rate,
  growth_rate
)
```

## Arguments

- data:

  data frame containing the full trajectory company data

- baseline_scenario:

  Character. A string that indicates which of the scenarios included in
  the analysis should be used to set the baseline technology
  trajectories.

- shock_scenario:

  Character. A string that indicates which of the scenarios included in
  the analysis should be used to set the late & sudden technology
  trajectories.

- start_year:

  Numeric, holding start year of analysis. Used to pull the terminal
  value back to present value as per the 2DII "Limited Visibility"
  methodology (Figure 1).

- end_year:

  Numeric, holding end year of analysis.

- discount_rate:

  Numeric, the discount rate

- growth_rate:

  Numeric, that holds the terminal growth rate of profits beyond the
  `end_year` in the DCF.

## Value

A tibble holding annual profits
