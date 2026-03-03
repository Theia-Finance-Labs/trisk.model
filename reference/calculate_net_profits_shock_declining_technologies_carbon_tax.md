# Calculates annual net profits on the company-technology level for the baseline and late and sudden scenarios - with a carbon tax being added.

Calculates annual net profits on the company-technology level for the
baseline and late and sudden scenarios - with a carbon tax being added.

## Usage

``` r
calculate_net_profits_shock_declining_technologies_carbon_tax(
  data,
  shock_year,
  carbon_data,
  market_passthrough
)
```

## Arguments

- data:

  A data frame containing the production forecasts of companies under
  baseline and late and sudden, market prices/costs, company net profit
  margins, the proximity to target in the production forecast period and
  an indication of the direction of the technology.

- shock_year:

  A numeric vector of length one that indicates in which year the policy
  shock strikes in a given scenario.

- carbon_data:

  NGFS carbon prices.

- market_passthrough:

  A firm's ability to pass a carbon tax onto the consumer.

## Value

Data frame with annual netprofits for all cases without carbon tax.
