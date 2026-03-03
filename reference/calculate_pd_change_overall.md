# Calculate change in probabilities of default (PDs) of loans connected to companies at hand. This is based on the equity values derived from the DCF model. Said Equity values are used as different starting points for the Merton model (one reflecting the business as usual baseline scenario, the other reflecting the late & sudden shock scenario). The change in PDs can then be used to calculate the Expected Loss due to the shock on the portfolio level.

Calculate change in probabilities of default (PDs) of loans connected to
companies at hand. This is based on the equity values derived from the
DCF model. Said Equity values are used as different starting points for
the Merton model (one reflecting the business as usual baseline
scenario, the other reflecting the late & sudden shock scenario). The
change in PDs can then be used to calculate the Expected Loss due to the
shock on the portfolio level.

## Usage

``` r
calculate_pd_change_overall(
  data,
  shock_year = NULL,
  start_year = NULL,
  end_of_analysis = NULL,
  risk_free_interest_rate = NULL
)
```

## Arguments

- data:

  A dataframe containing the (discounted) annual profits

- shock_year:

  A numeric vector of length one that indicates in which year the policy
  shock strikes in a given scenario

- start_year:

  The start year.

- end_of_analysis:

  A numeric vector of length one that indicates until which year the
  analysis runs

- risk_free_interest_rate:

  A numeric vector of length one that indicates the risk free rate of
  interest
