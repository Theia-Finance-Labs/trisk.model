# Run transition risk stress test model

This function executes the core transition risk stress test model
calculations.

## Usage

``` r
run_trisk_model(
  assets_data,
  scenarios_data,
  financial_data,
  carbon_data,
  baseline_scenario = "",
  target_scenario = "",
  scenario_geography = "Global",
  carbon_price_model = "no_carbon_tax",
  risk_free_rate = 0.02,
  discount_rate = 0.07,
  growth_rate = 0.03,
  div_netprofit_prop_coef = 1,
  shock_year = 2030,
  market_passthrough = 0,
  run_id = NULL
)
```

## Arguments

- assets_data:

  Data frame containing asset information.

- scenarios_data:

  Data frame containing scenario information.

- financial_data:

  Data frame containing financial information.

- carbon_data:

  Data frame containing carbon price information.

- baseline_scenario:

  String specifying the name of the baseline scenario.

- target_scenario:

  String specifying the name of the shock scenario.

- scenario_geography:

  Character vector indicating which geographical region(s) to calculate
  results for.

- carbon_price_model:

  Character vector specifying which NGFS model to use for carbon prices.
  Default is "no_carbon_tax".

- risk_free_rate:

  Numeric value for the risk-free interest rate. Default is 0.02.

- discount_rate:

  Numeric value for the discount rate of dividends per year in the DCF.
  Default is 0.07.

- growth_rate:

  Numeric value for the terminal growth rate of profits beyond the final
  year in the DCF. Default is 0.03.

- div_netprofit_prop_coef:

  Numeric coefficient determining how strongly future dividends
  propagate to company value. Default is 1.

- shock_year:

  Numeric value specifying the year when the shock is applied. Default
  is 2030.

- market_passthrough:

  Numeric value representing the firm's ability to pass carbon tax onto
  the consumer. Default is 0.

- run_id:

  (Optional) Character value representing the ID of current Trisk's
  iteration

## Value

A list containing:

- pd_results:

  Data frame of overall probability of default changes

- company_trajectories:

  Data frame of company annual profits

- npv_results:

  Data frame of company technology net present values
