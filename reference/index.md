# Package index

## All functions

- [`apply_scenario_prices()`](apply_scenario_prices.md) : Calculate
  scenario prices
- [`calc_survival_probability_merton()`](calc_survival_probability_merton.md)
  : Calculate survival probability
- [`calculate_annual_profits()`](calculate_annual_profits.md) :
  Calculate annual profits
- [`calculate_asset_value_at_risk()`](calculate_asset_value_at_risk.md)
  : Calculate percentage value change between scenarios for equity (and
  temporarily other asset types) on the company-technology level
- [`calculate_net_profits()`](calculate_net_profits.md) : Calculates
  annual net profits on the company-technology level for the baseline
  and late and sudden scenarios. Climate laggards which need to build
  out their production in increasing technologies to compensate for
  their missed targets, are "punished" by adjusting the net profit
  margin on their additional build out based on their proximity to
  target within the given technology. Specifically, we measure the ratio
  of how much of the required build out or reduction in a technology the
  company will have done at the end of the forecast period. If the
  technology has an increasing target and the ratio of completion is
  below one, the net_profit_margin on the additional production build
  out is multiplied with the proximity to the target. This approximates
  the additional capital investment such a company would have to make in
  a short time, which leads to added costs. This ensures that late build
  out will not proportionally translate into increased profits.
- [`calculate_net_profits_baseline()`](calculate_net_profits_baseline.md)
  : Calculates annual net profits on the company-technology level for
  the baseline scenario
- [`calculate_net_profits_shock_declining_technologies_carbon_tax()`](calculate_net_profits_shock_declining_technologies_carbon_tax.md)
  : Calculates annual net profits on the company-technology level for
  the baseline and late and sudden scenarios - with a carbon tax being
  added.
- [`calculate_net_profits_shock_increasing_technologies()`](calculate_net_profits_shock_increasing_technologies.md)
  : Calculates annual net profits on the company-technology level for
  the shock scenario for increasing technologies. Climate laggards which
  need to build out their production in increasing technologies to
  compensate for their missed targets, are "punished" by adjusting the
  net profit margin on their additional build out based on their
  proximity to target within the given technology. Specifically, we
  measure the ratio of how much of the required build out or reduction
  in a technology the company will have done at the end of the forecast
  period. If the technology has an increasing target and the ratio of
  completion is below one, the net_profit_margin on the additional
  production build out is multiplied with the proximity to the target.
  This approximates the additional capital investment such a company
  would have to make in a short time, which leads to added costs. This
  ensures that late build out will not proportionally translate into
  increased profits.
- [`calculate_pd_change_overall()`](calculate_pd_change_overall.md) :
  Calculate change in probabilities of default (PDs) of loans connected
  to companies at hand. This is based on the equity values derived from
  the DCF model. Said Equity values are used as different starting
  points for the Merton model (one reflecting the business as usual
  baseline scenario, the other reflecting the late & sudden shock
  scenario). The change in PDs can then be used to calculate the
  Expected Loss due to the shock on the portfolio level.
- [`check_valid_financial_data_values()`](check_valid_financial_data_values.md)
  : Check if values in financial data are plausible
- [`dividend_discount_model()`](dividend_discount_model.md) : Calculates
  discounted net profits based on a dividends discount model
- [`extend_assets_trajectories()`](extend_assets_trajectories.md) :
  Calculate baseline and transition shock trajectoroes
- [`keep_merton_compatible_rows()`](keep_merton_compatible_rows.md) :
  Keep rows that fulfill constraints of the merton model
- [`process_params()`](process_params.md) : Process function parameters
- [`read_carbon_data()`](read_carbon_data.md) : Read in carbon price
  data from ngfs data
- [`read_financial_data()`](read_financial_data.md) : Read in company
  financial data processed from eikon exports and AR master data that
  contain information on multiple credit risk inputs
- [`read_production_data()`](read_production_data.md) : Read in AR PAMS
  production data.
- [`read_scenario_data()`](read_scenario_data.md) : Read in scenario
  data
- [`retype_data()`](retype_data.md) : Retype Data Columns
- [`run_trisk()`](run_trisk.md) : Run stress testing for provided asset
  type
- [`run_trisk_model()`](run_trisk_model.md) : Run transition risk stress
  test model
- [`set_baseline_trajectory()`](set_baseline_trajectory.md) : Defines
  which scenario values to use for the baseline trajectory in the stress
  test.
- [`set_trisk_trajectory()`](set_trisk_trajectory.md) : Defines which
  scenario values to use for the late & sudden trajectory in the stress
  test.
- [`st_read_agnostic()`](st_read_agnostic.md) : Read agnostic data from
  a directory
- [`validate_data_has_expected_cols()`](validate_data_has_expected_cols.md)
  : Validate that a data frame contains expected columns
- [`validate_file_exists()`](validate_file_exists.md) : Validate that a
  file exists in a given directory
