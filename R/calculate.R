#' Calculate transition shock trajectory
#'
#' @param input_data_list List with project agnostic and project specific input data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Considered timeframe for PACTA analysis.
#'
#' @return A tibble holding annual profits
calculate_trisk_trajectory <- function(input_data_list,
                                       baseline_scenario,
                                       target_scenario,
                                       shock_year,
                                       start_year,
                                       end_year,
                                       time_horizon) {
  production_data <- input_data_list$production_data %>%
    set_baseline_trajectory(
      baseline_scenario = baseline_scenario
    ) %>%
    set_trisk_trajectory(
      target_scenario = target_scenario,
      target_scenario_aligned = target_scenario,
      start_year = start_year,
      end_year = end_year,
      shock_year=shock_year,
      analysis_time_frame = time_horizon
    )

  price_data <- input_data_list$df_price %>%
    calc_scenario_prices(
      baseline_scenario = baseline_scenario,
      target_scenario = target_scenario,
      start_year = start_year
    )

  full_trajectory <- production_data %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = c("company_id")
    ) %>%
    fill_annual_profit_cols()

  full_trajectory <- full_trajectory %>%
    join_price_data(df_prices = price_data)

  return(full_trajectory)
}


calculate_terminal_value <- function(data,
                                     end_year,
                                     growth_rate,
                                     discount_rate,
                                     baseline_scenario,
                                     shock_scenario) {
  # the calculation follows the formula described in the 2DII paper "Limited
  # Visibility", available under https://2degrees-investing.org/resource/limited-visibility-the-current-state-of-corporate-disclosure-on-long-term-risks/
  terminal_value <- data %>%
    dplyr::filter(.data$year == .env$end_year) %>%
    dplyr::mutate(
      year = .env$end_year + 1,
      net_profits_baseline = .data$net_profits_baseline * (1 + .env$growth_rate),
      net_profits_ls = .data$net_profits_ls * (1 + .env$growth_rate),
      discounted_net_profit_baseline = .data$net_profits_baseline /
        (.env$discount_rate - .env$growth_rate),
      discounted_net_profit_ls = .data$net_profits_ls /
        (.env$discount_rate - .env$growth_rate)
    ) %>%
    # ADO3112: All columns that reflect a change over time are set to NA, as
    # they cannot be extrapolated from the start_year to end_year period. All
    # columns that are time invariant are kept.
    dplyr::mutate(
      !!rlang::sym(baseline_scenario) := NA_real_,
      !!rlang::sym(shock_scenario) := NA_real_,
      baseline = NA_real_,
      scen_to_follow_aligned = NA_real_,
      late_sudden = NA_real_,
      Baseline_price = NA_real_,
      late_sudden_price = NA_real_,
      production_compensation = NA_real_
    )

  data <- data %>%
    dplyr::bind_rows(terminal_value) %>%
    dplyr::arrange(
      .data$company_id, .data$scenario_geography, .data$company_name, .data$ald_sector,
      .data$ald_business_unit, .data$year
    )

  return(data)
}
