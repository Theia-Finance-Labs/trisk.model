#' Calculate scenario prices
#'
#' Function generates prices for baseline and late and sudden shock scenario.
#' Price for baseline scenario correspond to prices of `baseline_scenario`.
#' Prices for the late sudden scenario also correspond to `baseline_scenario`
#' until the `year_of_shock`. From then on they linearly approach the price
#' level of the `shock_scenario` during the `duration_of_shock`.
#'
#'
#' @param price_data A tibble holding price data.
#' @param baseline_scenario String holding name of the baseline scenario.
#' @param target_scenario String holding name of the target scenario.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Start_year of analysis
#'
#' @return A tibble holding late_and_sudden_prices
calc_scenario_prices <- function(price_data, baseline_scenario, target_scenario, start_year, shock_year, duration_of_shock) {
  data <- price_data %>%
    dplyr::group_by(.data$ald_sector, .data$ald_business_unit) %>%
    dplyr::mutate(
      late_sudden_price = late_sudden_prices(
        price_target_scenario = .data$price_target_scenario,
        baseline_price = .data$price_baseline_scenario,
        year_of_shock = shock_year,
        start_year = start_year,
        duration_of_shock = duration_of_shock
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$year, .data$ald_sector, .data$ald_business_unit, .data$price_baseline_scenario, .data$late_sudden_price)

  return(data)
}
