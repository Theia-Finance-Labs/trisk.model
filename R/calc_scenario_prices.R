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
#' @param shock_year Year of shock.
#'
#' @return A tibble holding late_and_sudden_prices
apply_scenario_prices <- function(data, shock_year) {
  # Part 1: Process for years <= shock_year
  before_shock <- data %>%
    dplyr::filter(.data$year <= shock_year) %>%
    dplyr::mutate(late_sudden_price = .data$price_baseline)

  # Part 2: Process for years > shock_year
  after_shock <- data %>%
    dplyr::filter(.data$year > shock_year - 1) %>%
    dplyr::group_by(.data$company_id, .data$ald_business_unit) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::summarise(
      baseline_price_at_shock = dplyr::first(.data$price_baseline),
      target_price_end_shockperiod = dplyr::last(.data$price_target),
      first_year = min(.data$year) + 1,
      last_year = max(.data$year)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Create a sequence of years from first_year to last_year
      year = list(seq(.data$first_year, .data$last_year)),
      # Create a linear interpolation from baseline to target
      ls_price_full = list(
        zoo::na.approx(
          c(baseline_price_at_shock, target_price_end_shockperiod),
          x = c(.data$first_year - 1, .data$last_year),
          xout = seq(.data$first_year - 1, .data$last_year),
          na.rm = FALSE
        )
      ),
      # Remove the first value from ls_price_full
      late_sudden_price = list(tail(.data$ls_price_full, -1))
    ) %>%
    tidyr::unnest(c(.data$year, .data$late_sudden_price)) %>%
    dplyr::select(.data$company_id, .data$ald_business_unit, .data$year, .data$late_sudden_price)

  # Combine both parts
  final_result <- dplyr::bind_rows(before_shock, after_shock) %>%
    dplyr::select(.data$company_id, .data$ald_business_unit, .data$year, .data$late_sudden_price)


  data <- data %>%
    dplyr::inner_join(final_result, by = c("company_id", "ald_business_unit", "year"))


  return(data)
}
