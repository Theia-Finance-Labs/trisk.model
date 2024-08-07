#' Calculate annual profits
#'
#' Wrapper function to calculate discounted annual profits and terminal value.
#'
#' @param data data frame containing the full trajectory company data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline technology trajectories.
#' @param shock_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden technology trajectories.
#' @param end_year Numeric, holding end year of analysis.
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the `end_year` in the DCF.
#'
#' @return A tibble holding annual profits
calculate_annual_profits <- function(data,
                                     baseline_scenario,
                                     shock_scenario,
                                     end_year,
                                     discount_rate,
                                     growth_rate) {
  data <- data %>%
    dividend_discount_model(discount_rate = discount_rate) %>%
    calculate_terminal_value(
      end_year = end_year,
      growth_rate = growth_rate,
      discount_rate = discount_rate,
      baseline_scenario = baseline_scenario,
      shock_scenario = shock_scenario
    )

  return(data)
}




#' Calculates discounted net profits based on a dividends discount model
#'
#' @param data A data frame containing the annual net profits on company
#'   technology level
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF.
dividend_discount_model <- function(data, discount_rate) {
  data <- data %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$sector, .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      t_calc = seq(0, (dplyr::n() - 1)),
      discounted_net_profit_baseline = .data$net_profits_baseline /
        (1 + .env$discount_rate)^.data$t_calc,
      discounted_net_profit_ls = .data$net_profits_ls /
        (1 + .env$discount_rate)^.data$t_calc
    ) %>%
    dplyr::select(-.data$t_calc) %>%
    dplyr::ungroup()

  return(data)
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
      late_sudden = NA_real_,
      scenario_price_baseline = NA_real_,
      late_sudden_price = NA_real_,
      production_compensation = NA_real_
    )

  data <- data %>%
    dplyr::bind_rows(terminal_value) %>%
    dplyr::arrange(
      .data$company_id, .data$scenario_geography, .data$company_name, .data$sector,
      .data$technology, .data$year
    )

  return(data)
}
