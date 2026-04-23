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
#' @param start_year Numeric, holding start year of analysis. Used to pull the
#'   terminal value back to present value as per the 2DII "Limited Visibility"
#'   methodology (Figure 1).
#' @param end_year Numeric, holding end year of analysis.
#' @param discount_rate Numeric, the discount rate
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the `end_year` in the DCF.
#'
#' @return A tibble holding annual profits
calculate_annual_profits <- function(data,
                                     baseline_scenario,
                                     shock_scenario,
                                     start_year,
                                     end_year,
                                     discount_rate,
                                     growth_rate) {
  data <- data %>%
    dividend_discount_model(discount_rate = discount_rate) %>%
    calculate_terminal_value(
      start_year = start_year,
      end_year = end_year,
      growth_rate = growth_rate,
      discount_rate = discount_rate
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
      .data$asset_id, .data$company_id, .data$sector, .data$technology,
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




# Terminal value follows the stable-growth formula from the 2DII paper
# "Limited Visibility" (https://2degrees-investing.org/resource/limited-visibility-the-current-state-of-corporate-disclosure-on-long-term-risks/).
#   Appendix 3, Figure 1:  TV_at_end_year = CF[end_year] * (1 + g) / (r - g)
#   Main body, Figure 1:   Discounted TV  = TV_at_end_year / (1 + r)^(end_year - start_year)
# The pull-back factor `(1 + r)^(end_year - start_year)` is required so the
# terminal-value row can be summed with the other already-discounted flows
# (Appendix 3: "explicit value" and "extrapolated value" are summed with "all
# values discounted").
calculate_terminal_value <- function(data,
                                     start_year,
                                     end_year,
                                     growth_rate,
                                     discount_rate) {
  horizon <- end_year - start_year
  terminal_value <- data %>%
    dplyr::filter(.data$year == .env$end_year) %>%
    dplyr::mutate(
      year = .env$end_year + 1,
      net_profits_baseline = .data$net_profits_baseline * (1 + .env$growth_rate),
      net_profits_ls = .data$net_profits_ls * (1 + .env$growth_rate),
      discounted_net_profit_baseline = .data$net_profits_baseline /
        (.env$discount_rate - .env$growth_rate) /
        (1 + .env$discount_rate)^.env$horizon,
      discounted_net_profit_ls = .data$net_profits_ls /
        (.env$discount_rate - .env$growth_rate) /
        (1 + .env$discount_rate)^.env$horizon
    ) %>%
    # ADO3112: All columns that reflect a change over time are set to NA, as
    # they cannot be extrapolated from the start_year to end_year period. All
    # columns that are time invariant are kept.
    dplyr::mutate(
      production_plan_company_technology = NA_real_,
      production_scenario_baseline = NA_real_,
      production_scenario_target = NA_real_,
      production_change_scenario_baseline = NA_real_,
      production_change_scenario_target = NA_real_,
      production_asset_baseline = NA_real_,
      late_sudden = NA_real_,
      scenario_price_baseline = NA_real_,
      scenario_price_target = NA_real_,
      late_sudden_price = NA_real_
    )

  data <- data %>%
    dplyr::bind_rows(terminal_value) %>%
    dplyr::arrange(
      .data$asset_id, .data$company_id, .data$sector, .data$technology, .data$year
    )

  return(data)
}
