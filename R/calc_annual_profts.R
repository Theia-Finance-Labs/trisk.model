#' Calculate annual profits
#'
#' Wrapper function to calculate discounted annual profits and terminal value.
#'
#' @param data data frame containing the full trajectory company data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param shock_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
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


#' Fill missing values on annual_profits
#'
#' Function fill missing rows on cols company_id, pd, net_profit_margin,
#' debt_equity_ratio, volatility.
#'
#' @param annual_profits A tibble holding annual profit data.
#'
#' @return Tibble holding `annual profits` with replaces missings.
fill_annual_profit_cols <- function(annual_profits) {
  annual_profits_filled <- annual_profits %>%
    dplyr::arrange(
      .data$scenario_name, .data$scenario_geography, .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit, .data$year
    ) %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit
    ) %>%
    # NOTE: this assumes emissions factors stay constant after forecast and prod not continued
    tidyr::fill(
      # TODO: what is company_id even doing here?
      # company_id,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio, .data$volatility,
      .direction = "down"
    ) %>%
    dplyr::ungroup()

  return(annual_profits_filled)
}
