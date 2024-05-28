#' Calculate percentage value change between scenarios for equity (and
#' temporarily other asset types) on the company-ald_business_unit level
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param shock_scenario A dataframe containing the specification of the
#'   shock scenario at hand
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value
#' @param flat_multiplier Numeric. A ratio that determines for the asset type
#'   if how strongly the DCF should propagate to value changes.
#' @param crispy Boolean. Indicates if the output should be used for the CRISPY
#'   database or for standard portfolio calculation (default).
company_technology_asset_value_at_risk <- function(data,
                                                   shock_year,
                                                   start_year,
                                                   div_netprofit_prop_coef = NULL,
                                                   flat_multiplier = NULL,
                                                   crispy = FALSE) {
                                                    
  data <- data %>%
    dplyr::filter(
      .data$year >= shock_year,
      !is.na(.data$discounted_net_profit_ls),
      !is.na(.data$discounted_net_profit_baseline)
    ) %>%
    dplyr::group_by(
      .data$company_id, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::summarise(
      total_disc_npv_ls = sum(.data$discounted_net_profit_ls),
      total_disc_npv_baseline = sum(.data$discounted_net_profit_baseline),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      VaR_tech_company = .env$flat_multiplier * 100 * .env$div_netprofit_prop_coef *
        (.data$total_disc_npv_ls - .data$total_disc_npv_baseline) /
        .data$total_disc_npv_baseline
    )

  if (crispy) {
    data <- data %>%
      dplyr::select(-c(.data$VaR_tech_company))
  } else {
    data <- data %>%
      dplyr::select(-c(.data$total_disc_npv_ls, .data$total_disc_npv_baseline))
  }

  data <- data %>%
    dplyr::mutate(
      duration_of_shock = shock_year - start_year,
      year_of_shock = shock_year
    )

  return(data)
}
