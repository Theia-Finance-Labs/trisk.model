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
                                                   flat_multiplier = 1,
                                                   crispy = FALSE) {
  force(data)
  
  div_netprofit_prop_coef %||% stop("Must provide input for 'div_netprofit_prop_coef'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "year", "scenario_geography", "ald_sector", "ald_business_unit",
      "discounted_net_profit_ls", "discounted_net_profit_baseline"
    )
  )
  
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
      scenario_name = "TEST",
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
      duration_of_shock = .env$shock_year - .env$start_year,
      year_of_shock = .env$shock_year
    )

  return(data)
}
