process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography, start_analysis, end_analysis) {
  scenarios_data <- data$scenario_data

  scenarios_data <- scenarios_data %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_analysis, .env$end_analysis)) %>%
    tidyr::replace_na(list(capacity_factor = 1)) %>%
    dplyr::arrange(year, .by_group = TRUE)

  return(scenarios_data)
}

process_assets_data <- function(data, start_analysis, end_analysis, scenario_geography) {
  production_financial_data <- dplyr::inner_join(
    data$production_data,
    data$financial_data,
    by = "company_id"
  ) %>%
    dplyr::filter(.data$scenario_geography == .env$scenario_geography)

  assets_data <- production_financial_data %>%
    extend_to_full_analysis_timeframe(
      start_analysis = start_analysis,
      end_analysis = end_analysis
    )

  return(assets_data)
}




#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries fo their forecasts and the phase out indicator.
#' @param start_analysis start of the analysis
#' @param end_analysis end of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,
                                              start_analysis,
                                              end_analysis) {
  data <- data %>%
    tidyr::complete(
      year = seq(.env$start_analysis, .env$end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "company_id", "company_name", "ald_sector", "technology", "scenario_geography"
          )
        )
      )
    ) %>%
    dplyr::arrange(
      .data$company_id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography, .data$year
    ) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility"
      ))
    )

  return(data)
}
