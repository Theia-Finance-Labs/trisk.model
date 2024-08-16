
merge_assets_and_scenarios_data <- function(assets_data, scenarios_data) {

    start_analysis <- min(assets_data$production_year)
    end_analysis <- max(scenarios_data$scenario_year)

    assets_data <- assets_data %>%
        extend_to_full_analysis_timeframe(start_analysis, end_analysis) 
    
  # add extend production data with scenario targets
  assets_scenarios <- dplyr::inner_join(
    assets_data, scenarios_data,
    by = c("sector", "technology", "scenario_geography", "production_year" = "scenario_year")
  ) %>%
    dplyr::rename(year = .data$production_year) 

  return(assets_scenarios)
}



#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries of their forecasts, and the phase-out indicator.
#' @noRd
extend_to_full_analysis_timeframe <- function(data, start_analysis, end_analysis) {

  data <- data %>%
    tidyr::complete(
      production_year = seq(start_analysis, end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "asset_id", "company_id", "sector", "technology"
          )
        )
      )
    ) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::arrange(
      .data$production_year,
      .by_group = TRUE
    ) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "asset_name",
        "company_name",
        "country_iso2",
        "scenario_geography",
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility"
      )),
      .direction = "down"
    ) %>%
    tidyr::fill(
      c(
        "asset_name",
        "company_name",
        "country_iso2",
        "scenario_geography",
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility",
        "production_plan_company_technology"
      ),
      .direction = "up"
    ) %>%
    dplyr::ungroup()

  # Fill down production_plan_company_technology only up to the start_analysis year
  data_before_start_analysis <- data %>%
    dplyr::filter(.data$production_year <= start_analysis) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::arrange(.data$production_year) %>%
    tidyr::fill(.data$production_plan_company_technology, .direction = "down") %>%
    dplyr::ungroup()

  # Combine filled data before start_analysis with the rest of the data
  data <- data_before_start_analysis %>%
    dplyr::bind_rows(data %>% dplyr::filter(.data$production_year > start_analysis)) %>%
    dplyr::arrange(.data$asset_id, .data$company_id, .data$sector, .data$technology, .data$production_year)

  return(data)
}
