merge_assets_and_scenarios_data <- function(assets_data, scenarios_data) {
  assets_data_filtered <- filter_assets_on_scenario_perimeter(assets_data, scenarios_data)

  start_analysis <- min(scenarios_data$scenario_year)
  end_analysis <- min(max(scenarios_data$scenario_year), MAX_POSSIBLE_YEAR)

  assets_data_full <- assets_data_filtered %>%
    extend_to_full_analysis_timeframe(start_analysis = start_analysis, end_analysis = end_analysis)

  # add extend production data with scenario targets
  assets_scenarios <- dplyr::inner_join(
    assets_data_full, scenarios_data,
    by = c("sector", "technology", "production_year" = "scenario_year")
  ) %>%
    dplyr::rename(year = .data$production_year)

  return(assets_scenarios)
}


filter_assets_on_scenario_perimeter <- function(assets_data, scenarios_data) {
    technologies_filter <- scenarios_data %>% dplyr::distinct(.data$technology)

  assets_data_filtered <- assets_data %>%
    dplyr::inner_join(
      scenarios_data %>% dplyr::distinct(.data$technology),
      by = c("technology")
    )


  countries_filter <- scenarios_data %>% dplyr::distinct(.data$country_iso2_list) %>% dplyr::pull()
  if (!is.na(countries_filter)){
    countries_filter <- strsplit(countries_filter, ",")[[1]]
    assets_data_filtered <-  assets_data_filtered %>%
      dplyr::filter(.data$country_iso2 %in% countries_filter)
  }

  stopifnot(nrow(assets_data_filtered) > 0)

  return(assets_data_filtered)
}

#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries of their forecasts, and the phase-out indicator.
#' @param start_analysis Start of the analysis
#' @param end_analysis End of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,start_analysis,end_analysis) {
  
  # the first production year should start before the first scenario year
  stopifnot(min(data$production_year) <= start_analysis)

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

  # TODO CAREFULLY INTEGRATE
  # Fill down production_plan_company_technology only up to the start_analysis year
  # data_before_start_analysis <- data %>%
  #   filter(production_year <= start_analysis) %>%
  #   group_by(asset_id, company_id, sector, technology) %>%
  #   arrange(production_year) %>%
  #   tidyr::fill(production_plan_company_technology, .direction = "down") %>%
  #   ungroup()

  # # Combine filled data before start_analysis with the rest of the data
  # data <- data_before_start_analysis %>%
  #   bind_rows(filter(data, production_year > start_analysis)) %>%
  #   arrange(asset_id, company_id, sector, technology, production_year)

  return(data)
}
