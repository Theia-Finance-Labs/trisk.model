
merge_assets_and_scenarios_data <- function(assets_data, scenarios_data) {

  assets_data_filtered <- assets_data %>%
    dplyr::inner_join(
      scenarios_data %>% dplyr::distinct(.data$technology)
      , by=c("technology"))

  stopifnot(nrow(assets_data_filtered) > 0)

  start_analysis <- min(scenarios_data$scenario_year)
  end_analysis <- min(max(scenarios_data$scenario_year), MAX_POSSIBLE_YEAR)

  assets_data_full <- assets_data_filtered %>%
      extend_to_full_analysis_timeframe(start_analysis=start_analysis, end_analysis=end_analysis) %>%
      fill_production_plan()
    
  # add extend production data with scenario targets
  assets_scenarios <- dplyr::inner_join(
    assets_data_full, scenarios_data,
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
#' 
extend_to_full_analysis_timeframe <- function(data, start_analysis, end_analysis) {
  data_extended <- data %>%
    dplyr::group_by(asset_id, company_id, sector, technology) %>%
    # Identify the first available year for each group
    dplyr::mutate(first_year = min(production_year, na.rm = TRUE)) %>%
    # Complete the data to include all years from the first year to end_analysis
    tidyr::complete(production_year = full_seq(c(first_year, end_analysis), 1)) %>%
    # Arrange the data by production year within each group
    dplyr::arrange(production_year) %>%
    # Fill down all values from the first available year, except production_plan_company_technology
    tidyr::fill(-production_plan_company_technology, .direction = "down") %>%
    # Ungroup to return a flat data structure
    dplyr::ungroup() %>%
    # Drop the helper columns
    dplyr::select(-first_year)

  return(data_extended)
}


# Fill down production_plan_company_technology up to the last non-na year (over all assets) of production_plan_company_technology
# This is so no matter when an asset production starts, all assets will get their productions extended by scenarios from the same year 
# Meaning scenario pathways are consistently applied on all assets even ig they have end of forecast years
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries of their forecasts, and the phase-out indicator.
#' 
fill_production_plan <- function(data) {
  # Determine the last year where production_plan_company_technology is available
  last_production_forecast_year <- data %>%
    dplyr::filter(!is.na(.data$production_plan_company_technology)) %>%
    dplyr::summarize(max_year = max(production_year, na.rm = TRUE)) %>%
    dplyr::pull(max_year)

  # Filter the data to include only years up to last_production_forecast_year and fill down
  data_before_start_analysis <- data %>%
    dplyr::filter(production_year <= last_production_forecast_year) %>%
    dplyr::group_by(asset_id, company_id, sector, technology) %>%
    tidyr::fill(.data$production_plan_company_technology, .direction = "down") %>%
    dplyr::ungroup()

  # Filter the data to include only years after last_production_forecast_year
  data_after_forecast <- data %>%
    dplyr::filter(production_year > last_production_forecast_year)

  # Combine the filled data with the data after the forecast
  data_filled <- bind_rows(data_before_start_analysis, data_after_forecast) %>%
    dplyr::arrange(asset_id, company_id, sector, technology, production_year)

  return(data_filled)
}
