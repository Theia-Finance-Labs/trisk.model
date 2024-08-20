
merge_assets_and_scenarios_data <- function(assets_data, scenarios_data) {

  start_analysis <- min(scenarios_data$scenario_year)
  end_analysis <- min(max(scenarios_data$scenario_year), MAX_POSSIBLE_YEAR)

  assets_data <- assets_data %>%
      extend_to_full_analysis_timeframe(end_analysis=end_analysis, start_analysis=start_analysis) 
    
  # add extend production data with scenario targets
  assets_scenarios <- dplyr::inner_join(
    assets_data, scenarios_data,
    by = c("sector", "technology", "scenario_geography", "production_year" = "scenario_year")
  ) %>%
    dplyr::rename(year = .data$production_year)
browser()
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
    dplyr::group_by(asset_id, company_id, sector, technology) %>%
    # Step 1: Identify the first and last available years for each group
    dplyr::mutate(first_year = min(production_year, na.rm = TRUE)) %>%
    # Step 3: Arrange the data by production year within each group
    dplyr::arrange(asset_id, production_year, .by_group = TRUE) %>%
    # Step 2: Complete the data to include all years from the first year to end_analysis
    tidyr::complete(production_year = full_seq(c(first_year, end_analysis), 1)) %>%
    # Step 4: Fill down all values from the first available year, except production_plan_company_technology
    tidyr::fill(-production_plan_company_technology, .direction = "down") %>%
    # Ungroup to return a flat data structure
    dplyr::ungroup() %>%
    # Drop the helper columns
    dplyr::select(-first_year)

  # Fill down production_plan_company_technology up to the last non-na year (over all assets) of production_plan_company_technology
  # This is so no matter when an asset production starts, all assets will get their productions extended by scenarios from the same year 
  # Meaning scenario pathways are consistently applied on all assets even ig they have end of forecast years
  # Step 1: Compute start_analysis
  last_production_forecast_year <- data %>%
    dplyr::filter(!is.na(production_plan_company_technology)) %>%
    dplyr::summarize(max_year = max(production_year, na.rm = TRUE)) %>%
    dplyr::pull(max_year)

  # Step 2: Filter the data to include only years up to last_production_forecast_year and fill down
  data_before_start_analysis <- data %>%
    dplyr::filter(.data$production_year <= last_production_forecast_year) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::arrange(.data$production_year) %>%
    tidyr::fill(.data$production_plan_company_technology, .direction = "down") %>%
    dplyr::ungroup()

  # Step 3: Combine filled data with the rest of the data
  data <- data_before_start_analysis %>%
    dplyr::bind_rows(data %>% dplyr::filter(.data$production_year > last_production_forecast_year)) %>%
    dplyr::arrange(.data$asset_id, .data$company_id, .data$sector, .data$technology, .data$production_year)

  return(data)
}
