process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography, start_analysis, end_analysis) {
  scenario_data <- data$scenario_data
  capacity_factors_power <- data$capacity_factors_power
  df_price <- data$df_price

  scenario_capfac <- dplyr::left_join(scenario_data, capacity_factors_power, by = c("scenario", "scenario_geography", "ald_business_unit", "year"))
  scenario_capfac_price <- dplyr::left_join(scenario_capfac, df_price, by = c("year", "scenario", "ald_sector", "ald_business_unit"))

  scenarios_data <- scenario_capfac_price %>%
    harmonise_cap_fac_geo_names() %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$ scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_analysis, .env$end_analysis)) %>%
    dplyr::group_by(.data$scenario, .data$ald_sector, .data$ald_business_unit, .data$scenario_type) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      scenario_change = .data$fair_share_perc - dplyr::lag(.data$fair_share_perc)
    ) %>%
    tidyr::replace_na(list(scenario_change = 0, capacity_factor = 1)) %>%
    dplyr::ungroup()

  return(scenarios_data)
}
