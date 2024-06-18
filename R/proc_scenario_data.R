process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography, start_analysis, end_analysis) {
  scenarios_pathways <- data$scenario_data
  capacity_factors_power <- data$capacity_factors_power %>% harmonise_cap_fac_geo_names()
  df_price <- data$df_price

  scenarios_data <- scenarios_pathways %>%
    dplyr::left_join(capacity_factors_power, by = c("scenario", "scenario_geography", "ald_business_unit", "year")) %>%
    dplyr::left_join(df_price, by = c("year", "scenario", "ald_sector", "ald_business_unit"))

  scenarios_data <- scenarios_data %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$ scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_analysis, .env$end_analysis)) %>%
    tidyr::replace_na(list(capacity_factor = 1)) %>%
    dplyr::arrange(year, .by_group = TRUE)

  return(scenarios_data)
}

harmonise_cap_fac_geo_names <- function(data) {
  data <- data %>%
    # hardcoded adjustments are needed here for compatibility with P4I
    dplyr::mutate(scenario_geography = gsub(" ", "", .data$scenario_geography, fixed = TRUE)) %>%
    dplyr::mutate(scenario_geography = dplyr::case_when(
      .data$scenario_geography == "EuropeanUnion" ~ "EU",
      .data$scenario_geography == "Non-OECD" ~ "NonOECD",
      .data$scenario_geography == "UnitedStates" ~ "US",
      TRUE ~ .data$scenario_geography
    ))
  return(data)
}
