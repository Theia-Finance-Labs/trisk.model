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

  scenarios_data  <- scenarios_data %>% 
    pivot_to_baseline_target_columns()

  return(scenarios_data)
}



pivot_to_baseline_target_columns <- function(data) {

  index_cols <- c("scenario_geography", "ald_sector", "ald_business_unit", "year")
  to_pivot <- c("fair_share_perc", "capacity_factor", "price")

  # Filter baseline scenario
  baseline_data <- data %>%
    dplyr::filter(.data$scenario_type == "baseline") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    )

  # Filter shock scenario
  shock_data <- data %>%
    dplyr::filter(.data$scenario_type == "shock") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    )

  # Merge the pivoted data
  pivoted_scenarios_data <- dplyr::inner_join(baseline_data, shock_data, by = index_cols)
  return(pivoted_scenarios_data)
}

