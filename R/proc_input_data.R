process_scenarios_data <- function(scenarios_data, baseline_scenario, target_scenario, scenario_geography) {
  scenarios_data <- scenarios_data %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography) %>%
    dplyr::arrange(.data$scenario_year, .by_group = TRUE) %>%
    add_technology_fair_share_ratio() %>%
    calculate_fair_share_perc()


  return(scenarios_data)
}

process_assets_data <- function(assets_data, financial_data) {
  production_financial_data <- dplyr::inner_join(
    assets_data,
    financial_data,
    by = "company_id"
  ) %>%
    dplyr::mutate(
      production_plan_company_technology = .data$capacity * .data$capacity_factor
    ) %>%
    dplyr::select(-c(.data$capacity, .data$capacity_factor))
  assets_data <- production_financial_data
  return(assets_data)
}
