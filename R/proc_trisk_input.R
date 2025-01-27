process_trisk_input <- function(assets_scenarios,
                                target_scenario) {
  outputs <- create_base_production_trajectories(data = assets_scenarios)
  assets_scenarios_productions <- outputs$data
  proximity_to_target_df <- outputs$proximity_to_target
  assets_scenarios_production_lagged <- lag_scenario_productions(data = assets_scenarios_productions)
  assets_scenarios_production_pivoted <- pivot_to_baseline_target_columns(data = assets_scenarios_production_lagged)

  trisk_model_input <- assets_scenarios %>%
    dplyr::distinct(
      .data$asset_name,
      .data$company_name,
      .data$asset_id,
      .data$company_id,
      .data$sector,
      .data$technology,
      .data$technology_type,
      .data$country_iso2,
      .data$debt_equity_ratio,
      .data$net_profit_margin,
      .data$pd,
      .data$scenario_geography,
      .data$year,
      .data$emission_factor,
      .data$volatility
    ) %>%
    dplyr::inner_join(
      assets_scenarios_production_pivoted,
      by = c("asset_id", "company_id", "sector", "technology", "year")
    )

  return(list("trisk_model_input" = trisk_model_input, "proximity_to_target_df" = proximity_to_target_df))
}



#' Transform assets capacities into yearly real and theoretical (baseline and target) productions
#'
#' 1. Apply TMSR/SMSP scenario targets based on initial technology or sector
#' production and type of technology
#'
#' 2. Translate power capacity to power generation
#'
#' Units of generated power are assumed to be sold and hence get priced in the
#' net profit calculations. This also entails converting MWh into MW per year,
#' since we calculate yearly profits. Note: For use in webscripts
#' [convert_cap_to_generation()] is used currently, which only distinguishes
#' capacity factor by technology and scenario_geography, whereas this function
#' distinguishes further by year and scenario. Also note that for generation of
#' variable `production_plan_company_technology` (planned capacity) capacity factors from baseline
#' scenario are used.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
create_base_production_trajectories <- function(data) {
  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::group_by(
      .data$asset_id, .data$company_id, .data$sector, .data$technology
    ) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(
      # Initial value is identical between production and scenario target,
      # can thus be used for both
      initial_technology_production = dplyr::first(.data$production_plan_company_technology[!is.na(.data$production_plan_company_technology)]),
      final_technology_production = dplyr::last(.data$production_plan_company_technology[!is.na(.data$production_plan_company_technology)])
    ) %>%
    # 1. Apply tmsr / smsp
    dplyr::mutate(
      production_scenario = .data$initial_technology_production * (1 + .data$fair_share_perc)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      production_scenario = ifelse(.data$production_scenario < 0, 0, .data$production_scenario)
    )

  proximity_to_target <- calculate_proximity_to_target(data)

  data <- data %>%
    # 2. Apply capacity factors
    dplyr::mutate(
      production_plan_company_technology = ifelse(.data$sector == "Power",
        .data$production_plan_company_technology * .data$scenario_capacity_factor * .env$hours_to_year,
        .data$production_plan_company_technology * .data$scenario_capacity_factor
      ),
      production_scenario = ifelse(.data$sector == "Power",
        .data$production_scenario * .data$scenario_capacity_factor * .env$hours_to_year,
        .data$production_scenario * .data$scenario_capacity_factor
      )
    )

  return(list("data" = data, "proximity_to_target" = proximity_to_target))
}




#' Apply a lag on scenarios matched to the companies.
#' Later used to compute the shock trajectory.
#' @param data A data frame containing the production forecasts of companies
#'   merged with their respective scenario pthways.
#' @noRd
lag_scenario_productions <- function(data) {
  data <- data %>%
    # 3. compute scenario changes
    dplyr::group_by(.data$scenario_type) %>%
    dplyr::arrange(.data$company_id, .data$asset_id, .data$sector, .data$technology, .data$year, .by_group = TRUE) %>%
    dplyr::mutate(
      production_change_scenario = .data$production_scenario - dplyr::lag(.data$production_scenario, default = NA)
    ) %>%
    # 4. set assets scenario production to NA when real asset production is known
    dplyr::mutate(
      production_change_scenario = ifelse(
        !is.na(.data$production_plan_company_technology) | (.data$year == min(.data$year)), NA, .data$production_change_scenario
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::replace_na(replace = list(production_change_scenario = 0))

  return(data)
}


pivot_to_baseline_target_columns <- function(data) {
  index_cols <- c("asset_id", "company_id", "sector", "technology", "year")
  to_pivot <- c("production_scenario", "scenario_price", "production_plan_company_technology", "production_change_scenario")

  # Filter baseline scenario
  baseline_data <- data %>%
    dplyr::filter(.data$scenario_type == "baseline") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = .data$scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    ) %>%
    dplyr::rename(
      production_plan_company_technology = .data$production_plan_company_technology_baseline
    )

  # Filter target scenario
  target_data <- data %>%
    dplyr::filter(.data$scenario_type == "target") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = .data$scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    ) %>%
    dplyr::select(
      -c(.data$production_plan_company_technology_target)
    )

  # Merge the pivoted data
  data <- dplyr::inner_join(baseline_data, target_data, by = index_cols)

  return(data)
}
