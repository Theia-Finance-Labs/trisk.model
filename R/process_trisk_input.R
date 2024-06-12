process_trisk_input <- function(assets_data, scenarios_data,
                                target_scenario, start_analysis) {
                                  
  # add extend production data with scenario targets
  assets_scenario_productions <- dplyr::inner_join(
    assets_data, scenarios_data,
    by = c("ald_sector", "ald_business_unit", "scenario_geography", "year")
  ) %>%
    create_base_production_trajectories() %>%
    calculate_proximity_to_target( # TODO MOVE TO NPV COMPUTATION ?
      start_analysis = start_analysis,
      target_scenario = target_scenario
    ) %>%
    pivot_to_baseline_target_columns() %>%
    lag_scenario_productions()
  

  trisk_model_input <- dplyr::inner_join(
    assets_data, assets_scenario_productions,
    by = c("company_id","ald_sector", "ald_business_unit", "scenario_geography", "year")
  )

  return(trisk_model_input)
}



#' Transform assets capacities into yearly real and theoretical (baseline and target) productions
#' 
#' 1. Apply TMSR/SMSP scenario targets based on initial ald_business_unit or sector
#' production and type of ald_business_unit
#'
#' 2. Translate power capacity to power generation
#'
#' Units of generated power are assumed to be sold and hence get priced in the
#' net profit calculations. This also entails converting MWh into MW per year,
#' since we calculate yearly profits. Note: For use in webscripts
#' [convert_cap_to_generation()] is used currently, which only distinguishes
#' capacity factor by ald_business_unit and scenario_geography, whereas this function
#' distinguishes further by year and scenario. Also note that for generation of
#' variable `plan_tech_prod` (planned capacity) capacity factors from baseline
#' scenario are used.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
create_base_production_trajectories <- function(data){
  
  hours_to_year <- 24 * 365
  data <- data %>%
    # 1. Apply tmsr / smsp
    dplyr::mutate(
      production_scenario = dplyr::if_else(
        .data$direction == "carbontech",
        .data$initial_technology_production * (1 + .data$fair_share_perc), # tmsr
        .data$initial_technology_production + (.data$initial_sector_production * .data$fair_share_perc) # smsp
      )
    ) %>%
    dplyr::mutate(
      production_scenario = ifelse(.data$production_scenario < 0, 0, .data$production_scenario)
    )%>%
    # 2. Apply capacity factors
    dplyr::mutate(
      plan_tech_prod = .data$plan_tech_prod * .data$capacity_factor * .env$hours_to_year,
      production_scenario = .data$production_scenario * .data$capacity_factor * .env$hours_to_year
    )
}




pivot_to_baseline_target_columns <- function(data) {

  index_cols <- c("company_id" ,"scenario_geography", "ald_sector", "ald_business_unit", "year")
  to_pivot <- c("production_scenario", "price")

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
  data <- dplyr::inner_join(baseline_data, shock_data, by = index_cols)
  return(data)
}




lag_scenario_productions <- function(data) {
  
  data <- data%>%
    dplyr::mutate(
      production_change_scenario_baseline = .data$production_scenario_baseline - dplyr::lag(.data$production_scenario_baseline),
      production_change_scenario_target = .data$production_scenario_shock - dplyr::lag(.data$production_scenario_shock)
    ) %>%
    tidyr::replace_na(replace = list(production_change_scenario_baseline = 0, production_change_scenario_target = 0))

  return(data)
}