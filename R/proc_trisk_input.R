process_trisk_input <- function(assets_data, scenarios_data,
                                target_scenario, start_analysis) {
  # add extend production data with scenario targets
  assets_scenarios <- dplyr::inner_join(
    assets_data, scenarios_data,
    by = c("ald_sector", "ald_business_unit", "scenario_geography", "year")
  ) 
  
  assets_scenarios_productions <- create_base_production_trajectories(data=assets_scenarios)
  
  assets_scenarios_production_lagged <- lag_scenario_productions(data=assets_scenarios_productions)
  assets_scenarios_production_pivoted <- pivot_to_baseline_target_columns(data=assets_scenarios_production_lagged) 
  
  assets_proximity_to_target <- calculate_proximity_to_target( # TODO MOVE TO NPV COMPUTATION ?
      data=assets_scenarios_productions,
      start_analysis = start_analysis,
      target_scenario = target_scenario
    ) %>% dplyr::distinct(scenario_geography, company_name, company_id, ald_sector, ald_business_unit, year, emission_factor, pd, net_profit_margin, debt_equity_ratio, volatility, direction, proximity_to_target)


  trisk_model_input <- dplyr::inner_join(
    assets_proximity_to_target, assets_scenarios_production_pivoted,
    by = c("company_id", "ald_sector", "ald_business_unit", "scenario_geography", "year")
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
      # 2. Apply capacity factors
    dplyr::mutate(
      production_plan_company_technology = ifelse(.data$ald_sector == "Power", 
        .data$production_plan_company_technology * .data$capacity_factor * .env$hours_to_year,
        .data$production_plan_company_technology * .data$capacity_factor),
      plan_sec_prod = ifelse(.data$ald_sector == "Power", 
        .data$plan_sec_prod * .data$capacity_factor * .env$hours_to_year,
        .data$plan_sec_prod * .data$capacity_factor)) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      # Initial value is identical between production and scenario target,
      # can thus be used for both
      initial_technology_production = dplyr::first(.data$production_plan_company_technology[!is.na(.data$production_plan_company_technology)]),
      final_technology_production = dplyr::last(.data$production_plan_company_technology[!is.na(.data$production_plan_company_technology)]),
      initial_sector_production = dplyr::first(.data$plan_sec_prod[!is.na(.data$plan_sec_prod)]),
    )%>%
    # 1. Apply tmsr / smsp
    dplyr::mutate(
      production_scenario = dplyr::if_else(
        .data$direction == "carbontech",
        .data$initial_technology_production * (1 + .data$fair_share_perc), # tmsr
        .data$initial_technology_production + (.data$initial_sector_production * .data$fair_share_perc) # smsp
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      production_scenario = ifelse(.data$production_scenario < 0, 0, .data$production_scenario)
    ) 
    # %>%
    # # 2. Apply capacity factors
    # dplyr::mutate(
    #   production_scenario = ifelse(.data$ald_sector == "Power", 
    #     .data$production_scenario * .data$capacity_factor * .env$hours_to_year,
    #     .data$production_scenario * .data$capacity_factor)
    # ) 
    
    return(data)
}





lag_scenario_productions <- function(data) {
  
  data <- data %>%
    # 3. compute scenario changes
    dplyr::group_by(scenario_type) %>%
    dplyr::arrange(company_id, ald_sector, ald_business_unit, scenario_geography, year, .by_group=TRUE) %>%
    dplyr::mutate(
      production_change_scenario = .data$production_scenario - dplyr::lag(.data$production_scenario, default = NA)
    ) %>%
    # 4. set assets scenario production to NA when real asset production is known
    dplyr::mutate(
      production_change_scenario = ifelse(
        !is.na(.data$production_plan_company_technology) | (.data$year == min(.data$year)), NA, production_change_scenario
      )
    )%>%
    dplyr::ungroup()%>%
    tidyr::replace_na(replace = list(production_change_scenario = 0))

  return(data)
}



#' Calculate the ratio of the required change in ald_business_unit that each company
#' has achieved per ald_business_unit at the end of the production forecast period.
#' This ratio will later serve to adjust the net profit margin for companies
#' that have not built out enough production capacity in increasing technologies
#' and hence need to scale up production to compensate for their lag in buildout.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param start_analysis Numeric. A vector of length 1 indicating the start
#'   year of the analysis.
#' @param target_scenario Character. A vector of length 1 indicating target
#'   scenario
#'
#' @noRd
calculate_proximity_to_target <- function(data,
                                          start_analysis = 2022,
                                          target_scenario) {
  time_frame <- 5
  production_changes <- data %>%
    dplyr::filter(
      dplyr::between(
        .data$year, .env$start_analysis, .env$start_analysis + .env$time_frame
      ),
      .data$scenario == .env$target_scenario
    ) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      required_change = .data$production_scenario - .data$initial_technology_production,
      realised_change = .data$production_plan_company_technology - .data$initial_technology_production
    ) %>%
    dplyr::summarise(
      sum_required_change = sum(.data$required_change, na.rm = TRUE),
      sum_realised_change = sum(.data$realised_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ratio_realised_required = .data$sum_realised_change / .data$sum_required_change,
      proximity_to_target = dplyr::case_when(
        .data$ratio_realised_required < 0 ~ 0,
        .data$ratio_realised_required > 1 ~ 1,
        TRUE ~ .data$ratio_realised_required
      )
    ) %>%
    dplyr::select(
      -dplyr::all_of(c(
        "sum_required_change", "sum_realised_change",
        "ratio_realised_required"
      ))
    )

  data <- data %>%
    dplyr::inner_join(
      production_changes,
      by = c(
        "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"
      )
    )
}




pivot_to_baseline_target_columns <- function(data) {
  index_cols <- c("company_id", "scenario_geography", "ald_sector", "ald_business_unit", "year")
  to_pivot <- c("production_scenario", "production_change_scenario", "price", "production_plan_company_technology")

  # Filter baseline scenario
  baseline_data <- data %>%
    dplyr::filter(.data$scenario_type == "baseline") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    ) %>% dplyr::rename(
      production_plan_company_technology = .data$production_plan_company_technology_baseline
    )

  # Filter target scenario
  target_data <- data %>%
    dplyr::filter(.data$scenario_type == "target") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(index_cols),
      names_from = scenario_type,
      values_from = to_pivot,
      names_sep = "_"
    )%>% dplyr::select(
      -c(.data$production_plan_company_technology_target)
    )
  
  # Merge the pivoted data
  data <- dplyr::inner_join(baseline_data, target_data, by = index_cols)
  return(data)
}





# #' Summarise the forecasts for company-tech level production within the five
# #' year time frame
# #'
# #' @param data A data frame containing the production forecasts of companies
# #'   (in the portfolio). Pre-processed to fit analysis parameters and after
# #'   conversion of power capacity to generation.
# #' @param start_analysis start of the analysis
# #' @noRd
# summarise_production_technology_forecasts <- function(data,
#                                                       start_analysis) {
#   time_frame <- 5
#   data <- data %>%
#     dplyr::select(
#       dplyr::all_of(c(
#         "company_id", "company_name", "ald_sector", "ald_business_unit",
#         "scenario_geography", "year", "production_plan_company_technology", "plan_sec_prod",
#         "plan_emission_factor"
#       ))
#     ) %>%
#     dplyr::filter(.data$year <= .env$start_analysis + .env$time_frame) %>%
#     dplyr::arrange(.data$year) %>%
#      %>%
#     dplyr::ungroup()

#   return(data)
# }

# #' Summarise the forecasts for company-sector level production within the five
# #' year time frame
# #'
# #' @param data A data frame containing the production forecasts of companies
# #'   (in the portfolio). Pre-processed to fit analysis parameters and after
# #'   conversion of power capacity to generation.
# #' @noRd
# summarise_production_sector_forecasts <- function(data) {
#   data <- data %>%
#     # dplyr::select(.data$company_id, .data$company_name, .data$ald_sector,
#     #               .data$initial_technology_production,
#     #               .data$ald_business_unit, .data$emission_factor,
#     #              .data$scenario_geography, .data$year, .data$plan_sec_prod, .data$production_plan_company_technology) %>%
#     # dplyr::group_by(
#     #   .data$company_id, .data$company_name, .data$ald_sector, .data$scenario,
#     #   .data$scenario_geography, .data$year
#     # ) %>%
#     # dplyr::mutate(plan_sec_prod = sum(.data$production_plan_company_technology, na.rm = TRUE)) %>%
#     # tidyr::fill("plan_sec_prod", .direction="down") %>%
#     # dplyr::ungroup() %>%
#     dplyr::group_by(
#       .data$company_id, .data$company_name, .data$ald_sector, .data$scenario_geography
#     ) %>%
#     dplyr::arrange(.data$year, by_group = TRUE) %>%
#     dplyr::mutate(
#       # first year plan and scenario values are equal by construction,
#       # can thus be used for production and target
#       initial_sector_production = dplyr::first(.data$plan_sec_prod)
#     ) %>%
#     dplyr::ungroup()
# }
