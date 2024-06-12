process_trisk_input <- function(assets_data, scenarios_data,
                                target_scenario, start_analysis) {
                                  browser()
  # add extend production data with scenario targets
  trisk_model_input <- dplyr::inner_join(
    assets_data, scenarios_data,
    by = c("ald_sector", "ald_business_unit", "scenario_geography", "year")
  ) %>%
    create_base_production_trajectories() %>%
    pivot_to_baseline_target_columns() %>%
    lag_scenario_productions()

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
      scen_tech_prod = dplyr::if_else(
        .data$direction == "carbontech",
        .data$initial_technology_production * (1 + .data$fair_share_perc), # tmsr
        .data$initial_technology_production + (.data$initial_sector_production * .data$fair_share_perc) # smsp
      )
    ) %>%
    dplyr::mutate(
      scen_tech_prod = ifelse(.data$scen_tech_prod < 0, 0, .data$scen_tech_prod)
    )%>%
    # 2. Apply capacity factors
    dplyr::mutate(
      plan_tech_prod = .data$plan_tech_prod * .data$capacity_factor * .env$hours_to_year,
      scen_tech_prod = .data$scen_tech_prod * .data$capacity_factor * .env$hours_to_year
    )
}


#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries fo their forecasts and the phase out indicator.
#' @param start_analysis start of the analysis
#' @param end_analysis end of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,
                                              start_analysis,
                                              end_analysis) {
  data <- data %>%
    tidyr::complete(
      year = seq(.env$start_analysis, .env$end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"
          )
        )
      )
    ) %>%
    dplyr::arrange(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography, .data$year
    ) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "initial_technology_production",
        "final_technology_production",
        "plan_emission_factor"
      ))
    ) %>%
    dplyr::rename(
      emission_factor = "plan_emission_factor"
    )

  return(data)
}



lag_scenario_productions <- function(data) {
  data <- data%>%
    dplyr::mutate(
      baseline_scenario_change = .data$prod_baseline_scenario - dplyr::lag(.data$prod_baseline_scenario),
      target_scenario_change = .data$prod_target_scenario - dplyr::lag(.data$prod_target_scenario)
    ) %>%
    tidyr::replace_na(replace = list(baseline_scenario_change = 0, target_scenario_change = 0))

  return(data)
}