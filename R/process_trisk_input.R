process_trisk_input <- function(assets_data, scenarios_data,
                                target_scenario, start_analysis) {
  # add extend production data with scenario targets
  trisk_model_data <- dplyr::inner_join(
    assets_data, scenario_data,
    by = c("ald_sector", "ald_business_unit", "scenario_geography", "year")
  ) %>%
    convert_fair_share_perc_to_production() %>%
    convert_power_cap_to_generation() %>%
    lag_scenario_productions() %>%
    pivot_to_baseline_target_columns()

  # TODO MOVE TO NPV COMPUTATION
  trisk_model_data <- trisk_model_data %>%
    calculate_proximity_to_target(
      start_analysis = start_analysis,
      target_scenario = target_scenario
    )



  production_data <- process_production_data(
    data$production_data,
    start_year = start_year,
    end_year = end_year,
    scenario_geography_filter = scenario_geography,
    sectors = sectors_and_technologies_list$sectors,
    technologies = sectors_and_technologies_list$technologies,
    TIME_HORIZON_LOOKUP = 6 # TODO TIME_HORIZON_LOOKUP begone!
  )

  return(list(
    trisk_model_data = trisk_model_data,
    end_year = end_year
  ))
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

#' Apply TMSR/SMSP scenario targets based on initial ald_business_unit or sector
#' production and type of ald_business_unit
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
convert_fair_share_perc_to_production <- function(data) {
  data <- data %>%
    dplyr::mutate(
      scen_tech_prod = dplyr::if_else(
        .data$direction == "carbontech",
        .data$initial_technology_production * (1 + .data$fair_share_perc), # tmsr
        .data$initial_technology_production + (.data$initial_sector_production * .data$fair_share_perc) # smsp
      )
    ) %>%
    dplyr::mutate(
      scen_tech_prod = ifelse(.data$scen_tech_prod < 0, 0, .data$scen_tech_prod)
    )

  return(data)
}





#' Translate power capacity to power generation
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
#' @param data A data frame filtered and wrangled company level production
#'   forecasts (of the companies in the portfolio). Usually based on PACTA
#'   output.
#' @param capacity_factors_power A data frame containing capacity factors to
#'   translate company level power capacity to units sold. Contains information
#'   on the ald_business_unit (power sector only) and scenario_geography levels.
#' @param baseline_scenario String holding name of baseline scenario.
#' @param target_scenario String holding name of target scenario.
convert_power_cap_to_generation <- function(data) {
  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::mutate(
      # the planned generation is assumed to follow baseline
      plan_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$plan_tech_prod * !!rlang::sym(glue::glue("capfac_{baseline_scenario}")) * .env$hours_to_year,
        .data$plan_tech_prod
      ),
      baseline_scenario := dplyr::if_else(
        .data$ald_sector == "Power",
        !!rlang::sym(baseline_scenario) * !!rlang::sym(glue::glue("capfac_{baseline_scenario}")) * .env$hours_to_year,
        !!rlang::sym(baseline_scenario)
      ),
      !!rlang::sym(target_scenario) := dplyr::if_else(
        .data$ald_sector == "Power",
        !!rlang::sym(target_scenario) * !!rlang::sym(glue::glue("capfac_{target_scenario}")) * .env$hours_to_year,
        !!rlang::sym(target_scenario)
      ),
      scenario_geography = .data$scenario_geography
    )

  return(data)
}


lag_scenario_productions <- function(trisk_model_data) {
  trisk_model_data <- trisk_model_data %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(
        c("year", "scenario_geography", "ald_sector", "ald_business_unit", "direction")
      ),
      names_from = "scenario",
      values_from = "scen_tech_prod"
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$ald_sector,
      .data$ald_business_unit, .data$year
    ) %>%
    dplyr::rename(
      prod_baseline_scenario = .env$baseline_scenario,
      prod_target_scenario = .env$target_scenario
    ) %>%
    dplyr::mutate(
      baseline_scenario_change = .data$prod_baseline_scenario - dplyr::lag(.data$prod_baseline_scenario),
      target_scenario_change = .data$prod_target_scenario - dplyr::lag(.data$prod_target_scenario)
    ) %>%
    tidyr::replace_na(replace = list(baseline_scenario_change = 0, target_scenario_change = 0))

  return(trisk_model_data)
}

pivot_to_baseline_target_columns <- function(trisk_model_data) {
  trisk_model_data <- trisk_model_data %>%
    tidyr::pivot_wider(id_cols = c("scenario_geography", "ald_sector", "ald_business_unit", "units", "year", "direction"), names_from = "scenario_type", values_from = c("price", "fair_share_perc"))
  return(trisk_model_data)
}






















#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_capacity_factors_power <- function(data,
                                           scenarios_filter,
                                           scenario_geography_filter,
                                           technologies,
                                           start_year,
                                           end_year) {
  data_processed <- data %>%
    harmonise_cap_fac_geo_names() %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Capacity Factors")

  return(data_processed)
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


#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_carbon_data <- function(data, start_year, end_year, carbon_price_model) {
  data_processed <- data

  ## dataframe will be NULL for lrisk this is the case as lrisk does not read in and use carbon prices
  if (is.null(data_processed)) {
    data_processed <- NULL
  } else {
    data_processed <- data_processed %>%
      dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
      dplyr::select(-c(scenario_geography)) %>%
      dplyr::filter(.data$scenario %in% .env$carbon_price_model) %>%
      stop_if_empty(data_name = "Carbon Data")
  }

  return(data_processed)
}


#' Get End year from data
#'
#' @param data data
#' @param scenarios_filter scenarios to use
#'
#' @return the end year
get_end_year <- function(data, scenarios_filter, MAX_POSSIBLE_YEAR = 2050) {
  available_min_of_max_years <- dplyr::bind_rows(
    data$df_price %>%
      dplyr::distinct(.data$year, .data$scenario) %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::summarise(year = max(.data$year)),
    data$capacity_factors_power %>%
      dplyr::distinct(.data$year, .data$scenario) %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::summarise(year = max(.data$year)),
    data$scenario_data %>%
      dplyr::distinct(.data$year, .data$scenario) %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::summarise(year = max(.data$year))
  ) %>%
    dplyr::group_by(.data$scenario) %>%
    dplyr::summarise(year = min(.data$year)) %>%
    dplyr::filter(.data$scenario %in% scenarios_filter) %>%
    dplyr::pull(.data$year)

  end_year <- min(MAX_POSSIBLE_YEAR, min(available_min_of_max_years))

  return(end_year)
}
