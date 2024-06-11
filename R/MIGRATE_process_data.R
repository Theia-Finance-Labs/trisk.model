process_assets_data <- function(assets_data, start_analysis){
    assets_data <- assets_data %>%
        summarise_production_technology_forecasts(
        start_analysis = start_analysis
        ) %>%
        extend_to_full_analysis_timeframe(
        start_analysis = start_analysis,
        end_analysis = end_analysis
        ) 

    return(assets_data)

}

process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography){

  scenario_data <- data$scenario_data
  capacity_factors_power <- data$capacity_factors_power
  df_price <- data$df_price

  scenario_capfac <- dplyr::left_join(scenario_data, capacity_factors_power, by=c("scenario"  ,  "scenario_geography", "ald_business_unit" , "year"))
  scenario_capfac_price <- dplyr::left_join(scenario_capfac, df_price, by=c("year", "scenario" ,   "ald_sector" ,"ald_business_unit"))
browser()
  scenarios_data <- scenario_capfac_price %>%
    harmonise_cap_fac_geo_names() %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_analysis, .env$end_analysis)) %>%
    dplyr::group_by("scenario", "ald_sector" ,"ald_business_unit", "scenario_type") %>%
    dplyr::arrange(year, .by_group=TRUE) %>%
    dplyr::mutate(
      scenario_change = .data$fair_share_perc - dplyr::lag(.data$fair_share_perc)
    ) %>%
    dplyr::ungroup()

return(scenarios_data)
      
}

st_process <- function(data, scenario_geography, baseline_scenario,
                       target_scenario, start_year, carbon_price_model) {
                        

  # TODO remove MAX_POSSIBLE_YEAR 
  end_year <- get_end_year(data, c(baseline_scenario, shock_scenario), MAX_POSSIBLE_YEAR = 2050)

  start_year <- min(data$production_data$year)

  scenarios_data <- process_scenarios_data(data, baseline_scenario, target_scenario)
  assets_data <- process_assets_data(data, start_year)


  # add extend production data with scenario targets
  trisk_model_data <- production_data %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
      start_analysis = start_year,
      end_analysis = end_year,
      target_scenario = target_scenario,
      baseline_scenario=baseline_scenario
    )


  # sectors_and_technologies_list <- infer_sectors_and_technologies(
  #   price_data = data$df_price,
  #   scenario_data = data$scenario_data,
  #   production_data = data$production_data,
  #   baseline_scenario = baseline_scenario,
  #   shock_scenario = shock_scenario,
  #   scenario_geography = scenario_geography
  # )

  # df_price <- process_price_data(
  #   data$df_price,
  #   technologies = sectors_and_technologies_list$technologies,
  #   sectors = sectors_and_technologies_list$sectors,
  #   start_year = start_year,
  #   end_year = end_year,
  #   baseline_scenario = baseline_scenario,
  #   target_scenario = shock_scenario
  # )

  # scenario_data <- process_scenario_data(
  #   data$scenario_data,
  #   start_year = start_year,
  #   end_year = end_year,
  #   sectors = sectors_and_technologies_list$sectors,
  #   technologies = sectors_and_technologies_list$technologies,
  #   scenario_geography_filter = scenario_geography,
  #   scenarios_filter = scenarios_filter
  # )

  financial_data <- process_financial_data(
    data$financial_data
  )


  carbon_data <- process_carbon_data(
    data$carbon_data,
    start_year = start_year,
    end_year = end_year,
    carbon_price_model = carbon_price_model
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

  # # add extend production data with scenario targets
  # trisk_model_data <- production_data %>%
  #   extend_scenario_trajectory(
  #     scenario_data = scenario_data,
  #     start_analysis = start_year,
  #     end_analysis = end_year,
  #     target_scenario = shock_scenario,
  #     baseline_scenario=baseline_scenario
  #   )

  # # capacity_factors are only applied for power sector
  # if ("Power" %in% sectors_and_technologies_list$sectors) {
  #   capacity_factors_power <- process_capacity_factors_power(
  #     data$capacity_factors_power,
  #     scenarios_filter = scenarios_filter,
  #     scenario_geography_filter = scenario_geography,
  #     technologies = sectors_and_technologies_list$technologies,
  #     start_year = start_year,
  #     end_year = end_year
  #   )

  #   # convert power capacity to generation
  #   production_data <- convert_power_cap_to_generation(
  #     data = production_data,
  #     capacity_factors_power = capacity_factors_power,
  #     baseline_scenario = baseline_scenario,
  #     target_scenario = shock_scenario
  #   )
  # } else {
  #   capacity_factors_power <- data$capacity_factors_power
  # }
  
  # out <- list(
  #   capacity_factors_power = capacity_factors_power,
  #   df_price = df_price,
  #   scenario_data = scenario_data,
  #   financial_data = financial_data,
  #   production_data = production_data,
  #   carbon_data = carbon_data
  # )

  return(list(
    input_data_list = trisk_model_data,
    end_year = end_year
  ))
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
process_price_data <- function(data, technologies, sectors, start_year, end_year,
                               baseline_scenario, target_scenario) {
  data_processed <- data %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Price Data") %>%
    tidyr::pivot_wider(names_from = "scenario", values_from = "price", names_prefix = "price_") %>%
    dplyr::rename(
      price_baseline = !!rlang::sym(paste0("price_", baseline_scenario)),
      price_target = !!rlang::sym(paste0("price_", target_scenario))
    )

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_scenario_data <- function(data, start_year, end_year, sectors, technologies,
                                  scenario_geography_filter, scenarios_filter) {
  data_processed <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    stop_if_empty(data_name = "Scenario Data") %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Scenario Data")
  return(data_processed)
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

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#' @inheritParams run_trisk
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_financial_data <- function(data) {
  data_processed <- data %>%
    stop_if_empty(data_name = "Financial Data")

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

#' Process data of type indicated by function name
#'
#' @param data A tibble of data of type indicated by function name.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param scenario_geography_filter Character. A vector of length 1 that
#'   indicates which geographic scenario to apply in the analysis.
#' @param sectors Character vector, holding considered sectors.
#' @param technologies Character vector, holding considered technologies.
#'
#' @return A tibble of data as indicated by function name.
process_production_data <- function(data, start_year, end_year,
                                    scenario_geography_filter, sectors,
                                    technologies, TIME_HORIZON_LOOKUP) {
  data_processed <- data %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(.data$year < (min(.data$year) + TIME_HORIZON_LOOKUP)) %>%
    stop_if_empty(data_name = "Production Data")

  return(data_processed)
}
