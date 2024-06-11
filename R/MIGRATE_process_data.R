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
convert_power_cap_to_generation <- function(data,
                                            capacity_factors_power = NULL,
                                            baseline_scenario,
                                            target_scenario) {
  capacity_factors_power <- capacity_factors_power %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c("scenario_geography", "ald_business_unit", "year")),
      names_from = "scenario",
      names_prefix = "capfac_",
      values_from = "capacity_factor"
    )

  # ADO 1945 - Left join is applied since only rows in data from ald_sector
  # power will have matching rows in capacity_factors_power
  data <- data %>%
    dplyr::left_join(
      capacity_factors_power,
      by = c("ald_business_unit", "scenario_geography", "year")
    )

  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::mutate(
      # the planned generation is assumed to follow baseline
      plan_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$plan_tech_prod * !!rlang::sym(glue::glue("capfac_{baseline_scenario}")) * .env$hours_to_year,
        .data$plan_tech_prod
      ),
      !!rlang::sym(baseline_scenario) := dplyr::if_else(
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
    ) %>%
    dplyr::select(-dplyr::starts_with("capfac_"))
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
      price_baseline_scenario = !!rlang::sym(paste0("price_", baseline_scenario)),
      price_target_scenario = !!rlang::sym(paste0("price_", target_scenario))
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

st_process <- function(data, scenario_geography, baseline_scenario,
                       shock_scenario, start_year, carbon_price_model) {
  scenarios_filter <- c(baseline_scenario, shock_scenario)

  # TODO MAX_POSSIBLE_YEAR begone!
  end_year <- get_end_year(data, scenarios_filter, MAX_POSSIBLE_YEAR = 2050)

  start_year <- min(data$production_data$year)

  sectors_and_technologies_list <- infer_sectors_and_technologies(
    price_data = data$df_price,
    scenario_data = data$scenario_data,
    production_data = data$production_data,
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography
  )

  df_price <- process_price_data(
    data$df_price,
    technologies = sectors_and_technologies_list$technologies,
    sectors = sectors_and_technologies_list$sectors,
    start_year = start_year,
    end_year = end_year,
    baseline_scenario = baseline_scenario,
    target_scenario = shock_scenario
  )

  scenario_data <- process_scenario_data(
    data$scenario_data,
    start_year = start_year,
    end_year = end_year,
    sectors = sectors_and_technologies_list$sectors,
    technologies = sectors_and_technologies_list$technologies,
    scenario_geography_filter = scenario_geography,
    scenarios_filter = scenarios_filter
  )

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

  # add extend production data with scenario targets
  production_data <- production_data %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
      start_analysis = start_year,
      end_analysis = end_year,
      target_scenario = shock_scenario,
      baseline_scenario=baseline_scenario
    )

  # capacity_factors are only applied for power sector
  if ("Power" %in% sectors_and_technologies_list$sectors) {
    capacity_factors_power <- process_capacity_factors_power(
      data$capacity_factors_power,
      scenarios_filter = scenarios_filter,
      scenario_geography_filter = scenario_geography,
      technologies = sectors_and_technologies_list$technologies,
      start_year = start_year,
      end_year = end_year
    )

    # convert power capacity to generation
    production_data <- convert_power_cap_to_generation(
      data = production_data,
      capacity_factors_power = capacity_factors_power,
      baseline_scenario = baseline_scenario,
      target_scenario = shock_scenario
    )
  } else {
    capacity_factors_power <- data$capacity_factors_power
  }
  # browser()
  #   trisk_model_data <- dplyr::inner_join(
  #     capacity_factors_power,
  #     dplyr::inner_join(df_price,
  #     dplyr::inner_join(scenario_data,
  #     dplyr::inner_join(financial_data,
  #     dplyr::inner_join(production_data,carbon_data))))) %>%
  #     dplyr::mutate(
  #       target_scenario_change =
  #         dplyr::if_else(
  #           is.na(.data$plan_tech_prod),
  #           .data$target_scenario - dplyr::lag(.data$target_scenario),
  #           0
  #         ),
  #       baseline_scenario_change = dplyr::if_else(
  #         is.na(.data$plan_tech_prod),
  #         .data$baseline_scenario - dplyr::lag(.data$baseline_scenario),
  #         0
  #       )
  #     )

  out <- list(
    capacity_factors_power = capacity_factors_power,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data,
    production_data = production_data,
    carbon_data = carbon_data
  )

  return(list(
    input_data_list = out,
    end_year = end_year
  ))
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
