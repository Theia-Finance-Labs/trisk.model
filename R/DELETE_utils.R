
#' Infer supported sectors and technologies
#'
#' Function returns supported sectors and technologies for provided combination
#' of `baseline_scenario`, `shock_scenario` and `scenario_geography`.
#'
#' @inheritParams run_trisk
#'
#' @return A list with entries sectors and technologies
#' @noRd
infer_sectors_and_technologies <-
  function(price_data,
           scenario_data,
           production_data,
           baseline_scenario,
           shock_scenario,
           scenario_geography) {
    baseline_type <- scenario_data %>%
      dplyr::distinct(.data$scenario, .data$scenario_type) %>%
      dplyr::filter(
        .data$scenario == !!baseline_scenario & .data$scenario_type == "baseline"
      )

    if (nrow(baseline_type) == 0) {
      available_baselines <- scenario_data %>%
        dplyr::filter(.data$scenario_type == "baseline") %>%
        dplyr::distinct(.data$scenario) %>%
        dplyr::pull()

      rlang::abort(
        c(
          "The selected baseline scenario is not of a baseline type",
          x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
          i = glue::glue("Available baseline scenarios : {available_baselines}")
        )
      )
    }


    shock_type <- scenario_data %>%
      dplyr::distinct(.data$scenario, .data$scenario_type) %>%
      dplyr::filter(
        .data$scenario == !!shock_scenario & .data$scenario_type == "shock"
      )

    if (nrow(shock_type) == 0) {
      available_shocks <- scenario_data %>%
        dplyr::filter(.data$scenario_type == "shock") %>%
        dplyr::distinct(.data$scenario) %>%
        dplyr::pull()

      rlang::abort(
        c(
          "The selected shock scenario is not of a shock type",
          x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
          i = glue::glue("Available shock scenarios : {available_shocks}")
        )
      )
    }


    available_scenario_data <- scenario_data %>%
      dplyr::distinct(.data$scenario, .data$ald_sector, .data$ald_business_unit, .data$scenario_geography) %>%
      dplyr::filter(.data$scenario %in% c(baseline_scenario, shock_scenario))

    available_scenario_geography_data <- available_scenario_data %>%
      dplyr::filter(.data$scenario_geography == .env$scenario_geography)

    if ((nrow(available_scenario_data) > 0) & (nrow(available_scenario_geography_data) == 0)) {
      rlang::abort(
        c(
          "Could not find scenario data matching the provided geography.",
          x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
          i = "Please use function geographies_for_sector to identify a valid geography for those scenarios."
        )
      )
    }

    available_price_data <- price_data %>%
      dplyr::distinct(.data$scenario, .data$ald_sector, .data$ald_business_unit) %>%
      dplyr::filter(.data$scenario %in% c(baseline_scenario, shock_scenario))


    scenario_geography_x_ald_sector <- dplyr::inner_join(available_scenario_geography_data, available_price_data)

    if (nrow(scenario_geography_x_ald_sector) != nrow(available_scenario_geography_data) |
      nrow(scenario_geography_x_ald_sector) != nrow(available_price_data)) {
      anti_joined_rows <- dplyr::bind_rows(
        dplyr::anti_join(available_scenario_geography_data, available_price_data),
        dplyr::anti_join(available_price_data, available_scenario_geography_data)
      )
      # Convert the dataframe to a string
      rows_as_string <- apply(anti_joined_rows, 1, function(x) paste(x, collapse = ", "))
      formatted_rows <- paste(rows_as_string, collapse = "\n")


      rlang::abort(
        c(
          "Could not match all the data points between price and scenario datasets.",
          x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
          i = glue::glue("Please solve the mismatch of datapoints that appear on the given perimeter, between price_data_long.csv and Scenarios_AnalysisInput.csv . \n Mismatching rows : \n {formatted_rows}")
        )
      )
    }

    available_production_data <- production_data %>%
      dplyr::distinct(.data$ald_sector, .data$ald_business_unit)

    scenario_geography_x_ald_sector <- dplyr::inner_join(scenario_geography_x_ald_sector, available_production_data)

    sectors_baseline <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!baseline_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_sector)

    sectors_shock <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!shock_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_sector)

    shared_sectors <- dplyr::intersect(sectors_baseline, sectors_shock)

    if (length(shared_sectors) == 0) {
      rlang::abort(
        c(
          "Could not find sectors that are supported for baseline and shock scenario for selected scenario_geography.",
          x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
          i = "Please use function scenario_for_sector_x_geography to identify a valid combination."
        )
      )
    }

    technologies_baseline <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!baseline_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_business_unit)

    technologies_shock <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!shock_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_business_unit)

    technologies <- dplyr::intersect(technologies_baseline, technologies_shock)
    technologies_baseline <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!baseline_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_business_unit)

    technologies_shock <- scenario_geography_x_ald_sector %>%
      dplyr::filter(.data$scenario == !!shock_scenario & .data$scenario_geography == !!scenario_geography) %>%
      dplyr::pull(.data$ald_business_unit)

    technologies <- dplyr::intersect(technologies_baseline, technologies_shock)

    return(list(sectors = shared_sectors, technologies = technologies))
  }


stop_if_empty <- function(data, data_name) {
  if (nrow(data) == 0) {
    rlang::abort(glue::glue("Stopping calculation, dataset {data_name} is empty."))
  }
  return(invisible(data))
}

