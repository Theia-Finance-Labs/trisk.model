st_read_agnostic <- function(
    dir,
    capacity_factor_file = "prewrangled_capacity_factors.csv",
    price_data_file = "price_data_long.csv",
    scenario_data_file = "Scenarios_AnalysisInput.csv",
    financial_data_file = "prewrangled_financial_data_stress_test.csv",
    production_data_file = "abcd_stress_test_input.csv",
    carbon_price_data_file = "ngfs_carbon_price.csv") {
  out <- list(
    capacity_factors_power = read_capacity_factors_power(file.path(dir, capacity_factor_file)),
    df_price = read_price_data(file.path(dir, price_data_file)),
    scenario_data = read_scenario_data(file.path(dir, scenario_data_file)),
    financial_data = read_financial_data(file.path(dir, financial_data_file)),
    production_data = read_production_data(file.path(dir, production_data_file)),
    carbon_data = read_carbon_data(file.path(dir, carbon_price_data_file))
  )
  return(out)
}


#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors.
#' @family import functions
read_capacity_factors_power <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(col_types = readr::cols())

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario", "scenario_geography", "ald_business_unit", "year", "capacity_factor"
    )
  )

  return(data)
}

#' Read in carbon price data from ngfs data
#'
#'
#' @param path A string that points to the location of the file containing the
#'   carbon price data.
#'
#' @family import functions
read_carbon_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(file.path(path)) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        year = "d",
        model = "c",
        scenario = "c",
        scenario_geography = "c",
        variable = "c",
        unit = "c",
        carbon_tax = "d"
      )
    )

  return(data)
}

#' Read in company financial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#'
#' @family import functions
read_financial_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(file.path(path)) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        company_id = "d",
        pd = "d",
        net_profit_margin = "d",
        debt_equity_ratio = "d",
        volatility = "d"
      )
    )

  check_valid_financial_data_values(data) # TODO move to DBT
  return(data)
}


#' Check if values in financial data are plausible
#'
#' Checks that numeric columns hold values in acceptable ranges.
#'
#'
#' @return NULL
check_valid_financial_data_values <- function(financial_data) {
  if (any(financial_data$pd < 0 | financial_data$pd >= 1)) {
    stop("Implausibe value(s) < 0 or >= 1 for pd detected. Please check.")
  }

  if (any(financial_data$net_profit_margin <= 0 | financial_data$net_profit_margin > 1)) {
    stop("Implausibe value(s) <= 0 or > 1 for net_profit_margin detected. Please check.")
  }


  if (any(financial_data$debt_equity_ratio < 0)) {
    stop("Implausibe value(s) < 0 for debt_equity_ratio detected. Please check.")
  }

  if (any(financial_data$volatility < 0)) {
    stop("Implausibe value(s) < 0 for volatility detected. Please check.")
  }
}

#' Read in price data
#'
#' This function reads in price data using long file format. It is expected to
#' work with data based on IEA WEO 2020.
#'
#' @param path A string that points to the location of the file containing the
#'   price data
#' @return A tibble holding price data in long format.
read_price_data <- function(path) {
  data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        year = "d",
        scenario = "c",
        scenario_geography = "c",
        ald_business_unit = "c",
        indicator = "c",
        unit = "c",
        price = "d"
      )
    )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "scenario", "scenario_geography", "ald_business_unit",
      "indicator", "unit", "price"
    )
  )

  data <- data %>%
    dplyr::select_at(c("year", "scenario", "ald_sector", "ald_business_unit", "price"))

  return(data)
}

#' Read in AR PAMS production data.
#'
#' @param path A string that points to the location of the file containing the
#'   AR PAMS dataset
#'
#' @family import functions
read_production_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        company_id = "d",
        company_name = "c",
        scenario_geography = "c",
        year = "d",
        ald_sector = "c",
        ald_business_unit = "c",
        plan_tech_prod = "d",
        plan_emission_factor = "d",
        plan_sec_prod = "d"
      )
    )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "scenario_geography", "year", "ald_sector",
      "ald_business_unit", "plan_tech_prod", "plan_emission_factor", "plan_sec_prod"
    )
  )

  return(data)
}

#' Read in scenario data
#'
#' Function reads in scenario data and checks for existence of required columns.
#'
#' @param path Path to dir holding scenario data.
#'
#' @return A tibble holding scenario data.
read_scenario_data <- function(path) {
  scenario_data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        scenario_geography = "c",
        scenario = "c",
        scenario_type = "c",
        ald_sector = "c",
        units = "c",
        ald_business_unit = "c",
        year = "d",
        direction = "c",
        fair_share_perc = "d"
      )
    )

  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c(
      "scenario_geography", "scenario", "scenario_type",
      "ald_sector", "units", "ald_business_unit", "year",
      "direction", "fair_share_perc"
    )
  )

  return(scenario_data)
}



#' Validate that a file exists in a given directory
#'
#' Before performing an operation on a file assumed to be found in a given
#' directory, validate this file exists and give indicative error if not.
#'
#' @param path Character vector indicating the directory of a file.
#'
#' @return String holding provided `path`.
#' @export
validate_file_exists <- function(path) {
  valid_file_path <- file.exists(path)

  if (!valid_file_path) {
    rlang::abort(c(
      "Path must point to an existing file.",
      x = glue::glue("Invalid file path: {file.path(path)}."),
      i = "Did you set path to data correctly?."
    ))
  }
  invisible(path)
}

#' Validate that a data frame contains expected columns
#'
#' Validate that all expected columns for an operation are given in a data frame.
#'
#' @param data data frame that is to be validated
#' @param expected_columns Character vector listing the expected columns
#'
#' @return NULL
#' @export
validate_data_has_expected_cols <- function(data,
                                            expected_columns) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_columns))

  data_has_expected_columns <-
    all(expected_columns %in% colnames(data))

  if (!data_has_expected_columns) {
    affected_cols <- glue::glue_collapse(sort(setdiff(expected_columns, names(data))), sep = ", ")
    rlang::abort(c(
      "Must include expected columns in data set.",
      x = glue::glue("Missing columns: {affected_cols}."),
      i = "Please check that data have expected columns."
    ))
  }
  invisible()
}


#' Generate transition scenario shock from a start year that represents when a
#' large scale climate transition policy is deployed.
#'
#' @param start_of_analysis A numeric vector of length one that indicates the
#'   start year of the analysis.
#' @param end_of_analysis A numeric vector of length one that indicates the
#'   end year of the analysis.
#' @param shock_year A numeric vector of length 1 that provides the start year
#'   of the shock to be used in the analysis.
generate_transition_shocks <- function(start_of_analysis,
                                       end_of_analysis,
                                       shock_year) {
  bounds <- list(start_of_analysis, end_of_analysis)

  if (dplyr::n_distinct(purrr::map_int(bounds, length)) > 1) {
    stop("Input arugments for start_of_analysis and end_of_analysis need to have length 1.")
  }

  input_args <- list(start_of_analysis, end_of_analysis, shock_year)

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  if (shock_year < start_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (shock_year > end_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  data <- tibble::tibble(
    year_of_shock = shock_year,
    scenario_name = glue::glue("Carbon balance {year_of_shock}"),
    duration_of_shock = end_of_analysis - .data$year_of_shock + 1
  )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario_name", "year_of_shock", "duration_of_shock"
    )
  )

  return(data)
}