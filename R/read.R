st_read_agnostic <- function(dir) {
  scenario_data_file = "scenario_data.csv"
  financial_data_file = "prewrangled_financial_data_stress_test.csv"
  production_data_file = "abcd_stress_test_input.csv"
  carbon_price_data_file = "ngfs_carbon_price.csv"

  out <- list(
    scenario_data = read_scenario_data(file.path(dir, scenario_data_file)),
    financial_data = read_financial_data(file.path(dir, financial_data_file)),
    production_data = read_production_data(file.path(dir, production_data_file)),
    carbon_data = read_carbon_data(file.path(dir, carbon_price_data_file))
  )
  return(out)
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
        technology = "c",
        indicator = "c",
        unit = "c",
        price = "d"
      )
    )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "scenario", "scenario_geography", "technology",
      "indicator", "unit", "price"
    )
  )

  data <- data %>%
    dplyr::select_at(c("year", "scenario", "ald_sector", "technology", "price"))

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
        technology = "c",
        plan_tech_prod = "d",
        plan_emission_factor = "d",
        plan_sec_prod = "d"
      )
    ) %>% 
    dplyr::rename(
      production_plan_company_technology = .data$plan_tech_prod,
      emission_factor = "plan_emission_factor"
    ) 

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "scenario_geography", "year", "ald_sector",
      "technology", "production_plan_company_technology", "emission_factor", "plan_sec_prod"
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
        technology = "c",
        year = "d",
        technology_type = "c",
        fair_share_perc = "d",
        indicator = "c",
        unit = "c",
        price = "d",
        capacity_factor="d"
      )
    ) %>%
    dplyr::mutate(
      technology_type = ifelse(.data$technology_type == "declining", "carbontech", "greentech"),
      scenario_type = ifelse(.data$scenario_type == "shock", "target", .data$scenario_type)
    )

  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c(
      "scenario_geography", "scenario", "scenario_type",
      "ald_sector", "units", "technology", "year",
      "technology_type", "fair_share_perc", "indicator", "unit", "price"
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