#' Read agnostic data from a directory
#'
#' This function reads scenario, financial, assets, and carbon price data
#' from CSV files located in a specified directory.
#'
#' @param dir A string representing the directory where the CSV files are stored.
#' @return A list containing four elements: `scenarios_data`, `financial_data`, `assets_data`, and `carbon_data`.
#' @export
st_read_agnostic <- function(dir) {
  scenarios_data_file <- "scenarios.csv"
  financial_data_file <- "financial_features.csv"
  assets_data_file <- "assets.csv"
  carbon_price_data_file <- "ngfs_carbon_price.csv"

  out <- list(
    scenarios_data = read_scenario_data(file.path(dir, scenarios_data_file)),
    financial_data = read_financial_data(file.path(dir, financial_data_file)),
    assets_data = read_production_data(file.path(dir, assets_data_file)),
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
  if (file.exists(file.path(path))) {
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
  } else {
    data <- NULL # TODO handle cases when carbon data is null
  }
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
        company_id = "c",
        pd = "d",
        net_profit_margin = "d",
        debt_equity_ratio = "d",
        volatility = "d"
      )
    )

  check_valid_financial_data_values(data)
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
        asset_id = "c",
        asset_name = "c",
        company_id = "c",
        company_name = "c",
        country_iso2 = "c",
        production_year = "d",
        sector = "c",
        technology = "c",
        emission_factor = "d",
        plan_tech_prod="d",
        plan_sec_prod="d"
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
        sector = "c",
        # pathway_unit = "c",
        technology = "c",
        scenario_year = "d",
        scenario_type = "c",
        scenario_price = "d",
        scenario_capacity_factor = "d",
        scenario_pathway = "d",
        country_iso2_list="c"
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
#' @param financial_data data frame that is to be validated
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
      dplyr::select(-c(.data$scenario_geography)) %>%
      dplyr::filter(.data$scenario %in% .env$carbon_price_model)
  }

  return(data_processed)
}
