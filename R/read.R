st_read_agnostic <- function(
    dir,
    capacity_factor_file="prewrangled_capacity_factors.csv",
    price_data_file="price_data_long.csv",
    scenario_data_file="Scenarios_AnalysisInput.csv",
    financial_data_file="prewrangled_financial_data_stress_test.csv", 
    production_data_file="abcd_stress_test_input.csv", 
    carbon_price_data_file="ngfs_carbon_price.csv"
    ) {

  out <- list(
    capacity_factors_power = read_capacity_factors_power(file.path(dir, capacity_factor_file)),
    df_price = read_price_data(file.path(dir, price_data_file)),
    scenario_data = read_scenario_data(file.path(dir, scenario_data_file)),
    financial_data = read_financial_data(file.path(dir, financial_data_file)),
    production_data = read_production_data(file.path(dir, production_data_file)),
    carbon_data <- read_carbon_data(file.path(dir, carbon_price_data_file))
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
