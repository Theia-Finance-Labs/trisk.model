join_price_data <- function(df, df_prices) {
  # Joins price data by sector, ald_business_unit, year
  # scenario_geography NOT YET INCLUDED!
  df <- df %>%
    dplyr::inner_join(df_prices, by = c("ald_business_unit", "ald_sector", "year"))
}




check_price_consistency <- function(df, start_year) {
  # the year of shock must be greater or equal to the start year of the analysis
  if (!all(df$year >= start_year)) {
    stop(
      "Timerange for price data out of bounds. Past prices cannot be included
      in the further analysis."
    )
  }
  return(df)
}

check_scenario_availability <- function(portfolio, scen_data, scenarios = scenarios) {
  # check that scenarios in portfolio are allowed
  if (!all(portfolio %>% dplyr::pull(.data$scenario) %>% unique() %in% scenarios)) {
    stop(
      "Some scenarios in this data frame are not in the list allowed of scenarios.
      Please check!"
    )
  }
  # check that at least two allowed scenarios remain in portfolio
  if (length(portfolio %>% dplyr::pull(.data$scenario) %>% unique()) < 2) {
    stop(
      "There are less than two allowed scenarios in the portfolio. Stress test
      requires at least two!"
    )
  }
  # check scenarios in portfolio correspond to scenarios in scen data
  if (!all(portfolio %>% dplyr::pull(.data$scenario) %>% unique() %in% (scen_data %>% dplyr::pull(.data$scenario) %>% unique()))) {
    stop(
      "Scenarios differ between portfolio and scenario trajectory data. Check if
      correct inputs were used."
    )
  }
}

# check if the imported scenario data covers every year within the timeframe of analysis
check_scenario_timeframe <- function(scenario_data, start_year = start_year, end_year = end_year) {
  if (!all(seq(start_year, end_year) %in% (scenario_data %>% dplyr::pull(.data$year) %>% unique()))) {
    stop(
      glue::glue(
        "Imported scenario data does not cover the full time frame of the
        analysis. Scenario data must cover at least the period from {start_year}
        to {end_year}"
      )
    )
  } else {
    return(scenario_data)
  }
}
