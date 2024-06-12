process_assets_data <- function(data, start_analysis, end_analysis, scenario_geography) {
  production_financial_data <- dplyr::inner_join(
    data$production_data, 
    data$financial_data, by = "company_id") %>%
    dplyr::filter(.data$scenario_geography == .env$scenario_geography)

  assets_data <- production_financial_data %>%
    summarise_production_technology_forecasts(
      start_analysis = start_analysis
    ) %>%
    summarise_production_sector_forecasts() %>%
    extend_to_full_analysis_timeframe(
      start_analysis = start_analysis,
      end_analysis = end_analysis
    )

  return(assets_data)
}


#' Summarise the forecasts for company-tech level production within the five
#' year time frame
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param start_analysis start of the analysis
#' @noRd
summarise_production_technology_forecasts <- function(data,
                                                      start_analysis) {
  time_frame <- 5
  data <- data %>%
    dplyr::select(
      dplyr::all_of(c(
        "company_id", "company_name", "ald_sector", "ald_business_unit",
        "scenario_geography", "year", "plan_tech_prod", "plan_sec_prod",
        "plan_emission_factor"
      ))
    ) %>%
    dplyr::filter(.data$year <= .env$start_analysis + .env$time_frame) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      # Initial value is identical between production and scenario target,
      # can thus be used for both
      initial_technology_production = dplyr::first(.data$plan_tech_prod),
      final_technology_production = dplyr::last(.data$plan_tech_prod),
      sum_production_forecast = sum(.data$plan_tech_prod, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(data)
}


#' Summarise the forecasts for company-sector level production within the five
#' year time frame
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
summarise_production_sector_forecasts <- function(data) {
  data <- data %>%
    # dplyr::select(.data$company_id, .data$company_name, .data$ald_sector,
    #               .data$initial_technology_production,
    #               .data$ald_business_unit, .data$emission_factor,
    #              .data$scenario_geography, .data$year, .data$plan_sec_prod, .data$plan_tech_prod) %>%
    # dplyr::group_by(
    #   .data$company_id, .data$company_name, .data$ald_sector, .data$scenario,
    #   .data$scenario_geography, .data$year
    # ) %>%
    # dplyr::mutate(plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE)) %>%
    # tidyr::fill("plan_sec_prod", .direction="down") %>%
    # dplyr::ungroup() %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$scenario_geography
    ) %>%
    dplyr::arrange(.data$year, by_group = TRUE) %>%
    dplyr::mutate(
      # first year plan and scenario values are equal by construction,
      # can thus be used for production and target
      initial_sector_production = dplyr::first(.data$plan_sec_prod)
    ) %>%
    dplyr::ungroup()
}
