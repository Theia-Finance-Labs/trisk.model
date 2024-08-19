process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography) {
  
  scenarios_data <- data$scenario_data %>%
    dplyr::filter(
      .data$scenario %in% c(baseline_scenario, target_scenario),
      .data$scenario_geography %in% .env$scenario_geography
    ) %>%
    purrr::when(
      nrow(.) > 0 ~ .,
      ~ stop("Error in process_scenarios_data: The dataframe is empty after filtering for scenarios and geographies.")
    ) %>%
    dplyr::group_by(.data$scenario) %>%
    dplyr::mutate(
      min_scenario_year = min(.data$scenario_year),
      max_scenario_year = max(.data$scenario_year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::between(.data$scenario_year, min(min_scenario_year), min(max_scenario_year))) %>%
    dplyr::arrange(.data$scenario_year) %>%
    dplyr::select(-c(.data$min_scenario_year, .data$max_scenario_year))

  stopifnot(nrow(scenarios_data) > 0)

  # compute fair_share_perc, used to extend asset's trajectories
  scenarios_data <- scenarios_data %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    calculate_fair_share_perc()

  return(scenarios_data)
}


process_assets_data <- function(data, scenario_geography) {
  production_financial_data <- dplyr::inner_join(
    data$production_data,
    data$financial_data,
    by = "company_id"
  ) %>%
    dplyr::filter(.data$scenario_geography == .env$scenario_geography)

  assets_data <- production_financial_data %>%
    remove_sectors_with_missing_production_start_year() %>%
    compute_plan_sec_prod()

  stopifnot(nrow(assets_data) > 0)

  return(assets_data)
}






#' Remove rows from PACTA results that belong to company-sector combinations
#' for which there is no positive production value in the relevant start year.
#' This handles the edge case that a company may have a green ald_business_unit with
#' zero initial production that should grow over time, but since the overall
#' sector production is also zero in the start year, the SMSP is unable to
#' calculate positive targets.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
remove_sectors_with_missing_production_start_year <- function(data) {
  companies_missing_sector_production_start_year <- data %>%
    dplyr::filter(.data$production_year == min(.data$production_year)) %>%
    dplyr::group_by(
      .data$company_id, .data$sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$production_plan_company_technology, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production_start_year,
      by = c("company_id", "sector")
    )

  # n_companies_post <- length(unique(data_filtered$company_name))

  # if (n_companies_pre > n_companies_post) {
  #   percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
  #   affected_companies <- sort(
  #     setdiff(
  #       data$company_name,
  #       data_filtered$company_name
  #     )
  #   )
  #   paste_write(
  #     format_indent_1(), "When filtering out holdings with 0 production in
  #     relevant sector in the start year of the analysis, dropped rows for",
  #     n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
  #     log_path = log_path
  #   )
  #   paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
  #   paste_write(format_indent_2(), "affected companies:", log_path = log_path)
  #   purrr::walk(affected_companies, function(company) {
  #     paste_write(format_indent_2(), company, log_path = log_path)
  #   })
  # }


  return(data_filtered)
}


compute_plan_sec_prod <- function(data) {
  data <- data %>%
    dplyr::group_by(.data$scenario_geography, .data$company_id, .data$sector, .data$production_year) %>%
    dplyr::mutate(plan_sec_prod = sum(.data$production_plan_company_technology, na.rm = TRUE)) %>%
    dplyr::ungroup()
  return(data)
}
