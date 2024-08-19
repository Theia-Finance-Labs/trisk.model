process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography, start_analysis, end_analysis) {
  scenarios_data <- data$scenario_data

  scenarios_data <- scenarios_data %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$scenario_year, .env$start_analysis, .env$end_analysis)) %>%
    dplyr::arrange(scenario_year, .by_group = TRUE) %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    calculate_fair_share_perc()


  return(scenarios_data)
}

process_assets_data <- function(data, start_analysis, end_analysis, scenario_geography) {
  production_financial_data <- dplyr::inner_join(
    data$production_data,
    data$financial_data,
    by = "company_id"
  ) %>%
    dplyr::filter(.data$scenario_geography == .env$scenario_geography)

  assets_data <- production_financial_data %>%
    remove_sectors_with_missing_production_start_year() %>%
    extend_to_full_analysis_timeframe(
      start_analysis = start_analysis,
      end_analysis = end_analysis
    ) %>%
    compute_plan_sec_prod()

  return(assets_data)
}




#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries of their forecasts, and the phase-out indicator.
#' @param start_analysis Start of the analysis
#' @param end_analysis End of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,
                                              start_analysis,
                                              end_analysis) {
  data <- data %>%
    tidyr::complete(
      production_year = seq(start_analysis, end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "asset_id", "company_id", "sector", "technology"
          )
        )
      )
    ) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::arrange(
      .data$production_year,
      .by_group = TRUE
    ) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "asset_name",
        "company_name",
        "country_iso2",
        "scenario_geography",
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility"
      )),
      .direction = "down"
    ) %>%
    tidyr::fill(
      c(
        "asset_name",
        "company_name",
        "country_iso2",
        "scenario_geography",
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility",
        "production_plan_company_technology"
      ),
      .direction = "up"
    ) %>%
    ungroup()

  # Fill down production_plan_company_technology only up to the start_analysis year
  data_before_start_analysis <- data %>%
    filter(production_year <= start_analysis) %>%
    group_by(asset_id, company_id, sector, technology) %>%
    arrange(production_year) %>%
    tidyr::fill(production_plan_company_technology, .direction = "down") %>%
    ungroup()

  # Combine filled data before start_analysis with the rest of the data
  data <- data_before_start_analysis %>%
    bind_rows(filter(data, production_year > start_analysis)) %>%
    arrange(asset_id, company_id, sector, technology, production_year)

  return(data)
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
    dplyr::group_by(scenario_geography, company_id, sector, production_year) %>%
    dplyr::mutate(plan_sec_prod = sum(production_plan_company_technology, na.rm = TRUE)) %>%
    dplyr::ungroup()
  return(data)
}
