process_scenarios_data <- function(data, baseline_scenario, target_scenario, scenario_geography, start_analysis, end_analysis) {
  scenarios_data <- data$scenario_data
  
  scenarios_data <- scenarios_data %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_analysis, .env$end_analysis)) %>%
    tidyr::replace_na(list(capacity_factor = 1)) %>%
    dplyr::arrange(year, .by_group = TRUE)

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
    remove_sectors_with_missing_production_start_year()%>%
    extend_to_full_analysis_timeframe(
      start_analysis = start_analysis,
      end_analysis = end_analysis
    )

  return(assets_data)
}




#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries fo their forecasts and the phase out indicator.
#' @param start_analysis start of the analysis
#' @param end_analysis end of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,
                                              start_analysis,
                                              end_analysis) {
  data <- data %>%
    tidyr::complete(
      year = seq(.env$start_analysis, .env$end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "company_id", "company_name", "ald_sector", "technology", "scenario_geography"
          )
        )
      )
    ) %>%
    dplyr::arrange(
      .data$company_id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography, .data$year
    ) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "emission_factor",
        "pd",
        "net_profit_margin",
        "debt_equity_ratio",
        "volatility"
      ))
    )

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
  n_companies_pre <- length(unique(data$company_name))

  companies_missing_sector_production_start_year <- data %>%
    dplyr::filter(.data$year == min(.data$year)) %>%
    dplyr::group_by(
      .data$company_id, .data$ald_sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production_start_year,
      by = c("company_id", "ald_sector")
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
