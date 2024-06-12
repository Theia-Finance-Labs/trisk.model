#' Calculate the ratio of the required change in ald_business_unit that each company
#' has achieved per ald_business_unit at the end of the production forecast period.
#' This ratio will later serve to adjust the net profit margin for companies
#' that have not built out enough production capacity in increasing technologies
#' and hence need to scale up production to compensate for their lag in buildout.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param start_analysis Numeric. A vector of length 1 indicating the start
#'   year of the analysis.
#' @param target_scenario Character. A vector of length 1 indicating target
#'   scenario
#'
#' @noRd
calculate_proximity_to_target <- function(data,
                                          start_analysis = 2022,
                                          target_scenario) {
  time_frame <- 5
  production_changes <- data %>%
    dplyr::filter(
      dplyr::between(
        .data$year, .env$start_analysis, .env$start_analysis + .env$time_frame
      ),
      .data$scenario == .env$target_scenario
    ) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      required_change = .data$scen_tech_prod - .data$initial_technology_production,
      realised_change = .data$plan_tech_prod - .data$initial_technology_production
    ) %>%
    dplyr::summarise(
      sum_required_change = sum(.data$required_change, na.rm = TRUE),
      sum_realised_change = sum(.data$realised_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ratio_realised_required = .data$sum_realised_change / .data$sum_required_change,
      proximity_to_target = dplyr::case_when(
        .data$ratio_realised_required < 0 ~ 0,
        .data$ratio_realised_required > 1 ~ 1,
        TRUE ~ .data$ratio_realised_required
      )
    ) %>%
    dplyr::select(
      -dplyr::all_of(c(
        "sum_required_change", "sum_realised_change",
        "ratio_realised_required"
      ))
    )

  data <- data %>%
    dplyr::inner_join(
      production_changes,
      by = c(
        "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"
      )
    )
}
