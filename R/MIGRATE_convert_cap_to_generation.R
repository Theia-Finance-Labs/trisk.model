

#' Translate power capacity to power generation
#'
#' Units of generated power are assumed to be sold and hence get priced in the
#' net profit calculations. This also entails converting MWh into MW per year,
#' since we calculate yearly profits. Note: For use in webscripts
#' [convert_cap_to_generation()] is used currently, which only distinguishes
#' capacity factor by ald_business_unit and scenario_geography, whereas this function
#' distinguishes further by year and scenario. Also note that for generation of
#' variable `plan_tech_prod` (planned capacity) capacity factors from baseline
#' scenario are used.
#'
#' @param data A data frame filtered and wrangled company level production
#'   forecasts (of the companies in the portfolio). Usually based on PACTA
#'   output.
#' @param capacity_factors_power A data frame containing capacity factors to
#'   translate company level power capacity to units sold. Contains information
#'   on the ald_business_unit (power sector only) and scenario_geography levels.
#' @param baseline_scenario String holding name of baseline scenario.
#' @param target_scenario String holding name of target scenario.
convert_power_cap_to_generation <- function(data,
                                            capacity_factors_power = NULL,
                                            baseline_scenario,
                                            target_scenario) {
  capacity_factors_power <- capacity_factors_power %>%
    dplyr::filter(.data$scenario %in% c(baseline_scenario, target_scenario)) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c("scenario_geography", "ald_business_unit", "year")),
      names_from = "scenario",
      names_prefix = "capfac_",
      values_from = "capacity_factor"
    )

  # ADO 1945 - Left join is applied since only rows in data from ald_sector
  # power will have matching rows in capacity_factors_power
  data <- data %>%
    dplyr::left_join(
      capacity_factors_power,
      by = c("ald_business_unit", "scenario_geography", "year")
    )

  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::mutate(
      # the planned generation is assumed to follow baseline
      plan_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$plan_tech_prod * !!rlang::sym(glue::glue("capfac_{baseline_scenario}")) * .env$hours_to_year,
        .data$plan_tech_prod
      ),
      !!rlang::sym(baseline_scenario) := dplyr::if_else(
        .data$ald_sector == "Power",
        !!rlang::sym(baseline_scenario) * !!rlang::sym(glue::glue("capfac_{baseline_scenario}")) * .env$hours_to_year,
        !!rlang::sym(baseline_scenario)
      ),
      !!rlang::sym(target_scenario) := dplyr::if_else(
        .data$ald_sector == "Power",
        !!rlang::sym(target_scenario) * !!rlang::sym(glue::glue("capfac_{target_scenario}")) * .env$hours_to_year,
        !!rlang::sym(target_scenario)
      ),
      scenario_geography = .data$scenario_geography
    ) %>%
    dplyr::select(-dplyr::starts_with("capfac_"))
}
