#' Calculate baseline and transition shock trajectoroes
#'
#' @param input_data_list List with project agnostic and project specific input data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param shock_year year of transition from baseline scenario to target scenario
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#'
#' @return A tibble holding annual profits
extend_assets_trajectories <- function(trisk_model_input,
                                       shock_year) {

                 
  trajectories <- trisk_model_input %>%
    set_baseline_trajectory(
    ) %>%
    set_trisk_trajectory(
      shock_year = shock_year
    ) %>%
    apply_scenario_prices(
      shock_year = shock_year
    )


    trajectories  <- trajectories %>%
      dplyr::select_at(c(
        "year", "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography",
        "production_plan_company_technology",  "emission_factor", "production_scenario_baseline", "production_scenario_target",
        "production_change_scenario_baseline", "production_change_scenario_target", 
        "production_asset_baseline", "late_sudden", "overshoot_direction",
        "price_baseline", "price_target", "late_sudden_price"))

  return(trajectories)
}



#' Defines which scenario values to use for the baseline trajectory in the
#' stress test.
#'
#' @description
#' Picks the corresponding values from the original scenario
#' column indicated in the input and has the option to include PACTA based
#' production forecast for the first few years of the baseline
#' trajectory. If included, the trajectory after the end of the production
#' forecast is offset by the initial production forecast so that the
#' remainder of the baseline trajectory now is a parallel shift of the
#' original scenario values. If not included, the trajectories replicate
#' externally provided scenario trajectories.
#' Trajectories are furthermore differentiated by scenario_geography, if
#' multiple are passed.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/ald_business_unit level.
#'
#' @param data A dataframe that contains scenario trajectories by ald_business_unit
#'   until 2040 for all the scenarios included in the analysis and
#'   production build out plans by ald_business_unit or company and ald_business_unit,
#'   usually for 5 years, based on PACTA results.
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#'
#' @family scenario definition
#'
#' @return dataframe.
set_baseline_trajectory <- function(data) {
  data <- data %>%
    dplyr::mutate(
      production_asset_baseline = .data$production_plan_company_technology
    ) %>%
    # Fill the baseline/input production with the latest non-NA value
    tidyr::fill(.data$production_asset_baseline, .direction = "down") %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      # compute per group the cumulative sum of the scenario change derivatives at each year
      # add the cumsum to the input production , so that the latest non-NA value is incremented
      # by the cumulative sum of all scenario change local derivative value
      production_asset_baseline = .data$production_asset_baseline + cumsum(.data$production_change_scenario_baseline)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      production_asset_baseline = dplyr::if_else(.data$production_asset_baseline < 0, 0, .data$production_asset_baseline)
    )
  return(data)
}


#' Defines which scenario values to use for the late & sudden trajectory in the
#' stress test.
#'
#' @description
#' Picks the corresponding values from the original scenario
#' column indicated in the input and has the option to include PACTA based
#' production forecast for the first few years of the late & sudden
#' trajectory. Similarly, it is possible to define another input scenario
#' in case the company is already aligned after the production forecast.
#' If the production forecast is included, the trajectory after the end of
#' the production forecast is offset by the initial production forecast
#' so that the remainder of the late & sudden trajectory now is a parallel
#' shift of the original scenario values. If not included, the trajectories
#' replicate externally provided scenario trajectories at least until the
#' year of the policy shock.
#' Trajectories are calculated for each company by sector, scenario_geography,
#' ald_business_unit, year.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/ald_business_unit level.
#' @param data A dataframe that contains the scenario data prepared until the
#'   step after the baseline trajectories are calculated.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param shock_scenario A dataframe that contains information about the
#'   transition scenario, specifically the shock year and, duration of the
#'   shock and the name of the shock scenario
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @family scenario definition
#'
#' @return data frame
set_trisk_trajectory <- function(data,
                                 shock_year) {
  year_of_shock <- shock_year
  duration_of_shock <- shock_year - start_year

  late_sudden_df <- calc_late_sudden_traj(data,
    year_of_shock = year_of_shock
  )

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::left_join(late_sudden_df, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography", "year"))

  return(data)
}

calc_late_sudden_traj <- function(data, year_of_shock, TIME_FRAME_BEGONE = 5) {
  
  # Preprocess data to compute cumulative sums, overshoot direction, and fill missing values
  late_sudden_data <- data %>%
    dplyr::select_at(c(
      "company_id", "company_name", "year", "ald_sector", "ald_business_unit", "scenario_geography", 
      "production_plan_company_technology", "production_scenario_target", "production_change_scenario_target", "production_change_scenario_baseline"
    )) %>%
    dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      overshoot_direction = ifelse(
        dplyr::first(.data$production_scenario_target) -
          dplyr::last(.data$production_scenario_target) > 0,
        "Decreasing",
        "Increasing"
      ),
      # Compute cumulative sums for baseline and aligned scenario changes
      scenario_change_baseline_cumsum = cumsum(production_change_scenario_baseline),
      scenario_change_cumsum = cumsum(production_change_scenario_target),
      # Fill missing planned production values
      production_plan_company_technology_filled = .data$production_plan_company_technology,
    ) %>%
    tidyr::fill(production_plan_company_technology_filled, .direction = "down") %>%
    dplyr::ungroup()


  # Flag groups who need to be applied the overshoot compensation method.
  # Group will be applied the compensation if at least 1 year matches the condition.
  flagged_overshoot <- late_sudden_data %>%
    dplyr::filter(year > min(year), year <= min(year) + TIME_FRAME_BEGONE) %>% # TODO IS IT A BUG ??
    dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
    dplyr::summarise(
      prod_to_follow = sum(.data$production_scenario_target),
      real_prod = sum(.data$production_plan_company_technology),
      requires_overshoot_correction = any(
        (.data$overshoot_direction == "Decreasing" & (prod_to_follow < real_prod)) |
          (.data$overshoot_direction == "Increasing" & (prod_to_follow > real_prod))
      ),
      overshoot_direction = dplyr::first(overshoot_direction),
      .groups = "drop"
    )

  # Separate companies into those needing and not needing overshoot compensation
  to_compensate <- flagged_overshoot %>%
    dplyr::filter(requires_overshoot_correction) %>%
    dplyr::select(-c(requires_overshoot_correction, overshoot_direction))

  to_not_compensate <- flagged_overshoot %>%
    dplyr::filter(!requires_overshoot_correction) %>%
    dplyr::select(-c(requires_overshoot_correction, overshoot_direction))

  # Process companies requiring overshoot compensation
  if (nrow(to_compensate) > 0) {
    ls_data_to_compensate <- late_sudden_data %>%
      dplyr::inner_join(to_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"))

    ls_pre_clean_to_compensate <- ls_data_to_compensate %>%
      dplyr::mutate(
        late_sudden = ifelse(year <= year_of_shock, production_plan_company_technology_filled + scenario_change_baseline_cumsum, 0)
      )

    ls_pre_shock_to_compensate <- ls_pre_clean_to_compensate %>%
      dplyr::filter(year <= year_of_shock - 1) %>%
      dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
      dplyr::summarize(
        late_sudden_pre_shock_val = dplyr::last(late_sudden),
        late_sudden_pre_shock_tot = sum(late_sudden),
        .groups = "drop"
      )

    production_scenario_target_tot_to_compensate <- ls_data_to_compensate %>%
      dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
      dplyr::summarize(
        production_scenario_target_total_sum = sum(production_scenario_target),
        n_shocked_years = dplyr::last(year) - year_of_shock + 1,
        .groups = "drop"
      )

    x_integral_to_compensate <- dplyr::left_join(ls_pre_shock_to_compensate, production_scenario_target_tot_to_compensate,
      by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")
    ) %>%
      dplyr::mutate(
        sum_1_to_n_shocked_years = .data$n_shocked_years * (.data$n_shocked_years + 1) / 2,
        x = (.data$production_scenario_target_total_sum - .data$late_sudden_pre_shock_tot - .data$n_shocked_years * .data$late_sudden_pre_shock_val) /
          (-sum_1_to_n_shocked_years)
      ) %>%
      dplyr::select(company_id, company_name, ald_sector, ald_business_unit, scenario_geography, x)

    ls_overshoot_compensated <-
      dplyr::bind_rows(
        ls_pre_clean_to_compensate %>%
          dplyr::filter(.data$year < year_of_shock),
        ls_pre_clean_to_compensate %>%
          dplyr::filter(year >= year_of_shock) %>%
          dplyr::left_join(ls_pre_shock_to_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")) %>%
          dplyr::left_join(x_integral_to_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")) %>%
          dplyr::mutate(
            year_diff = year - year_of_shock + 1,
            late_sudden = late_sudden_pre_shock_val - pmax(year_diff, 0) * x
          )
      ) %>%
      dplyr::select(company_id, company_name, ald_sector, ald_business_unit, scenario_geography, year, late_sudden)
  } else {
    ls_overshoot_compensated <- dplyr::tibble(
      company_id = character(),
      company_name = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      scenario_geography = character(),
      year = integer(),
      late_sudden = numeric()
    )
  }

  # Process companies not requiring overshoot compensation
  if (nrow(to_not_compensate) > 0) {
    ls_data_to_not_compensate <- late_sudden_data %>%
      dplyr::inner_join(to_not_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"))

    ls_post_prod_not_compensated <- ls_data_to_not_compensate %>%
      dplyr::mutate(
        late_sudden = production_plan_company_technology_filled + scenario_change_cumsum
      ) %>%
      dplyr::select(company_id, company_name, ald_sector, ald_business_unit, scenario_geography, year, late_sudden)
  } else {
    ls_post_prod_not_compensated <- dplyr::tibble(
      company_id = character(),
      company_name = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      scenario_geography = character(),
      year = integer(),
      late_sudden = numeric()
    )
  }

  # Combine results
  late_sudden_df <- dplyr::bind_rows(ls_overshoot_compensated, ls_post_prod_not_compensated) %>%
    dplyr::select_at(c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography", "year", "late_sudden"))
  late_sudden_df <- late_sudden_df %>%
    dplyr::left_join(
      flagged_overshoot %>%
        dplyr::select(-requires_overshoot_correction) %>%
        dplyr::distinct_at(c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography", "overshoot_direction")),
      by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")
    )

  # filter_negative_late_and_sudden
  late_sudden_df <- late_sudden_df %>%
    dplyr::mutate(
      late_sudden = pmax(.data$late_sudden, 0)
    )

  return(late_sudden_df)
}
