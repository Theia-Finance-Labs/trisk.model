#' Calculate baseline and transition shock trajectoroes
#'
#' @param trisk_model_input List with project agnostic and project specific input data
#' @param shock_year year of transition from baseline scenario to target scenario
#' @param start_year Numeric, holding start year of analysis.
#'
#' @return A tibble holding annual profits
extend_assets_trajectories <- function(
    trisk_model_input,
    start_year,
    shock_year) {
  trajectories <- trisk_model_input %>%
    set_baseline_trajectory() %>%
    set_trisk_trajectory(
      start_year = start_year,
      shock_year = shock_year
    ) %>%
    apply_scenario_prices(
      shock_year = shock_year
    )


  trajectories <- trajectories %>%
    dplyr::select_at(c(
      "year", "asset_id", "asset_name", "company_id", "company_name", "sector", "technology",
      "production_plan_company_technology", "production_scenario_baseline", "production_scenario_target",
      "production_change_scenario_baseline", "production_change_scenario_target",
      "production_asset_baseline", "late_sudden", "overshoot_direction",
      "scenario_price_baseline", "scenario_price_target", "late_sudden_price", "plant_age_rank"
    ))



  # attach the necessary columns for the rest
  trisk_model_output <- trajectories %>%
    dplyr::left_join(
      trisk_model_input %>%
        dplyr::distinct(
          .data$asset_id, .data$company_id, .data$sector, .data$technology, .data$technology_type,
          .data$scenario_geography, .data$year, .data$emission_factor, .data$debt_equity_ratio,
          .data$net_profit_margin, .data$pd, .data$volatility, .data$country_iso2
        ),
      by = c("asset_id", "company_id", "sector", "technology", "year")
    ) 


  return(trisk_model_output)
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
#' portfolio/technology level.
#'
#' @param data A dataframe that contains scenario trajectories by technology
#'   until 2040 for all the scenarios included in the analysis and
#'   production build out plans by technology or company and technology,
#'   usually for 5 years, based on PACTA results.
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
      .data$company_id, .data$asset_id, .data$sector, .data$technology
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
#' technology, year.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/technology level.
#' @param data A dataframe that contains the scenario data prepared until the
#'   step after the baseline trajectories are calculated.
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param shock_year Numeric. the shock year.
#' @family scenario definition
#'
#' @return data frame
set_trisk_trajectory <- function(data,
                                 start_year,
                                 shock_year) {

  late_sudden_df <- calc_late_sudden_traj(data,
    year_of_shock = shock_year
  )

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::left_join(late_sudden_df, by = c("company_id", "asset_id", "sector", "technology", "year"))

  return(data)
}

calc_late_sudden_traj <- function(data, year_of_shock) {
  
  # Preprocess data to compute cumulative sums, overshoot direction, and fill missing values
  late_sudden_data <- data %>%
    dplyr::select_at(c(
      "asset_id", "company_id", "year", "sector", "technology", "plant_age_rank",
      "production_plan_company_technology", "production_scenario_target", "production_change_scenario_target", "production_change_scenario_baseline"
    )) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(
      overshoot_direction = ifelse(
        dplyr::first(.data$production_scenario_target) -
          dplyr::last(.data$production_scenario_target) > 0,
        "Decreasing",
        "Increasing"
      ),
      # Compute cumulative sums for baseline and aligned scenario changes
      scenario_change_baseline_cumsum = cumsum(.data$production_change_scenario_baseline),
      scenario_change_cumsum = cumsum(.data$production_change_scenario_target),
      # Fill missing planned production values
      production_plan_company_technology_filled = .data$production_plan_company_technology,
    ) %>%
    tidyr::fill(.data$production_plan_company_technology_filled, .direction = "down") %>%
    dplyr::ungroup()


  # Apply the function to get the last non-NA year for each group
  last_non_na_positions <- late_sudden_data %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::summarise(
      last_non_na_year = max(.data$year[!is.na(.data$production_plan_company_technology)]),
      .groups = "drop"
    )

  # Flag groups who need to be applied the overshoot compensation method.
  # Group will be applied the compensation if at least 1 year matches the condition.
  flagged_overshoot <- late_sudden_data %>%
    dplyr::inner_join(last_non_na_positions, by = c("asset_id", "company_id", "sector", "technology")) %>%
    dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
    dplyr::filter(.data$year > min(.data$year), .data$year <= .data$last_non_na_year) %>% # TODO IS (year > min(year), and not (year >= min(year),  A BUG ??
    dplyr::summarise(
      prod_to_follow = sum(.data$production_scenario_target, na.rm = TRUE),
      real_prod = sum(.data$production_plan_company_technology, na.rm = TRUE),
      requires_overshoot_correction = any(
        (.data$overshoot_direction == "Decreasing" & (.data$prod_to_follow < .data$real_prod)) |
          (.data$overshoot_direction == "Increasing" & (.data$prod_to_follow > .data$real_prod))
      ),
      overshoot_direction = dplyr::first(.data$overshoot_direction),
      .groups = "drop"
    )

  # Separate companies into those needing and not needing overshoot compensation
  to_compensate <- flagged_overshoot %>%
    dplyr::filter(.data$requires_overshoot_correction) %>%
    dplyr::select(-c(.data$requires_overshoot_correction, .data$overshoot_direction))

  to_not_compensate <- flagged_overshoot %>%
    dplyr::filter(!.data$requires_overshoot_correction) %>%
    dplyr::select(-c(.data$requires_overshoot_correction, .data$overshoot_direction))

  # Process companies requiring overshoot compensation
  if (nrow(to_compensate) > 0) {
    ls_data_to_compensate <- late_sudden_data %>%
      dplyr::inner_join(to_compensate, by = c("asset_id", "company_id", "sector", "technology"))

    ls_pre_clean_to_compensate <- ls_data_to_compensate %>%
      dplyr::mutate(
        late_sudden = ifelse(.data$year <= (.env$year_of_shock + .data$plant_age_rank), .data$production_plan_company_technology_filled + .data$scenario_change_baseline_cumsum, 0)
      )

    ls_pre_shock_to_compensate <- ls_pre_clean_to_compensate %>%
      dplyr::filter(.data$year <= (.env$year_of_shock  - 1 + .data$plant_age_rank)) %>%
      dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
      dplyr::summarize(
        late_sudden_pre_shock_val = dplyr::last(.data$late_sudden),
        late_sudden_pre_shock_tot = sum(.data$late_sudden),
        .groups = "drop"
      )

    production_scenario_target_tot_to_compensate <- ls_data_to_compensate %>%
      dplyr::group_by(.data$asset_id, .data$company_id, .data$sector, .data$technology) %>%
      dplyr::summarize(
        production_scenario_target_total_sum = sum(.data$production_scenario_target),
        n_shocked_years = dplyr::last(.data$year) - (.env$year_of_shock  + 1 + .data$plant_age_rank),
        .groups = "drop"
      )

    x_integral_to_compensate <- dplyr::left_join(ls_pre_shock_to_compensate, production_scenario_target_tot_to_compensate,
      by = c("asset_id", "company_id", "sector", "technology")
    ) %>%
      dplyr::mutate(
        sum_1_to_n_shocked_years = .data$n_shocked_years * (.data$n_shocked_years + 1) / 2,
        x = (.data$production_scenario_target_total_sum - .data$late_sudden_pre_shock_tot - .data$n_shocked_years * .data$late_sudden_pre_shock_val) /
          (-.data$sum_1_to_n_shocked_years)
      ) %>%
      dplyr::select(.data$asset_id, .data$company_id, .data$sector, .data$technology, .data$x)

    ls_overshoot_compensated <-
      dplyr::bind_rows(
        ls_pre_clean_to_compensate %>%
          dplyr::filter(.data$year <  (.env$year_of_shock + .data$plant_age_rank)),
        ls_pre_clean_to_compensate %>%
          dplyr::filter(.data$year >=  (.env$year_of_shock + .data$plant_age_rank)) %>%
          dplyr::left_join(ls_pre_shock_to_compensate, by = c("asset_id", "company_id", "sector", "technology")) %>%
          dplyr::left_join(x_integral_to_compensate, by = c("asset_id", "company_id", "sector", "technology")) %>%
          dplyr::mutate(
            year_diff = .data$year -  (.env$year_of_shock  + 1 + .data$plant_age_rank),
            late_sudden = .data$late_sudden_pre_shock_val - pmax(.data$year_diff, 0) * .data$x
          )
      ) %>%
      dplyr::select(.data$asset_id, .data$company_id, .data$sector, .data$technology, .data$year, .data$late_sudden)
  } else {
    ls_overshoot_compensated <- dplyr::tibble(
      company_id = character(),
      asset_id = character(),
      sector = character(),
      technology = character(),
      year = integer(),
      late_sudden = numeric()
    )
  }

  # Process companies not requiring overshoot compensation
  if (nrow(to_not_compensate) > 0) {
    ls_data_to_not_compensate <- late_sudden_data %>%
      dplyr::inner_join(to_not_compensate, by = c("asset_id", "company_id", "sector", "technology"))

    ls_post_prod_not_compensated <- ls_data_to_not_compensate %>%
      dplyr::mutate(
        late_sudden = .data$production_plan_company_technology_filled + .data$scenario_change_cumsum
      ) %>%
      dplyr::select(.data$asset_id, .data$company_id, .data$sector, .data$technology, .data$year, .data$late_sudden)
  } else {
    ls_post_prod_not_compensated <- dplyr::tibble(
      company_id = character(),
      asset_id = character(),
      sector = character(),
      technology = character(),
      year = integer(),
      late_sudden = numeric()
    )
  }

  # Combine results
  late_sudden_df <- dplyr::bind_rows(ls_overshoot_compensated, ls_post_prod_not_compensated) %>%
    dplyr::select_at(c("asset_id", "company_id", "sector", "technology", "year", "late_sudden"))
  late_sudden_df <- late_sudden_df %>%
    dplyr::left_join(
      flagged_overshoot %>%
        dplyr::select(-.data$requires_overshoot_correction) %>%
        dplyr::distinct_at(c("asset_id", "company_id", "sector", "technology", "overshoot_direction")),
      by = c("asset_id", "company_id", "sector", "technology")
    )

  # filter_negative_late_and_sudden
  late_sudden_df <- late_sudden_df %>%
    dplyr::mutate(
      late_sudden = pmax(.data$late_sudden, 0)
    )

  return(late_sudden_df)
}
