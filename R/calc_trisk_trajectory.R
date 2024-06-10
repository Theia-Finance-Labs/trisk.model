#' Calculate transition shock trajectory
#'
#' @param input_data_list List with project agnostic and project specific input data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#'
#' @return A tibble holding annual profits
calculate_trisk_trajectory <- function(input_data_list,
                                       baseline_scenario,
                                       target_scenario,
                                       shock_year,
                                       start_year,
                                       end_year) {
  production_data <- input_data_list$production_data %>%
    set_baseline_trajectory(
      baseline_scenario = baseline_scenario
    ) %>%
    set_trisk_trajectory(
      target_scenario = target_scenario,
      target_scenario_aligned = target_scenario,
      start_year = start_year,
      end_year = end_year,
      shock_year = shock_year
    )

  price_data <- input_data_list$df_price %>%
    calc_scenario_prices(
      baseline_scenario = baseline_scenario,
      target_scenario = target_scenario,
      start_year = start_year,
      shock_year = shock_year,
      duration_of_shock = end_year - shock_year + 1 # TODO REMOVE
    )

  full_trajectory <- production_data %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = c("company_id")
    )

  full_trajectory <- full_trajectory %>%
    join_price_data(df_prices = price_data)

  return(full_trajectory)
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
set_baseline_trajectory <- function(data,
                                    baseline_scenario) {
  data <- data %>%
    dplyr::mutate(
      scen_to_follow = !!rlang::sym(baseline_scenario),
      # compute the scenario change derivative, where the input production is NA
      scenario_change = dplyr::if_else(
        is.na(.data$plan_tech_prod),
        .data$scen_to_follow - dplyr::lag(.data$scen_to_follow, default = 0),
        0
      ),
      baseline = .data$plan_tech_prod
    ) %>%
    # Fill the baseline/input production with the latest non-NA value
    tidyr::fill(.data$baseline, .direction = "down") %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      # compute per group the cumulative sum of the scenario change derivatives at each year
      # add the cumsum to the input production , so that the latest non-NA value is incremented
      # by the cumulative sum of all scenario change local derivative value
      baseline = .data$baseline + cumsum(.data$scenario_change)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      baseline = dplyr::if_else(.data$baseline < 0, 0, .data$baseline),
      baseline_change = .data$scenario_change
    ) %>%
    dplyr::select(-dplyr::all_of(c("scenario_change", "scen_to_follow")))

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
#' @param target_scenario_aligned Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories in case the company is aligned after
#'   the forecast period.
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @family scenario definition
#'
#' @return data frame
set_trisk_trajectory <- function(data,
                                 target_scenario,
                                 shock_year,
                                 target_scenario_aligned,
                                 start_year,
                                 end_year) {
  year_of_shock <- shock_year
  duration_of_shock <- shock_year - start_year

  data <- data %>%
    dplyr::mutate(
      scen_to_follow = !!rlang::sym(target_scenario),
      scen_to_follow_aligned = !!rlang::sym(target_scenario_aligned),
      scenario_change =
        dplyr::if_else(
          is.na(.data$plan_tech_prod),
          .data$scen_to_follow - dplyr::lag(.data$scen_to_follow),
          0
        ),
      scenario_change_aligned =
        dplyr::if_else(
          is.na(.data$plan_tech_prod),
          .data$scen_to_follow_aligned - dplyr::lag(.data$scen_to_follow_aligned),
          0
        ),
      scenario_change_baseline = dplyr::if_else(
        is.na(.data$plan_tech_prod),
        .data$baseline - dplyr::lag(.data$baseline),
        0
      )
    )


  # data %>%
  #     dplyr::group_by(
  #       .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
  #       .data$scenario_geography
  #     ) %>%
  #     dplyr::mutate(
  #       overshoot_direction = rep(
  #         dplyr::if_else(
  #           .data$scen_to_follow[1] - .data$scen_to_follow[length(.data$scen_to_follow)] > 0,
  #           "Decreasing",
  #           "Increasing"
  #         ),
  #         dplyr::n()
  #       ))%>% dplyr::filter(company_id==942, ald_business_unit=="Gas")

  # data %>%
  #     dplyr::group_by(
  #       .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
  #       .data$scenario_geography
  #     ) %>%
  #     dplyr::mutate(
  #       overshoot_direction = rep(
  #         dplyr::if_else(
  #           .data$scen_to_follow[1] - .data$scen_to_follow[length(.data$scen_to_follow)] > 0,
  #           "Decreasing",
  #           "Increasing"
  #         ),
  #         dplyr::n()
  #       ),
  #       late_sudden = calc_late_sudden_traj(
  #         start_year = start_year,
  # #         end_year = end_year,
  #         year_of_shock = year_of_shock,
          # scen_to_follow = aa$scen_to_follow
          # planned_prod = aa$plan_tech_prod
          # late_sudden = aa$plan_tech_prod
          # scenario_change_baseline = aa$scenario_change_baseline
          # scenario_change_aligned = aa$scenario_change_aligned
          # overshoot_direction = "Increasing"
  #         data=data
  #       )
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(
  #       -dplyr::all_of(c(
  #         "scen_to_follow",
  #         "scenario_change",
  #         "scenario_change_baseline",
  #         "scenario_change_aligned"
  #       ))
  #     )



  late_sudden_df <- calc_late_sudden_traj2(data,
    start_year = start_year,
    end_year = end_year,
    year_of_shock = year_of_shock
  )

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(late_sudden_df, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography", "year"))

  data <- filter_negative_late_and_sudden(data)

  return(data)
}

calc_late_sudden_traj2 <- function(data, start_year, end_year, year_of_shock, TIME_FRAME_BEGONE=5) {
  
  # Preprocess data to compute cumulative sums, overshoot direction, and fill missing values
  ladata <- data %>%
    dplyr::select_at(c(
      "company_id", "company_name", "year", "ald_sector", "ald_business_unit", "scenario_geography",
      "plan_tech_prod", "scen_to_follow", "scenario_change_aligned", "scenario_change_baseline"
    ))  %>%
    dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      overshoot_direction = ifelse( dplyr::first(.data$scen_to_follow) - dplyr::last(.data$scen_to_follow) > 0, "Decreasing", "Increasing"),
      # Compute cumulative sums for baseline and aligned scenario changes
      scenario_change_baseline_cumsum = cumsum(scenario_change_baseline),
      scenario_change_aligned_cumsum = cumsum(scenario_change_aligned),
      # Fill missing planned production values
      plan_tech_prod_filled = .data$plan_tech_prod,
    ) %>%
    tidyr::fill(plan_tech_prod_filled, .direction = "down") %>%
    dplyr::ungroup()


  # Flag groups who need to be applied the overshoot compensation method
  # group is considered to be applied the compensation if at least 1 year matches the condition
  # overshoot_direction is the same for an entire group, and in all known cases the condition
  #   returns the same value for all rows of the group
  flagged_overshoot <- ladata %>%
  dplyr::filter(year > min(year), year <= min(year)+TIME_FRAME_BEGONE)  %>% # TODO IS IT A BUG ??
  dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
    dplyr::summarise(
      prod_to_follow = sum(.data$scen_to_follow),
      real_prod = sum(.data$plan_tech_prod),
      requires_overshoot_correction = any(
        (.data$overshoot_direction == "Decreasing" & (prod_to_follow < real_prod)) |
          (.data$overshoot_direction == "Increasing" & (prod_to_follow > real_prod))
      ),
      overshoot_direction = dplyr::first(overshoot_direction)
    , .groups="drop")

  # Separate companies into those needing and not needing overshoot compensation
  to_compensate <- flagged_overshoot %>%
    dplyr::filter(requires_overshoot_correction) %>%
    dplyr::select(-c(requires_overshoot_correction, overshoot_direction))

  to_not_compensate <- flagged_overshoot %>%
    dplyr::filter(!requires_overshoot_correction) %>%
    dplyr::select(-c(requires_overshoot_correction, overshoot_direction))

  # Process companies requiring overshoot compensation
  if (nrow(to_compensate) > 0) {
    ladata_to_compensate <- ladata %>%
      dplyr::inner_join(to_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"))

    ls_pre_clean_to_compensate <- ladata_to_compensate %>%
      dplyr::mutate(
        late_sudden = ifelse(year <= year_of_shock, plan_tech_prod_filled + scenario_change_baseline_cumsum, 0)
      )

    ls_pre_shock_to_compensate <- ls_pre_clean_to_compensate %>%
      dplyr::filter(year <= year_of_shock - 1) %>%
      dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
      dplyr::summarize(
        late_sudden_pre_shock_val = dplyr::last(late_sudden),
        late_sudden_pre_shock_tot = sum(late_sudden),
        .groups = "drop"
      )

    scen_to_follow_tot_to_compensate <- ladata_to_compensate %>%
      dplyr::group_by(company_id, company_name, ald_sector, ald_business_unit, scenario_geography) %>%
      dplyr::summarize(
        scen_to_follow_total_sum = sum(scen_to_follow),
        n_shocked_years = dplyr::last(year) - year_of_shock + 1,
        .groups = "drop"
      )

    x_integral_to_compensate <- dplyr::left_join(ls_pre_shock_to_compensate, scen_to_follow_tot_to_compensate,
      by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")
    ) %>%
      dplyr::mutate(
        sum_1_to_n_shocked_years = .data$n_shocked_years * (.data$n_shocked_years + 1) / 2,
        x = (.data$scen_to_follow_total_sum - .data$late_sudden_pre_shock_tot - .data$n_shocked_years * .data$late_sudden_pre_shock_val) /
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
      )%>%
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
    ladata_to_not_compensate <- ladata %>%
      dplyr::inner_join(to_not_compensate, by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"))

    ls_post_prod_not_compensated <- ladata_to_not_compensate %>%
      dplyr::mutate(
        late_sudden = plan_tech_prod_filled + scenario_change_aligned_cumsum
        
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
        dplyr::distinct_at(c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography", "overshoot_direction"))
      , by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography"))

  return(late_sudden_df)
}

#' Calculate how the production trajectory for a company/ald_business_unit changes
#' after the policy shock hits.
#'
#' @description
#' Prior to the shock, this will keep the
#' trajectory untouched, i.e., the trajectory follows baseline up until the
#' shock. After the shock hits, the development depends on whether or not
#' the company/ald_business_unit is already aligned and on what type of calculation
#' is selected for the shock. Overall the outcome should lead the
#' company/ald_business_unit to stay within the bounds of the carbon budget, if the
#' method applied is the overshoot/carbon budget method.
#'
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @param year_of_shock Numeric. A numeric vector of length 1 that contains the
#'   year in which the policy shock first hits.
#' @param duration_of_shock Numeric. A numeric vector of length 1 that contains
#'   the duration of the shock in years. I.e. the number of years it takes until
#'   the trajectory of the company/sector reaches a new equilibrium pathway.
#' @param shock_strength Numeric. A numeric vector that contains the shock
#'   size for the given company/ald_business_unit at hand, in case shock size is not
#'   calculated endogenously by using the overshoot/carbon budget method.
#'   TODO: (move to data argument)
#' @param scen_to_follow Numeric. A numeric vector that contains the production
#'   trajectory of the scenario indicated to use as the target for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param planned_prod Numeric vector that includes the production plans for a
#'   company or (aggregated) ald_business_unit to be included. The length of the vector
#'   for each company is from the start year of the analysis to the end year of
#'   the analysis, which means that in most cases, this vector will include NAs
#'   after the final forecast year. This usually comes from a PACTA analysis.
#'   TODO: (move to data argument)
#' @param late_sudden Numeric. A numeric vector that contains the
#'   late & sudden production trajectory for the company/ald_business_unit at hand.
#'   Before applying the shock, this follows the baseline scenario.
#'   TODO: (move to data argument)
#' @param scenario_change Numeric. A numeric vector that contains the
#'   absolute changes of the target scenario in yearly steps for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_baseline Numeric. A numeric vector that contains the
#'   absolute changes of the baseline scenario in yearly steps for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_aligned Numeric. A numeric vector that contains the
#'   absolute changes of the aligned target scenario in yearly steps for the
#'   company/ald_business_unit at hand, in case the company/ald_business_unit is aligned
#'   with the target after the forecast period.
#'   TODO: (move to data argument)
#' @param overshoot_direction Character. A character vector that indicates if
#'   the ald_business_unit at hand is increasing or decreasing over the time frame of
#'   the analysis.
#'   TODO: (move to data argument)
#'
#' @family scenario definition
#'
#' @return numeric vector
calc_late_sudden_traj <- function(start_year, end_year, year_of_shock,
                                  scen_to_follow, planned_prod, late_sudden,
                                  scenario_change, scenario_change_baseline, scenario_change_aligned,
                                  overshoot_direction, data) {
                                    
  # calculate the position where the shock kicks in
  position_shock_year <- year_of_shock - start_year + 1
  time_frame <- 5
  # get the NA indexes of values from last known planned prodcucion to the shock year
  na_range_to_shockyear <- which(is.na(planned_prod[1:position_shock_year]))

  if (length(na_range_to_shockyear) > 0) {
    # if this is true, then there are NA's in the period after the company prod
    # forecasts and the shock period
    # i.e. we need to fill the values between the last year we have production
    # forecasts, and the year of the shock
    # for example, if the shock year is 2026 and we have production forecasts
    # until 2024, we need to calculate L&S production for 2025 (we follow
    # baseline as it is the late & sudden scen)

    # first position for which future production before shock year is unknown
    first_production_na_sy <- na_range_to_shockyear[1]

    # Calculate the cumulative sum for the scenario_change_baseline
    # Update the late_and_sudden values
    late_sudden[first_production_na_sy:position_shock_year] <-
      late_sudden[first_production_na_sy - 1] +
      cumsum(scenario_change_baseline)[first_production_na_sy:position_shock_year]
  }

  # integral/overshoot compensation method
  # If the company production plans are already aligned
  # we do not need to compensate production capacity, and set LS trajectory to follow
  # the scenario indicated as late & sudden aligned
  if (
    (overshoot_direction == "Decreasing" & sum(scen_to_follow[1:time_frame + 1]) < sum(late_sudden[1:time_frame + 1])) |
      (overshoot_direction == "Increasing" & sum(scen_to_follow[1:time_frame + 1]) > sum(late_sudden[1:time_frame + 1]))
  ) {
    x <- (
      sum(scen_to_follow) -
        sum(late_sudden[1:(position_shock_year - 1)]) -
        (end_year - year_of_shock + 1) * late_sudden[position_shock_year - 1]
    ) /
      (
        -sum(seq(1, end_year - year_of_shock + 1))
      )

    # add the absolute production increase/decrease for each year during
    # the shock period, capping at a 0 lower bound for production volume
    sequence_length <- seq(position_shock_year, length(scen_to_follow))
    late_sudden[sequence_length] <- pmax(
      late_sudden[position_shock_year - 1] - (sequence_length - position_shock_year + 1) * x,
      0
    )
  } else {
    # company plans are already aligned
    # no need for overshoot in production cap, set LS trajectory to follow
    # the scenario indicated as late & sudden aligned
    # negative production adjustment: if shock production goes below 0
    # then this and future production stays constant at 0.

    first_production_na <- which(is.na(planned_prod))[1]

    # Calculate the cumulative sum starting from first_production_na
    cumulsum_change_aligned <- cumsum(scenario_change_aligned[first_production_na:length(scenario_change_aligned)])

    # Add the last non-NA value of late_sudden to the cumulative sum
    last_value_before_na <- late_sudden[first_production_na - 1]
    cumulsum_change_aligned <- last_value_before_na + cumulsum_change_aligned

    # Find the first index where cumulative sum becomes negative
    first_negative_index <- which(cumulsum_change_aligned < 0)[1]

    # If there is a negative value, set all subsequent values to 0
    if (!is.na(first_negative_index)) {
      cumulsum_change_aligned[first_negative_index:length(cumulsum_change_aligned)] <- 0
    }

    # Update late_sudden vector
    late_sudden[first_production_na:length(late_sudden)] <- cumulsum_change_aligned
  }

  return(late_sudden)
}

#' Remove negative late and sudden rows
#'
#' Function checks for negative values on variable late_and_sudden. All
#' ald_business_unit x company_name combinations holding >= 1 negative value are
#' removed.
#'
#' @param data A tibble containing scenario data with
#'   projected late and sudden trajectory.
#'
#' @return Input tibble with potentially removed rows.
filter_negative_late_and_sudden <- function(data) {
  data <- data %>%
    dplyr::mutate(
      late_sudden = pmax(.data$late_sudden, 0)
    )
  return(data)
}
