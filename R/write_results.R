write_results <- function(output_list, output_path, show_params_cols){
    browser()
    write_stress_test_results(output_list, output_path, show_params_cols)
    write_merton_results(output_list, output_path, show_params_cols)
    write_company_trajectories(output_list, output_path, show_params_cols)
}

write_stress_test_results <- function(output_list, output_path, show_params_cols){

}

write_merton_results <- function(output_list, output_path, show_params_cols){

}

write_company_trajectories <- function(output_list, output_path, show_params_cols){

}


#' Fill missing values on annual_profits
#'
#' Function fill missing rows on cols company_id, pd, net_profit_margin,
#' debt_equity_ratio, volatility.
#'
#' @param annual_profits A tibble holding annual profit data.
#'
#' @return Tibble holding `annual profits` with replaces missings.
fill_annual_profit_cols <- function(annual_profits) {
  annual_profits_filled <- annual_profits %>%
    dplyr::arrange(
      .data$scenario_name, .data$scenario_geography, .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit, .data$year
    ) %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit
    ) %>%
    # NOTE: this assumes emissions factors stay constant after forecast and prod not continued
    tidyr::fill(
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio, .data$volatility,
      .direction = "down"
    ) %>%
    dplyr::ungroup()

  return(annual_profits_filled)
}

#' Wrangle results
#'
#' Function wrangles results to expected formats. List element entry `results`
#' is split into market risk results for company and portfolio level.
#'
#' @param results_list A list of results.
#' @param sensitivity_analysis_vars  String vector holding names of iteration
#'   arguments.
#' @param risk_type String that is either lrisk or trisk.
#'
#' @return A list of wrangled results.
wrangle_results <- function(results_list, sensitivity_analysis_vars, risk_type) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

company_trajectories <- results_list$company_trajectories %>%
    dplyr::select(
    .data$scenario_name, .data$company_name, .data$year,
    .data$scenario_geography, .data$ald_sector, .data$ald_business_unit,
    .data$plan_tech_prod, .data$phase_out, .data$baseline,
    .data$scen_to_follow_aligned, .data$late_sudden, .data$company_id,
    .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
    .data$volatility, .data$Baseline_price, .data$late_sudden_price,
    .data$net_profits_baseline, .data$net_profits_ls,
    .data$discounted_net_profit_baseline, .data$discounted_net_profit_ls,
    !!!rlang::syms(sensitivity_analysis_vars)
    )

  company_trajectories <- company_trajectories %>%
    dplyr::rename(
      company_id = .data$company_id,
      production_plan_company_technology = .data$plan_tech_prod,
      # TODO: add once ADO3530 is merged
      # direction_of_target = .data$direction,
      production_baseline_scenario = .data$baseline,
      production_target_scenario = .data$scen_to_follow_aligned,
      production_shock_scenario = .data$late_sudden,
      price_baseline_scenario = .data$Baseline_price,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    )

  # Crispy Results -----
  merge_by_cols <- c(
    "company_id", "ald_sector", "scenario_name", "scenario_geography", sensitivity_analysis_vars
  )

  crispy_output <- results_list$company_technology_npv %>%
    dplyr::inner_join(
      results_list$company_pd_changes_overall,
      by = merge_by_cols
    )


  if (risk_type == "lrisk") {
    select_cols <- c(merge_by_cols, "ald_business_unit", "company_is_litigated", "settlement")
    crispy_output <- crispy_output %>%
      dplyr::inner_join(
        results_list$company_trajectories %>%
          dplyr::select(!!select_cols) %>%
          dplyr::distinct_all(),
        by = c(merge_by_cols, "ald_business_unit") # inlcuding since settlement is a ald_business_unit level variable
      )

    crispy_output <- crispy_output %>%
      dplyr::rename(
        scc = .data$scc_arg,
        settlement_factor = .data$settlement_factor_arg,
        exp_share_damages_paid = .data$exp_share_damages_paid_arg
      )
  } else {
    crispy_output <- crispy_output %>%
      dplyr::rename(
        carbon_price_model = .data$carbon_price_model_arg,
        market_passthrough = .data$market_passthrough_arg,
        financial_stimulus = .data$financial_stimulus_arg
      )
  }

  crispy_output <- crispy_output %>%
    dplyr::rename(
      ald_sector = .data$ald_sector,
      ald_business_unit = .data$ald_business_unit,
      baseline_scenario = .data$baseline_scenario_arg,
      shock_scenario = .data$shock_scenario_arg,
      lgd = .data$lgd_arg,
      discount_rate = .data$discount_rate_arg,
      div_netprofit_prop_coef = .data$div_netprofit_prop_coef_arg,
      growth_rate = .data$growth_rate_arg,
      shock_year = .data$shock_year_arg,
      start_year = .data$start_year_arg,
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls,
      pd_baseline = .data$PD_baseline,
      pd_shock = .data$PD_late_sudden
    ) %>%
    dplyr::mutate(roll_up_type = "equity_ownership") %>%
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      net_present_value_roc = .data$net_present_value_shock / .data$net_present_value_baseline - 1,
      pd_difference = .data$pd_shock - .data$pd_baseline
    )

  if (risk_type == "lrisk") {
    crispy_output <- crispy_output %>%
      dplyr::select(
        .data$company_name, .data$ald_sector, .data$ald_business_unit,
        .data$roll_up_type, .data$scenario_geography,
        .data$baseline_scenario, .data$shock_scenario, .data$lgd,
        .data$risk_free_rate, .data$discount_rate, .data$div_netprofit_prop_coef,
        .data$growth_rate, .data$scc, .data$settlement_factor,
        .data$exp_share_damages_paid, .data$shock_year, .data$start_year,
        .data$net_present_value_baseline,
        .data$net_present_value_shock, .data$net_present_value_difference,
        .data$term, .data$pd_baseline, .data$pd_shock, .data$pd_difference,
        .data$company_is_litigated, .data$settlement
      )
  } else {
    crispy_output <- crispy_output %>%
      dplyr::select(
        .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
        .data$roll_up_type, .data$scenario_geography,
        .data$baseline_scenario, .data$shock_scenario, .data$lgd,
        .data$risk_free_rate, .data$discount_rate, .data$div_netprofit_prop_coef,
        .data$carbon_price_model, .data$market_passthrough,
        .data$financial_stimulus, .data$start_year,
        .data$growth_rate, .data$shock_year, .data$net_present_value_baseline,
        .data$net_present_value_shock, .data$net_present_value_difference, .data$net_present_value_roc,
        .data$term, .data$pd_baseline, .data$pd_shock, .data$pd_difference
      )
  }

  # add a random uuid as the run_id
  run_id <- uuid::UUIDgenerate()
  company_trajectories <- company_trajectories %>%
    dplyr::mutate(run_id = .env$run_id)
  crispy_output <- crispy_output %>%
    dplyr::mutate(run_id = .env$run_id)

  return(list(
    company_trajectories = company_trajectories,
    crispy_output = crispy_output
  ))
}

#' Check results
#'
#' Function checks results for missings and duplicates.
#'
#' @inheritParams wrangle_results
#' @param wrangled_results_list A list of wrangled results.
#' @param risk_type String that is either lrisk or trisk.
#'
#' @return `wrangled_results_list`
check_results <- function(wrangled_results_list, sensitivity_analysis_vars, risk_type) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  # company trajectories ----------------------------------------------------
  wrangled_results_list$company_trajectories %>%
    # ADO 3112 - the last year contains the terminal value, which has no
    # production values hence that year contains multiple NAs and is ignored
    # here. Since this also affects the number of rows, we exclude the terminal
    # value year in the check for expected missingness already
    dplyr::filter(.data$year != max(.data$year, na.rm = TRUE)) %>%
    check_expected_missings() %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "company_name", "year", "scenario_geography", "ald_sector", "ald_business_unit",
        sensitivity_analysis_vars
      )
    ) %>%
    # not considering those two variables when checking for missings because
    # acceptable missing pattern is checked in ADO 4919
    dplyr::select(-.data$production_plan_company_technology) %>%
    report_missings(
      name_data = "Company Trajectories"
    )

  # crispy results ----------------------------------------------------

  composite_unique_cols_crispy_results <- c(
    "company_name", "ald_sector", "ald_business_unit", "roll_up_type",
    "scenario_geography", "baseline_scenario", "shock_scenario", "lgd",
    "risk_free_rate", "discount_rate", "div_netprofit_prop_coef", "growth_rate",
    "shock_year", "term"
  )

  if (risk_type == "lrisk") {
    composite_unique_cols_lrisk <- c("scc", "settlement_factor", "exp_share_damages_paid")
    composite_unique_cols_crispy_results <- c(composite_unique_cols_crispy_results, composite_unique_cols_lrisk)
  }

  wrangled_results_list$crispy_output %>%
    report_missings(
      name_data = "CRISPY Results"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = composite_unique_cols_crispy_results
    )

  return(invisible(wrangled_results_list))
}
