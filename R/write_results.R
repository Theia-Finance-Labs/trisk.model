write_results <- function(output_list, output_path, show_params_cols) {
  # create a random uuid as the run_id column
  run_id <- uuid::UUIDgenerate()

  output_path <- fs::path(output_path, run_id)
  dir.create(output_path, recursive = TRUE)

  npv_results <- prepare_npv_results(output_list, output_path, show_params_cols, run_id)
  pd_results <- prepare_pd_results(output_list, output_path, show_params_cols, run_id)
  company_trajectories <- prepare_company_trajectories(output_list, output_path, show_params_cols, run_id)

  npv_results %>% readr::write_csv(fs::path(output_path, "npv_results.csv"))
  pd_results %>% readr::write_csv(fs::path(output_path, "pd_results.csv"))
  company_trajectories %>% readr::write_csv(fs::path(output_path, "company_trajectories.csv"))
}

prepare_npv_results <- function(output_list, output_path, show_params_cols, run_id) {
  npv_results <- output_list$company_technology_npv %>%
    dplyr::rename(
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls,
    ) %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::select(
      .data$run_id,
      .data$company_id,
      .data$ald_sector,
      .data$ald_business_unit,
      .data$net_present_value_baseline,
      .data$net_present_value_shock,
    )
  return(npv_results)
}

prepare_pd_results <- function(output_list, output_path, show_params_cols, run_id) {
  pd_results <- output_list$company_pd_changes_overall %>%
    dplyr::rename(
      pd_baseline = .data$PD_baseline,
      pd_shock = .data$PD_late_sudden
    ) %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::select(
      .data$run_id,
      .data$company_id,
      .data$ald_sector,
      .data$term,
      .data$pd_baseline,
      .data$pd_shock,
    )
  return(pd_results)
}

prepare_company_trajectories <- function(output_list, output_path, show_params_cols, run_id) {
  company_trajectories <- output_list$company_trajectories %>%
    dplyr::rename(
      company_id = .data$company_id,
      production_baseline_scenario = .data$baseline,
      production_target_scenario = .data$scen_to_follow_aligned,
      production_shock_scenario = .data$late_sudden,
      price_baseline_scenario = .data$Baseline_price,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    ) %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::select(
      .data$run_id, .data$company_name, .data$year,
      .data$scenario_geography, .data$ald_sector, .data$ald_business_unit,
      .data$plan_tech_prod, .data$phase_out, .data$production_baseline_scenario,
      .data$production_target_scenario, .data$production_shock_scenario, .data$company_id,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility, .data$price_baseline_scenario, .data$price_shock_scenario,
      .data$net_profits_baseline_scenario, .data$net_profits_shock_scenario,
      .data$discounted_net_profits_baseline_scenario, .data$discounted_net_profits_shock_scenario,
    )
  return(company_trajectories)
}
