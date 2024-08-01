process_params <- function(fun, ...) {
  # Extract the parameters and their default values, replace by user inputs where applicable
  params <- formals(fun)
  default_params <- params[!sapply(params, is.symbol)]
  args <- list(...)
  final_params <- modifyList(default_params, args)
  return(final_params)
}

write_results <- function(output_list, output_path, trisk_params, show_params_cols) {
  # create a random uuid as the run_id column
  run_id <- uuid::UUIDgenerate()

  # Prepare results
  npv_results <- prepare_npv_results(output_list)
  pd_results <- prepare_pd_results(output_list)
  company_trajectories <- prepare_company_trajectories(output_list)
  params_df <- prepare_params_df(trisk_params, run_id)


  if (show_params_cols) {
    npv_results <- npv_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(npv_results)), ])

    pd_results <- pd_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(pd_results)), ])

    company_trajectories <- company_trajectories %>%
      dplyr::bind_cols(params_df[rep(1, nrow(company_trajectories)), ])
  }

  # Create output folder
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_path <- fs::path(output_path, paste0(timestamp, "__", run_id))
  dir.create(output_path, recursive = TRUE)

  # Save results
  npv_results %>% readr::write_csv(fs::path(output_path, "npv_results.csv"))
  pd_results %>% readr::write_csv(fs::path(output_path, "pd_results.csv"))
  company_trajectories %>% readr::write_csv(fs::path(output_path, "company_trajectories.csv"))
  params_df %>% readr::write_csv(fs::path(output_path, "params.csv"))

  print(paste("Outputs saved in folder:", output_path))
}

prepare_params_df <- function(trisk_params, run_id) {
  params_df <- tibble::as_tibble(trisk_params) %>%
    dplyr::mutate(run_id = .env$run_id)
  return(params_df)
}

prepare_npv_results <- function(output_list) {
  npv_results <- output_list$company_technology_npv %>%
    dplyr::rename(
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls,
    ) %>%
    dplyr::select(
      .data$company_id,
      .data$ald_sector,
      .data$technology,
      .data$net_present_value_baseline,
      .data$net_present_value_shock,
    )
  return(npv_results)
}

prepare_pd_results <- function(output_list) {
  pd_results <- output_list$company_pd_changes_overall %>%
    dplyr::rename(
      pd_baseline = .data$PD_baseline,
      pd_shock = .data$PD_late_sudden
    ) %>%
    dplyr::select(
      .data$company_id,
      .data$ald_sector,
      .data$term,
      .data$pd_baseline,
      .data$pd_shock,
    )
  return(pd_results)
}


prepare_company_trajectories <- function(output_list) {
  company_trajectories <- output_list$company_trajectories %>%
    dplyr::rename(
      company_id = .data$company_id,
      production_baseline_scenario = .data$production_asset_baseline,
      production_target_scenario = .data$production_scenario_target,
      production_shock_scenario = .data$late_sudden,
      price_baseline = .data$price_baseline,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    ) %>%
    dplyr::select(
      .data$company_name, .data$year,
      .data$ald_sector, .data$technology,
      .data$production_plan_company_technology, .data$production_baseline_scenario,
      .data$production_target_scenario, .data$production_shock_scenario, .data$company_id,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility, .data$price_baseline, .data$price_shock_scenario,
      .data$net_profits_baseline_scenario, .data$net_profits_shock_scenario,
      .data$discounted_net_profits_baseline_scenario, .data$discounted_net_profits_shock_scenario,
    )
  return(company_trajectories)
}
