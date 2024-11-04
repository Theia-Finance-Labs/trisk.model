#' Process function parameters
#'
#' This function extracts the default parameters from a function and replaces them with user-provided values where applicable.
#' /!\ It ignore params without default, or with a NULL default
#'
#' @param fun A function whose parameters are to be processed.
#' @param ... User-defined arguments to override the function's default parameters.
#' @return A list of final parameters.
#' @export
process_params <- function(fun, ...) {
  params <- formals(fun)
  # Ignore params without default, or with a NULL default
  default_params <- params[!sapply(params, is.symbol) & !sapply(params, is.null)]
  args <- list(...)
  final_params <- utils::modifyList(default_params, args)
  return(final_params)
}

write_results <- function(npv_results, pd_results, company_trajectories, trisk_params, run_id, output_path, show_params_cols) {
  params_df <- tibble::as_tibble(trisk_params)

  if (show_params_cols) {
    npv_results <- npv_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(npv_results)), ])

    pd_results <- pd_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(pd_results)), ])

    company_trajectories <- company_trajectories %>%
      dplyr::bind_cols(params_df[rep(1, nrow(company_trajectories)), ])
  }

  # Adds the run_id column to params_df
  params_df <- params_df %>%
    dplyr::mutate(run_id = .env$run_id)


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

  return(output_path)
}

prepare_npv_results <- function(company_technology_npv, trisk_model_input, run_id) {
  
  # Adds the country column to the output
  asset_countries <- trisk_model_input %>% 
    dplyr::distinct(.data$asset_id, .data$country_iso2)

  npv_results <- company_technology_npv %>%
    dplyr::inner_join(asset_countries, by=c("asset_id")) %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::rename(
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls
    ) %>%
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      net_present_value_change = .data$net_present_value_difference / .data$net_present_value_baseline
    )%>%
    dplyr::select(
      .data$run_id,
      .data$company_id,
      .data$asset_id,
      .data$company_name,
      .data$asset_name,
      .data$sector,
      .data$technology,
      .data$country_iso2,
      .data$net_present_value_baseline,
      .data$net_present_value_shock,
      .data$net_present_value_difference,
      .data$net_present_value_change
    )
  return(npv_results)
}

prepare_pd_results <- function(company_pd_changes_overall, run_id) {
  pd_results <- company_pd_changes_overall %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::rename(
      pd_baseline = .data$PD_baseline,
      pd_shock = .data$PD_late_sudden
    ) %>%
    dplyr::select(
      .data$run_id,
      .data$company_id,
      .data$company_name,
      .data$sector,
      .data$term,
      .data$pd_baseline,
      .data$pd_shock,
    )
  return(pd_results)
}


prepare_company_trajectories <- function(company_trajectories, run_id) {
  company_trajectories <- company_trajectories %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::rename(
      production_baseline_scenario = .data$production_asset_baseline,
      production_target_scenario = .data$production_scenario_target,
      production_shock_scenario = .data$late_sudden,
      scenario_price_baseline = .data$scenario_price_baseline,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    ) %>%
    dplyr::select(
      .data$run_id, .data$asset_id, .data$asset_name, .data$company_id, .data$company_name, .data$year,
      .data$sector, .data$technology,
      .data$production_plan_company_technology, .data$production_baseline_scenario,
      .data$production_target_scenario, .data$production_shock_scenario, .data$company_id,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility, .data$scenario_price_baseline, .data$price_shock_scenario,
      .data$net_profits_baseline_scenario, .data$net_profits_shock_scenario,
      .data$discounted_net_profits_baseline_scenario, .data$discounted_net_profits_shock_scenario,
    )
  return(company_trajectories)
}
