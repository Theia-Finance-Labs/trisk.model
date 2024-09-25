process_params <- function(fun, ...) {
  # Extract the parameters and their default values, replace by user inputs where applicable
  params <- formals(fun)
  default_params <- params[!sapply(params, is.symbol)]
  args <- list(...)
  final_params <- utils::modifyList(default_params, args)
  return(final_params)
}

#' Prepare TRISK Results
#'
#' This function prepares a list of results from a TRISK model run, including net present value (NPV), 
#' probability of default (PD), company trajectories, and model parameters. It converts these results into tibbles for further analysis.
#'
#' @param output_list A list containing the output data from the TRISK model run.
#' @param trisk_params A data structure (e.g., list or tibble) containing the TRISK model parameters.
#' @param run_id A unique identifier for the model run.
#'
#' @return A list of tibbles containing the following elements:
#' \describe{
#'   \item{npv}{A tibble with the NPV results of the TRISK model run.}
#'   \item{pd}{A tibble with the PD results of the TRISK model run.}
#'   \item{trajectories}{A tibble with the company trajectory results.}
#'   \item{params}{A tibble with the model parameters used in the run.}
#' }
#' 
#' @export
prepare_trisk_results <- function(output_list, trisk_params, run_id){
    list(
      npv = tibble::as_tibble(trisk.model:::prepare_npv_results(output_list, run_id)),
      pd = tibble::as_tibble(trisk.model:::prepare_pd_results(output_list, run_id)),
      trajectories = tibble::as_tibble(trisk.model:::prepare_company_trajectories(output_list, run_id)),
      params = tibble::as_tibble(trisk.model:::prepare_params_df(trisk_params, run_id))
  )
}

write_results <- function(output_list, output_path, trisk_params, show_params_cols) {
  # create a random uuid as the run_id column
  run_id <- uuid::UUIDgenerate()

  prepared_trisk_results <- prepare_trisk_results(output_list=output_list, trisk_params=trisk_params, run_id=run_id)

  # Prepare results
  npv_results <- prepared_trisk_results$npv
  pd_results <- prepared_trisk_results$pd
  company_trajectories <- prepared_trisk_results$trajectories
  params_df <- prepared_trisk_results$params


  if (show_params_cols) {
    npv_results <- npv_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(npv_results)), ] %>% dplyr::select(-c(.data$run_id)))

    pd_results <- pd_results %>%
      dplyr::bind_cols(params_df[rep(1, nrow(pd_results)), ] %>% dplyr::select(-c(.data$run_id)))

    company_trajectories <- company_trajectories %>%
      dplyr::bind_cols(params_df[rep(1, nrow(company_trajectories)), ] %>% dplyr::select(-c(.data$run_id)))
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

prepare_npv_results <- function(output_list, run_id) {
  npv_results <- output_list$company_technology_npv %>%
    dplyr::mutate(run_id = .env$run_id) %>%
    dplyr::rename(
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls,
    ) %>%
    dplyr::select(
      .data$run_id,
      .data$company_id,
      .data$asset_id,
      .data$company_name,
      .data$asset_name,
      .data$sector,
      .data$technology,
      .data$net_present_value_baseline,
      .data$net_present_value_shock,
    )
  return(npv_results)
}

prepare_pd_results <- function(output_list, run_id) {
  pd_results <- output_list$company_pd_changes_overall %>%
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


prepare_company_trajectories <- function(output_list, run_id) {
  company_trajectories <- output_list$company_trajectories %>%
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
