#' Run stress testing for provided asset type
#'
#' This function executes the transition risk stress test. It reads data from
#' the input path, runs the transition risk model using the provided parameters,
#' and writes results to the output path if provided.
#'
#' @param input_path String containing the path to project-agnostic data.
#' @param output_path String containing the path to which output files are written.
#'   If NULL, the function returns the results list. Results and logs per run are saved
#'   to a subdirectory of `output_path` that will be generated automatically with a
#'   timestamp.
#' @param show_params_cols Logical indicating whether parameter columns should be shown
#'   in the output. Default is TRUE.
#' @param ... Additional arguments passed to \code{\link{run_trisk_model}} (clic for parameters detail).
#'
#' @return Character, folder path containing output dataframes
#' @export
#'
run_trisk <- function(
    input_path,
    output_path,
    show_params_cols = TRUE,
    ...) {
  input_data_list <- st_read_agnostic(input_path)

  run_id <- uuid::UUIDgenerate()

  output_list <- run_trisk_model(
    assets_data = input_data_list$assets_data,
    scenarios_data = input_data_list$scenarios_data,
    financial_data = input_data_list$financial_data,
    carbon_data = input_data_list$carbon_data,
    run_id = run_id,
    ...
  )

  check_results <- function(output_list) {
    TRUE
  } # TODO
  stopifnot(check_results(output_list))

  trisk_params <- process_params(fun = run_trisk_model, ...)

  results_output_path <- write_results(
    npv_results = output_list$npv_results,
    pd_results = output_list$pd_results,
    company_trajectories = output_list$company_trajectories,
    trisk_params = trisk_params,
    run_id = run_id,
    output_path = output_path,
    show_params_cols = show_params_cols
  )

  return(results_output_path)
}



#' Run transition risk stress test model
#'
#' This function executes the core transition risk stress test model calculations.
#'
#' @param assets_data Data frame containing asset information.
#' @param scenarios_data Data frame containing scenario information.
#' @param financial_data Data frame containing financial information.
#' @param carbon_data Data frame containing carbon price information.
#' @param baseline_scenario String specifying the name of the baseline scenario.
#' @param target_scenario String specifying the name of the shock scenario.
#' @param scenario_geography Character vector indicating which geographical region(s) to calculate results for.
#' @param carbon_price_model Character vector specifying which NGFS model to use for carbon prices. Default is "no_carbon_tax".
#' @param risk_free_rate Numeric value for the risk-free interest rate. Default is 0.02.
#' @param discount_rate Numeric value for the discount rate of dividends per year in the DCF. Default is 0.07.
#' @param growth_rate Numeric value for the terminal growth rate of profits beyond the final year in the DCF. Default is 0.03.
#' @param div_netprofit_prop_coef Numeric coefficient determining how strongly future dividends propagate to company value. Default is 1.
#' @param shock_year Numeric value specifying the year when the shock is applied. Default is 2030.
#' @param market_passthrough Numeric value representing the firm's ability to pass carbon tax onto the consumer. Default is 0.
#' @param run_id (Optional) Character value representing the ID of current Trisk's iteration
#'
#' @return A list containing:
#'   \item{pd_results}{Data frame of overall probability of default changes}
#'   \item{company_trajectories}{Data frame of company annual profits}
#'   \item{npv_results}{Data frame of company technology net present values}
#' @export
#'
run_trisk_model <- function(assets_data,
                            scenarios_data,
                            financial_data,
                            carbon_data,
                            baseline_scenario = "",
                            target_scenario = "",
                            scenario_geography = "Global",
                            carbon_price_model = "no_carbon_tax",
                            risk_free_rate = 0.02,
                            discount_rate = 0.07,
                            growth_rate = 0.03,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            market_passthrough = 0,
                            run_id = NULL) {
  if (is.null(run_id)) {
    run_id <- uuid::UUIDgenerate()
  }

  cat("-- Processing Assets and Scenarios. \n")

  processed_assets_data <- process_assets_data(assets_data = assets_data, financial_data = financial_data)
  scenarios_data <- process_scenarios_data(scenarios_data = scenarios_data, baseline_scenario = baseline_scenario, target_scenario = target_scenario, scenario_geography = scenario_geography)

  cat("-- Transforming to Trisk model input. \n")

  assets_scenarios <- merge_assets_and_scenarios_data(assets_data = processed_assets_data, scenarios_data = scenarios_data)

  trisk_model_input <- process_trisk_input(
    assets_scenarios = assets_scenarios,
    target_scenario = target_scenario
  )

  start_year <- min(trisk_model_input$year)
  end_analysis <- max(trisk_model_input$year)

  cat("-- Calculating baseline, target, and shock trajectories. \n")

  trisk_model_output <- extend_assets_trajectories(
    trisk_model_input = trisk_model_input,
    start_year = start_year,
    shock_year = shock_year
  )

  cat("-- Calculating net profits. \n")

  processed_carbon_data <- process_carbon_data(
    carbon_data,
    start_year = start_year,
    end_year = end_analysis,
    carbon_price_model = carbon_price_model
  )

  # calc net profits
  company_net_profits <- calculate_net_profits(
    trisk_model_output,
    carbon_data = processed_carbon_data,
    shock_year = shock_year,
    market_passthrough = market_passthrough
  )

  # calc discounted net profits
  company_annual_profits <- calculate_annual_profits(
    data = company_net_profits,
    baseline_scenario = baseline_scenario,
    shock_scenario = target_scenario,
    end_year = end_analysis,
    discount_rate = discount_rate,
    growth_rate = growth_rate
  )

  cat("-- Calculating market risk. \n")

  company_technology_npv <- company_annual_profits %>%
    calculate_asset_value_at_risk(
      shock_year = shock_year,
      start_year = start_year,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      crispy = TRUE
    )

  cat("-- Calculating credit risk. \n")

  company_pd_changes_overall <- company_annual_profits %>%
    calculate_pd_change_overall(
      shock_year = shock_year,
      end_of_analysis = end_analysis,
      risk_free_interest_rate = risk_free_rate
    )


  npv_results <- tibble::as_tibble(prepare_npv_results(company_technology_npv = company_technology_npv, run_id = run_id))
  pd_results <- tibble::as_tibble(prepare_pd_results(company_pd_changes_overall = company_pd_changes_overall, run_id = run_id))
  company_trajectories_results <- tibble::as_tibble(prepare_company_trajectories(company_trajectories = company_annual_profits, run_id = run_id))

  return(
    list(
      npv_results = npv_results,
      pd_results = pd_results,
      company_trajectories = company_trajectories_results
    )
  )
}
