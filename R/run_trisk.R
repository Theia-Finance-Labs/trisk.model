#' Run stress testing for provided asset type.
#'
#' This function runs the transition risk stress test. It can be desirable to
#' understand sensitivities of the scenarios, in which case the user may pass a
#' vector of values to one (and only one) of the detail arguments. This will
#' result in running the analysis multiple times in a row with the argument
#' varied.
#' NOTE: if `return_results` is TRUE results will not be written to `output
#' path` but instead are returned.
#'
#' @param input_path String holding path to project agnostic data.
#' @param output_path String holding path to which output files are written.
#'   NOTE: Results and logs per run are saved to a subdirectory of output_path
#'   that will be generated automatically. The name of the subdirectory is the
#'   timestamp of the run of the analysis.
#' @param save_and_check Boolean, indicating if results shall be exported.
#'
#' @inheritParams run_trisk_model
run_trisk <- function(
    input_path,
    output_path = NULL,
    save_and_check = TRUE,
    show_params_cols = TRUE,
    ...) {
  # stopifnot(!((save_and_check & !is.null(output_path)) | !save_and_check),
  #  "Either output_path arg must be set, or save_and_check set to TRUE")

  input_data_list <- st_read_agnostic(input_path)

  output_list <- run_trisk_model(input_data_list = input_data_list, ...)

  if (save_and_check) {
    check_results <- function(output_list) {
      TRUE
    } # TODO
    stopifnot(check_results(output_list))

    trisk_params <- process_params(fun = run_trisk_model, ...)

    write_results(output_list = output_list, output_path = output_path, trisk_params = trisk_params, show_params_cols = show_params_cols)
  }
  return(output_list)
}



#' Run stress test model
#'
#' @param baseline_scenario Holds the name of the baseline scenario to be used
#'   in the stress test, for accepted value range check `stress_test_arguments`.
#' @param shock_scenario Holds the name of the shock scenario to be used in the
#'   stress test, for accepted value range check `stress_test_arguments`.
#' @param lgd Numeric, holding the loss given default for accepted value range
#'   check `stress_test_arguments`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `stress_test_arguments`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `stress_test_arguments`.
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the final year in the DCF. For accepted range compare
#'   `stress_test_arguments`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `stress_test_arguments`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `stress_test_arguments`.
#' @param scenario_geography Character vector, indicating which geographical
#'   region(s) (concerning asset location) results shall be calculated for. For
#'   accepted values compare `stress_test_arguments`.
#' @param start_year Numeric, first year in the analysis used as the starting
#'   point from which production forecasts are compared against scenario targets.
#'   Must be available in the production data and indicates the first year of
#'   the scenario data.
#' @param carbon_price_model Character vector, indicating which NGFS model is used in regards to
#'   carbon prices. Default is no carbon tax.
#' @param market_passthrough Firm's ability to pass carbon tax onto the consumer
#'
#' @return NULL
#' @export
run_trisk_model <- function(input_data_list,
                            baseline_scenario,
                            shock_scenario,
                            scenario_geography,
                            start_year = 2022,
                            carbon_price_model = "no_carbon_tax",
                            lgd = 0.45,
                            risk_free_rate = 0.02,
                            discount_rate = 0.07,
                            growth_rate = 0.03,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            market_passthrough = 0) {

  cat("-- Processing inputs. \n")                              
  # TODO remove MAX_POSSIBLE_YEAR
  end_analysis <- get_end_year(data, c(baseline_scenario, target_scenario), MAX_POSSIBLE_YEAR = 2050)

  end_analysis <- min(data$production_data$year)

  assets_data <- process_assets_data(data=input_data_list, start_analysis = start_analysis, end_analysis = end_analysis)
  scenarios_data <- process_scenarios_data(data = input_data_list, start_analysis = start_analysis, end_analysis = end_analysis, baseline_scenario = baseline_scenario, target_scenario = target_scenario, scenario_geography = scenario_geography)

  outputs <- process_trisk_input(
      assets_data = assets_data, scenarios_data = scenarios_data,
      target_scenario = target_scenario, start_analysis = start_analysis
    )
  trisk_model_data <- outputs$input_data_list
  end_year <- outputs$end_year

  carbon_data <- process_carbon_data(
    input_data_list$carbon_data,
    start_year = start_year,
    end_year = end_year,
    carbon_price_model = carbon_price_model
  )

  cat("-- Calculating baseline and shock trajectories. \n")

  trisk_trajectory <- calculate_trajectories(
    trisk_model_data = trisk_model_data,
    baseline_scenario = baseline_scenario,
    target_scenario = shock_scenario,
    start_year = start_year,
    shock_year = shock_year,
    end_year = end_year
  )


  cat("-- Calculating net profits. \n")

  # calc net profits
  company_net_profits <- calculate_net_profits(
    input_data_list$full_trajectory,
    carbon_data = input_data_list$carbon_data,
    shock_year = shock_year,
    market_passthrough = market_passthrough
  )

  # calc discounted net profits
  company_annual_profits <- calculate_annual_profits(
    data = company_net_profits,
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    end_year = end_year,
    discount_rate = discount_rate,
    growth_rate = growth_rate
  )

  cat("-- Calculating market risk. \n")

  company_technology_npv <- company_annual_profits %>%
    company_technology_asset_value_at_risk(
      shock_year = shock_year,
      start_year = start_year,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      crispy = TRUE
    )

  cat("-- Calculating credit risk. \n")

  company_pd_changes_overall <- company_annual_profits %>%
    calculate_pd_change_overall(
      shock_year = shock_year,
      end_of_analysis = end_year,
      risk_free_interest_rate = risk_free_rate
    )

  return(
    list(
      company_pd_changes_overall = company_pd_changes_overall,
      company_trajectories = company_annual_profits,
      company_technology_npv = company_technology_npv
    )
  )
}
