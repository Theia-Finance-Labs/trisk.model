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
#' @param return_results Boolean, indicating if results shall be exported.
#'
#' @inheritParams run_trisk_model
run_trisk <- function(input_path, output_path,
                      return_results = FALSE, ...) {
  
  input_data_list <- st_read_agnostic(input_path)

  input_data_list <- input_data_list %>%
    st_process(...
      # scenario_geography = scenario_geography,
      # baseline_scenario = baseline_scenario,
      # shock_scenario = shock_scenario,
      # start_year = start_year,
      # carbon_price_model = carbon_price_model
      )

  output_list <- run_trisk_model(input_data_list=input_data_list, ...)
  company_pd_changes_overall <- output_list$company_pd_changes_overall
  company_trajectories <- output_list$company_annual_profits
  company_technology_npv <- output_list$company_technology_npv

  write_results(output_list, output_path) # TODO

  if (return_results) {
    return(output_list)
  }
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
#' @param financial_stimulus Additional support for low carbon companies.
#'
#' @return NULL
#' @export
run_trisk_model <- function(input_data_list,
                            baseline_scenario = "WEO2021_STEPS",
                            shock_scenario = "WEO2021_SDS",
                            lgd = 0.45,
                            risk_free_rate = 0.02,
                            discount_rate = 0.07,
                            growth_rate = 0.03,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            scenario_geography = "Global",
                            start_year = 2022,
                            carbon_price_model = "no_carbon_tax",
                            market_passthrough = 0,
                            financial_stimulus = 1) {
  # input_data_list <- preprocess_data() # TODO


  cat("-- Calculating production trajectory under trisk shock. \n")


  input_data_list$full_trajectory <- calculate_trisk_trajectory(
    input_data_list = input_data_list,
    baseline_scenario = baseline_scenario,
    target_scenario = shock_scenario,
    start_year = start_year,
    shock_year=shock_year,
    end_year = end_year,
    time_horizon = time_horizon_lookup
  )

  cat("-- Calculating net profits. \n")

  # calc net profits
  company_net_profits <- calculate_net_profits(input_data_list$full_trajectory,
    carbon_data = input_data_list$carbon_data,
    shock_year = shock_year,
    market_passthrough = market_passthrough,
    financial_stimulus = financial_stimulus
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
      shock_year=shock_year,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      flat_multiplier = flat_multiplier_lookup,
      crispy = TRUE
    )

  cat("-- Calculating credit risk. \n\n\n")

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
