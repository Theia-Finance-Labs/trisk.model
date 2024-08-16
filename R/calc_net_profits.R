#' Calculates annual net profits on the company-technology level for the
#' baseline and late and sudden scenarios. Climate laggards which need to build
#' out their production in increasing technologies to compensate for their
#' missed targets, are "punished" by adjusting the net profit margin on their
#' additional build out based on their proximity to target within the given
#' technology. Specifically, we measure the ratio of how much of the required
#' build out or reduction in a technology the company will have done at the end
#' of the forecast period. If the technology has an increasing target and the
#' ratio of completion is below one, the net_profit_margin on the additional
#' production build out is multiplied with the proximity to the target. This
#' approximates the additional capital investment such a company would have to
#' make in a short time, which leads to added costs. This ensures that late
#' build out will not proportionally translate into increased profits.
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the technology.
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario.
#' @param market_passthrough A firm's ability to pass a carbon tax onto the consumer.
#' companies can be boosted under a shock scenario.
calculate_net_profits <- function(data,
                                  shock_year,
                                  market_passthrough) {
                                    
  baseline <- calculate_net_profits_baseline(data) %>%
    dplyr::select(
      .data$company_id,
      .data$asset_id,
      .data$year,
      .data$sector,
      .data$technology,
      .data$net_profits_baseline
    )

  shock_increasing_technologies <- calculate_net_profits_shock_increasing_technologies(
    data = data %>% dplyr::filter(.data$technology_type == "greentech"),
    shock_year = shock_year
  )

  shock_declining_technologies <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data %>% dplyr::filter(.data$technology_type == "carbontech"),
    shock_year = shock_year,
    market_passthrough = market_passthrough
  )

  data <- dplyr::bind_rows(shock_increasing_technologies, shock_declining_technologies)

  data <- dplyr::full_join(
    data,
    baseline,
    by = c(
      "company_id",
      "asset_id",
      "year",
      "sector",
      "technology"
    )
  )

  return(data)
}


#' Calculates annual net profits on the company-technology level for the baseline scenario
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the technology.
#'
#' @return A data frame with net profits for the baseline scenario.

calculate_net_profits_baseline <- function(data) {
  data <- data %>%
    dplyr::mutate(net_profits_baseline = .data$production_asset_baseline * .data$scenario_price_baseline * .data$net_profit_margin)

  return(data)
}




#' Calculates annual net profits on the company-technology level for the shock scenario for increasing technologies.
#' Climate laggards which need to build out their production in increasing technologies to compensate for their
#' missed targets, are "punished" by adjusting the net profit margin on their
#' additional build out based on their proximity to target within the given
#' technology. Specifically, we measure the ratio of how much of the required
#' build out or reduction in a technology the company will have done at the end
#' of the forecast period. If the technology has an increasing target and the
#' ratio of completion is below one, the net_profit_margin on the additional production build out
#' is multiplied with the proximity to the target. This approximates the additional capital investment
#' such a company would have to make in a short time, which leads to added costs. This ensures that late
#' build out will not proportionally translate into increased profits.
#'
#' @param data A data frame containing the production forecasts of companies with increasing under the late and sudden,
#' market prices/costs, company net profit margins, the proximity to target in the production forecast period and an
#' indication of the direction of the technology.
#' @param shock_year Year of the shock.
#'
#' @return  A data frame with net profits of companies with a increasing technology

calculate_net_profits_shock_increasing_technologies <- function(data, shock_year) {
  data_overshoot_increasing <- data %>%
    dplyr::filter(.data$overshoot_direction == "Increasing") %>%
    dplyr::mutate(
      production_compensation = .data$late_sudden - .data$production_asset_baseline,
      net_profits_ls = (.data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target))
    ) %>%
    dplyr::select(-c("proximity_to_target", "production_compensation"))

  data_overshoot_decreasing <- data %>%
    dplyr::filter(.data$overshoot_direction == "Decreasing") %>%
    dplyr::mutate(
      production_compensation = 0,
      net_profits_ls = (.data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target))
    ) %>%
    dplyr::select(-c("proximity_to_target", "production_compensation"))

  data <- dplyr::bind_rows(data_overshoot_increasing, data_overshoot_decreasing)

  return(data)
}
