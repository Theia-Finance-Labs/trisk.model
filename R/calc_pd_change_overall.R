#' Calculate change in probabilities of default (PDs) of loans connected to
#' companies at hand. This is based on the equity values derived from the DCF
#' model. Said Equity values are used as different starting points for the
#' Merton model (one reflecting the business as usual baseline scenario, the
#' other reflecting the late & sudden shock scenario). The change in PDs can
#' then be used to calculate the Expected Loss due to the shock on the portfolio
#' level.
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario
#' @param end_of_analysis A numeric vector of length one that indicates until
#'   which year the analysis runs
#' @param risk_free_interest_rate A numeric vector of length one that indicates
#'   the risk free rate of interest
calculate_pd_change_overall <- function(data,
                                        shock_year = NULL,
                                        end_of_analysis = NULL,
                                        risk_free_interest_rate = NULL) {
  data <- data %>%
    dplyr::filter(.data$year >= .env$shock_year) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$sector,
      .data$scenario_geography, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::summarise(
      equity_0_baseline = sum(.data$discounted_net_profit_baseline, na.rm = TRUE),
      equity_0_late_sudden = sum(.data$discounted_net_profit_ls, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::all_of(c(
        "scenario_geography", "company_id",
        "company_name", "sector", "equity_0_baseline",
        "equity_0_late_sudden", "debt_equity_ratio", "volatility"
      ))
    )

  data <- data %>%
    dplyr::mutate(
      debt = .data$equity_0_baseline * .data$debt_equity_ratio,
      risk_free_rate = .env$risk_free_interest_rate,
      # ADO 1943 - see nesting step below
      term = NA_integer_
    ) %>%
    dplyr::select(-"debt_equity_ratio")

  nesting_names <- c(colnames(data %>% dplyr::select(-.data$term)))

  data <- data %>%
    # ADO 1943 - this remains set to 1:5 irrespective of the main input argument,
    # as we describe the overall trend of PDs, not a change in the portfolio
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(nesting_names)),
      term = 1:5
    ) %>%
    dplyr::filter(!is.na(.data$term))

  data <- keep_merton_compatible_rows(data, stage = "overall")

  results <- data %>%
    dplyr::mutate(Survival_baseline = calc_survival_probability_merton(
      L = .data$debt,
      V0 = .data$equity_0_baseline + .data$debt,
      sigma = .data$volatility,
      r = .data$risk_free_rate,
      t = .data$term
    )) %>%
    dplyr::mutate(Survival_late_sudden = calc_survival_probability_merton(
      L = .data$debt,
      V0 = .data$equity_0_late_sudden + .data$debt,
      sigma = .data$volatility,
      r = .data$risk_free_rate,
      t = .data$term
    )) %>%
    dplyr::mutate(
      PD_baseline = 1 - .data$Survival_baseline,
      PD_late_sudden = 1 - .data$Survival_late_sudden,
      PD_change = .data$PD_late_sudden - .data$PD_baseline
    )

  return(results)
}

#' Keep rows that fulfill constraints of the merton model
#'
#' Keep rows that fulfill constraints of the merton model as line out for
#' [calc_survival_probability_merton()].
#'
#' @param data A tibble holding at least the columns `debt`,
#'   `equity_0_baseline` or `equity_t_baseline`, `equity_0_late_sudden` or `equity_t_late_sudden`, `volatility`, `risk_free_rate`
#'   and `term`.
#' @param stage String, indicating if checks are done for overall or annual
#'   results.
#'
#' @return Tibble holding rows from `data` that are compatible with constraints
#'  of [calc_survival_probability_merton()].
keep_merton_compatible_rows <- function(data, stage) {
  if (!stage %in% c("overall", "annual")) {
    stop("Invalid value for argument stage")
  }

  if (stage == "overall") {
    data <- data %>%
      dplyr::mutate(V0_base = .data$debt + .data$equity_0_baseline) %>%
      dplyr::mutate(V0_late_sudden = .data$debt + .data$equity_0_late_sudden)
  } else {
    data <- data %>%
      dplyr::mutate(V0_base = .data$debt + .data$equity_t_baseline) %>%
      dplyr::mutate(V0_late_sudden = .data$debt + .data$equity_t_late_sudden)
  }

  data_filtered <- data %>%
    dplyr::filter(.data$risk_free_rate >= 0) %>%
    dplyr::filter(.data$debt > 0 & .data$V0_base > 0 & .data$V0_late_sudden > 0 & .data$volatility > 0 & .data$term > 0) %>%
    dplyr::select(-dplyr::all_of(c("V0_base", "V0_late_sudden")))

  if (nrow(data_filtered) < nrow(data)) {
    cat(paste0("Removed ", nrow(data) - nrow(data_filtered), " rows when checking for compatibility with merton model. \n"))

    if (nrow(data_filtered) == 0) {
      stop("No data remain after removing rows that are not compatible with merton model.")
    }
  }
  return(data_filtered)
}

#' Calculate survival probability
#'
#' Function calculates survival probability for a maturity based on a structural
#' Merton model.
#' For details on implementation please compare `CreditRisk::Merton()`.
#' Unlike `CreditRisk::Merton()` this implementation:
#' 1. only holds functionality to calculate probability of survival
#' 1. can be called in vectorised fashion
#' 1. additionally checks that all input values are of the same length
#' 1. additionally checks input vectors for implausible values (`r` must be => 0
#' and all other args > 0)
#'
#' @param L Numeric vector, holding debt values at maturity.
#' @param V0 Numeric vector, holding company values at time t0.
#' @param sigma Numeric vector, holding volatility values.
#' @param r Numeric vector, holding risk free interest rates.
#' @param t Vector vector holding debt maturities.
#'
#' @return A vector holding survival probabilities,
calc_survival_probability_merton <- function(L, V0, sigma, r, t) {
  input_args <- list(L, V0, sigma, r, t)

  if (dplyr::n_distinct(purrr::map_int(input_args, length)) > 1) {
    stop("All input arugments need to have the same length.")
  }

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  if (!all(r >= 0)) {
    stop("Argument r may not be negative.")
  }

  if (!all(unique(purrr::map_lgl(list(L, V0, sigma, t), function(x) {
    all(x > 0)
  })))) {
    stop(paste0("Unexpected non positive numbers detected on at least one of arguments L, V0, sigma, t."))
  }

  d1 <- (log(V0 / L) + ((r + (sigma^2 / 2)) * t)) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)

  # Default Probability
  p_default <- stats::pnorm(-d2)

  # Survival Probability Q(tau > T)
  p_survival <- 1 - p_default

  return(p_survival)
}
