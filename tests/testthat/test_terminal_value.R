library(testthat)
library(dplyr)

# Terminal value must implement the 2DII "Limited Visibility" stable-growth
# formula with the pull-back to start_year:
#   TV_discounted = CF[end_year] * (1 + g) / (r - g) / (1 + r)^(end_year - start_year)

test_that("calculate_terminal_value applies Gordon perpetuity and pulls back to start_year", {
  start_year <- 2020
  end_year <- 2030
  discount_rate <- 0.08
  growth_rate <- 0.03

  fake <- tibble::tibble(
    asset_id = 1, company_id = 1, sector = "power", technology = "coal",
    year = start_year:end_year,
    net_profits_baseline = 100,
    net_profits_ls = 80
  )

  with_tv <- trisk.model:::calculate_terminal_value(
    fake,
    start_year = start_year,
    end_year = end_year,
    growth_rate = growth_rate,
    discount_rate = discount_rate
  )

  tv_row <- with_tv %>% dplyr::filter(.data$year == end_year + 1)

  horizon <- end_year - start_year
  expected_baseline <- 100 * (1 + growth_rate) / (discount_rate - growth_rate) /
    (1 + discount_rate)^horizon
  expected_ls <- 80 * (1 + growth_rate) / (discount_rate - growth_rate) /
    (1 + discount_rate)^horizon

  expect_equal(tv_row$discounted_net_profit_baseline, expected_baseline, tolerance = 1e-10)
  expect_equal(tv_row$discounted_net_profit_ls, expected_ls, tolerance = 1e-10)
  expect_equal(tv_row$net_profits_baseline, 100 * (1 + growth_rate))
  expect_equal(tv_row$net_profits_ls, 80 * (1 + growth_rate))
})

test_that("calculate_annual_profits end-to-end produces a decreasing discount factor in the terminal row", {
  start_year <- 2020
  end_year <- 2030
  discount_rate <- 0.08
  growth_rate <- 0.03

  fake <- tibble::tibble(
    asset_id = 1, company_id = 1, sector = "power", technology = "coal",
    year = start_year:end_year,
    net_profits_baseline = 100,
    net_profits_ls = 80
  )

  out <- trisk.model:::calculate_annual_profits(
    data = fake,
    baseline_scenario = "x", shock_scenario = "y",
    start_year = start_year,
    end_year = end_year,
    discount_rate = discount_rate,
    growth_rate = growth_rate
  )

  # Implied per-row discount factor relative to the undiscounted net profit.
  out <- out %>%
    dplyr::mutate(implied_factor = .data$discounted_net_profit_baseline / .data$net_profits_baseline)

  last_explicit_factor <- out$implied_factor[out$year == end_year]
  terminal_factor <- out$implied_factor[out$year == end_year + 1]

  # With proper pull-back, terminal factor equals (1 / (r-g)) * (1 / (1+r)^horizon),
  # which for r=0.08, g=0.03, horizon=10 is about 9.27. It must be larger than the
  # last explicit year's factor (1/(1+r)^10 ~ 0.463) — that's expected because the
  # terminal row represents an infinite stream, not a single year. The critical
  # property is that it is NOT equal to 1/(r-g) (20x) which would signal missing
  # pull-back.
  horizon <- end_year - start_year
  expect_equal(
    terminal_factor,
    (1 / (discount_rate - growth_rate)) / (1 + discount_rate)^horizon,
    tolerance = 1e-10
  )
  expect_lt(terminal_factor, 1 / (discount_rate - growth_rate))
  expect_gt(terminal_factor, last_explicit_factor)
})
