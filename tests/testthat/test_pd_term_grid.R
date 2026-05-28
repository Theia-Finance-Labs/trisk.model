library(testthat)
library(dplyr)

# Regression: calculate_pd_change_overall used to hardcode term = 1:5, then
# term = 1:10. Portfolio rows past the ceiling silently NA'd in the downstream
# join in trisk.analysis. The fix sizes the term grid to the analysis horizon
# (end_of_analysis - start_year + 1) so PDs cover the full projection window.

test_that("calculate_pd_change_overall sizes term grid to the analysis horizon", {
  start_year <- 2020
  end_of_analysis <- 2035
  expected_max_term <- end_of_analysis - start_year + 1  # 16

  fake <- tibble::tibble(
    year = start_year:end_of_analysis,
    company_id = "co_1",
    company_name = "Acme",
    sector = "power",
    debt_equity_ratio = 0.5,
    volatility = 0.3,
    discounted_net_profit_baseline = 100,
    discounted_net_profit_ls = 80
  )

  pd <- trisk.model:::calculate_pd_change_overall(
    fake,
    shock_year = 2025,
    start_year = start_year,
    end_of_analysis = end_of_analysis,
    risk_free_interest_rate = 0.02
  )

  expect_equal(max(pd$term), expected_max_term)
  expect_equal(sort(unique(pd$term)), seq_len(expected_max_term))
})

test_that("calculate_pd_change_overall produces non-NA PDs for term > 10 (regression for ADO 1943)", {
  fake <- tibble::tibble(
    year = 2020:2035,
    company_id = "co_1",
    company_name = "Acme",
    sector = "power",
    debt_equity_ratio = 0.5,
    volatility = 0.3,
    discounted_net_profit_baseline = 100,
    discounted_net_profit_ls = 80
  )

  pd <- trisk.model:::calculate_pd_change_overall(
    fake,
    shock_year = 2025,
    start_year = 2020,
    end_of_analysis = 2035,
    risk_free_interest_rate = 0.02
  )

  long_term_rows <- pd %>% dplyr::filter(.data$term >= 11)
  expect_gt(nrow(long_term_rows), 0)
  expect_true(all(!is.na(long_term_rows$PD_baseline)))
  expect_true(all(!is.na(long_term_rows$PD_late_sudden)))
})
