make_shock_data <- function(sector = "Power", year = 2026) {
  tibble::tibble(
    company_id = 1,
    asset_id = 1,
    sector = sector,
    technology = "CoalCap",
    year = year,
    overshoot_direction = "Increasing",
    late_sudden = 100,
    production_asset_baseline = 100,
    late_sudden_price = 10,
    net_profit_margin = 0.1,
    emission_factor = 1,
    proximity_to_target = 1,
    technology_type = "carbontech"
  )
}

make_carbon_data <- function(year = 2026, carbon_tax = 0.2) {
  tibble::tibble(
    year = year,
    scenario = "flat_carbon_tax",
    carbon_tax = carbon_tax
  )
}

test_that("carbon tax hits shock net profits as a full below-the-line cost, not margin-scaled", {
  data <- dplyr::bind_rows(
    make_shock_data(year = 2025),
    make_shock_data(year = 2026)
  )
  carbon_data <- make_carbon_data(year = c(2025, 2026), carbon_tax = c(0.2, 0.2))

  result <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data,
    shock_year = 2026,
    carbon_data = carbon_data,
    market_passthrough_power = 0,
    market_passthrough_primary = 0
  ) %>%
    dplyr::arrange(year)

  # Pre-shock year: tax zeroed, profit = q * p * margin = 100 * 10 * 0.1
  expect_equal(result$net_profits_ls[result$year == 2025], 100)

  # Shock year: profit = q * p * margin - q * tax * EF = 100 - 100 * 0.2 * 1 = 80.
  # The carbon cost must NOT be scaled by net_profit_margin (which would give 98).
  expect_equal(result$net_profits_ls[result$year == 2026], 80)
})

test_that("sector-level passthrough defaults shield Power at 60% and primary sectors at 90%", {
  data <- dplyr::bind_rows(
    make_shock_data(sector = "Power"),
    make_shock_data(sector = "Oil&Gas")
  )
  carbon_data <- make_carbon_data()

  result <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data,
    shock_year = 2026,
    carbon_data = carbon_data,
    market_passthrough_power = 0.6,
    market_passthrough_primary = 0.9
  )

  # Power bears (1 - 0.6): 100 - 100 * 0.4 * 0.2 * 1 = 92
  expect_equal(result$net_profits_ls[result$sector == "Power"], 92)
  # Oil&Gas (primary) bears (1 - 0.9): 100 - 100 * 0.1 * 0.2 * 1 = 98
  expect_equal(result$net_profits_ls[result$sector == "Oil&Gas"], 98)
})

test_that("sector-level passthrough rates remain user-adjustable", {
  data <- dplyr::bind_rows(
    make_shock_data(sector = "Power"),
    make_shock_data(sector = "Coal")
  )
  carbon_data <- make_carbon_data()

  result <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data,
    shock_year = 2026,
    carbon_data = carbon_data,
    market_passthrough_power = 0,
    market_passthrough_primary = 0.5
  )

  # Power at zero passthrough bears the full cost: 100 - 20 = 80
  expect_equal(result$net_profits_ls[result$sector == "Power"], 80)
  # Coal (primary) bears half: 100 - 10 = 90
  expect_equal(result$net_profits_ls[result$sector == "Coal"], 90)
})

test_that("net profits remain floored at zero when the carbon cost exceeds profit", {
  data <- make_shock_data(sector = "Power")
  carbon_data <- make_carbon_data(carbon_tax = 50)

  result <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data,
    shock_year = 2026,
    carbon_data = carbon_data,
    market_passthrough_power = 0,
    market_passthrough_primary = 0
  )

  # 100 - 100 * 50 * 1 is deeply negative -> current model floors at 0
  expect_equal(result$net_profits_ls, 0)
})

test_that("Decreasing overshoot branch applies the same below-the-line carbon cost", {
  data <- make_shock_data() %>%
    dplyr::mutate(overshoot_direction = "Decreasing")
  carbon_data <- make_carbon_data()

  result <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data,
    shock_year = 2026,
    carbon_data = carbon_data,
    market_passthrough_power = 0,
    market_passthrough_primary = 0
  )

  # q * p * margin - q * tax * EF = 100 - 100 * 0.2 * 1 = 80. The old
  # margin-scaled Decreasing formula would give 98.
  expect_equal(result$net_profits_ls, 80)
})

test_that("run_trisk_model ships sector passthrough defaults 0.6 (Power) and 0.9 (primary)", {
  expect_equal(formals(run_trisk_model)$market_passthrough_power, 0.6)
  expect_equal(formals(run_trisk_model)$market_passthrough_primary, 0.9)
})
