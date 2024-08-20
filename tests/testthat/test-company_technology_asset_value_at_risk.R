test_that("without specified arguments, calculate_asset_value_at_risk
          throws error", {
  testthat::expect_error(
    calculate_asset_value_at_risk(),
    "argument \"data\" is missing"
  )
})
