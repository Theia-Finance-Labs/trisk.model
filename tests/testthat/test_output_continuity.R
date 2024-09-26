library(testthat)
library(dplyr)
library(trisk.model)

# Load the internal datasets
assets_testdata <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios_testdata <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial_features_testdata <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
ngfs_carbon_price_testdata <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))

# Define the scenarios to use
baseline_scenario <- "NGFS2023GCAM_CP"
target_scenario <- "NGFS2023GCAM_NZ2050"
scenario_geography <- "Global"

# Define the snapshot path
SNAPSHOT_PATH <- testthat::test_path("snapshots", "stress_test_snapshot.rds")

# Test 1: Ensure continuity of stress test outputs against snapshot
test_that("Ensure continuity of stress test outputs against snapshot", {
  # Skip the test if not enabled
  skip_if_not(Sys.getenv("R_USE_TESTS") == "TRUE", "Test is disabled. Set R_USE_TESTS environment variable to TRUE to enable testing.")

  # Check if the specified scenarios exist in the scenarios_testdata
  available_scenarios <- unique(scenarios_testdata$scenario)
  expect_true(baseline_scenario %in% available_scenarios, info = paste("Baseline scenario", baseline_scenario, "not found in scenarios_testdata"))
  expect_true(target_scenario %in% available_scenarios, info = paste("Target scenario", target_scenario, "not found in scenarios_testdata"))

  # Run the model with the provided data
  st_results_current <- run_trisk_model(
    assets_data = assets_testdata,
    scenarios_data = scenarios_testdata,
    financial_data = financial_features_testdata,
    carbon_data = ngfs_carbon_price_testdata,
    baseline_scenario = baseline_scenario,
    target_scenario = target_scenario,
    scenario_geography = scenario_geography
  )

  # Check if the snapshot exists, if not, create and save it
  if (!file.exists(SNAPSHOT_PATH)) {
    message("Snapshot not found. Creating snapshot with current results.")
    dir.create(dirname(SNAPSHOT_PATH), showWarnings = FALSE, recursive = TRUE)
    saveRDS(st_results_current, SNAPSHOT_PATH)
    skip("Snapshot created. Run the test again to compare results.")
  }

  # Load the snapshot for comparison
  snapshot_results <- readRDS(SNAPSHOT_PATH)

  # Compare the entire data frames without selecting specific columns
  current_npv <- st_results_current$npv_results %>% select(-c(run_id)) %>% arrange_all()
  snapshot_npv <- snapshot_results$npv_results %>% select(-c(run_id)) %>% arrange_all()

  # Compare the data frames
  expect_equal(current_npv, snapshot_npv, tolerance = 1e-8, info = "npv_results does not match the snapshot")

  # Compare the PD calculations
  current_pd <- st_results_current$pd_results %>% select(-c(run_id)) %>% arrange_all()
  snapshot_pd <- snapshot_results$pd_results %>% select(-c(run_id)) %>% arrange_all()

  expect_equal(current_pd, snapshot_pd, tolerance = 1e-8, info = "pd_results does not match the snapshot")

  # Compare the company trajectories
  current_trajectories <- st_results_current$company_trajectories %>% select(-c(run_id)) %>% arrange_all()
  snapshot_trajectories <- snapshot_results$company_trajectories %>% select(-c(run_id)) %>% arrange_all()

  expect_equal(current_trajectories, snapshot_trajectories, tolerance = 1e-8, info = "company_trajectories do not match the snapshot")
})
