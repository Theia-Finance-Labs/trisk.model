library(trisk.model)

INPUT_DIR <- "./trisk_inputs"
OUTPUT_DIR <- "./trisk_outputs"

dir.create(OUTPUT_DIR, showWarnings = FALSE)


assets_data <- readr::read_csv(fs::path(INPUT_DIR, "assets_data.csv"))
financial_data <- readr::read_csv(fs::path(INPUT_DIR, "financial_data.csv"))
scenarios_data <- readr::read_csv(fs::path(INPUT_DIR, "scenarios.csv"))
ngfs_carbon_price_data <- readr::read_csv(fs::path(INPUT_DIR, "ngfs_carbon_price.csv"))




trisk_results <- run_trisk_model(
    assets_data=assets_data,
    scenarios_data=scenarios_data,
    financial_data=financial_data,
    carbon_data=ngfs_carbon_price_data,
    baseline_scenario = "NGFS2024GCAM_CP",
    target_scenario = "NGFS2024GCAM_NZ2050",
    scenario_geography = "Asia",
    carbon_price_model = "no_carbon_tax",
    risk_free_rate = 0.02,
    discount_rate = 0.07,
    growth_rate = 0.03,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    market_passthrough = 0
)


trisk_results$npv_results %>% readr::write_csv(fs::path(OUTPUT_DIR, "npv_results.csv"))
trisk_results$pd_results %>% readr::write_csv(fs::path(OUTPUT_DIR, "pd_results.csv"))
trisk_results$company_trajectories %>% readr::write_csv(fs::path(OUTPUT_DIR, "company_trajectories.csv"))
