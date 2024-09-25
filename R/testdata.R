#' Assets Test Data
#'
#' This dataset contains data about company assets, including production, technology, and geography details.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{company_id}{Unique identifier for the company.}
#'   \item{company_name}{Name of the company.}
#'   \item{plan_sec_prod}{Secondary production plan.}
#'   \item{country_name}{Country where the asset is located.}
#'   \item{plant_age_years}{Age of the plant in years.}
#'   \item{workforce_size}{Size of the workforce.}
#'   \item{asset_id}{Unique identifier for the asset.}
#'   \item{country_iso2}{ISO 3166-1 alpha-2 code for the country.}
#'   \item{asset_name}{Name of the asset.}
#'   \item{production_year}{Year of production data.}
#'   \item{emission_factor}{Emissions from production.}
#'   \item{technology}{Type of technology used.}
#'   \item{sector}{Production sector.}
#'   \item{capacity}{Asset capacity.}
#'   \item{capacity_factor}{Asset utilization percentage.}
#'   \item{production_unit}{Unit for production.}
#' }
#' 
#' @note This dataset is accessible via `trisk.model:::assets_testdata` for testing purposes.
"assets_testdata"

#' Financial Features Test Data
#'
#' This dataset contains financial metrics necessary for calculating stress test outputs.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{company_id}{Unique identifier for the company.}
#'   \item{pd}{Probability of default for the company.}
#'   \item{net_profit_margin}{Net profit margin for the company.}
#'   \item{debt_equity_ratio}{Debt to equity ratio.}
#'   \item{volatility}{Volatility of the company's asset values.}
#' }
#' 
#' @note This dataset is accessible via `trisk.model:::financial_features_testdata` for testing purposes.
"financial_features_testdata"

#' NGFS Carbon Price Test Data
#'
#' This dataset provides carbon pricing data used in the stress test scenarios.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{year}{Year of the carbon price.}
#'   \item{model}{Model used to generate the carbon price.}
#'   \item{scenario}{Scenario name.}
#'   \item{scenario_geography}{Geographic region for the scenario.}
#'   \item{variable}{The variable measured (e.g., carbon price).}
#'   \item{unit}{Unit of the variable.}
#'   \item{carbon_tax}{The amount of carbon tax applied in the scenario.}
#' }
#' 
#' @note This dataset is accessible via `trisk.model:::carbon_price_testdata` for testing purposes.
"ngfs_carbon_price_testdata"

#' Scenarios Test Data
#'
#' This dataset contains scenario-specific data including price paths, capacity factors, and other relevant information.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{scenario_geography}{Region relevant to the scenario.}
#'   \item{scenario}{Scenario name.}
#'   \item{scenario_pathway}{Specific pathway for the scenario.}
#'   \item{scenario_type}{Type of scenario (e.g., baseline, shock).}
#'   \item{sector}{Sector of production.}
#'   \item{technology}{Type of technology.}
#'   \item{scenario_year}{Year of the scenario data.}
#'   \item{scenario_price}{Price in the scenario.}
#'   \item{price_unit}{Unit for the price.}
#'   \item{pathway_unit}{Unit of the pathway.}
#'   \item{scenario_capacity_factor}{Capacity factor for the scenario.}
#'   \item{technology_type}{Type of technology involved.}
#'   \item{capacity_factor_unit}{Unit for the capacity factor.}
#'   \item{price_indicator}{Indicator for the price path.}
#' }
#' 
#' @note This dataset is accessible via `trisk.model:::scenarios_testdata` for testing purposes.
"scenarios_testdata"
