library(dplyr)
library(readr)

OUTPUT_DIR <- "./trisk_inputs"
PATH_AR_DATA_RAW <- "PATH/TO/COMPANY_INDICATORS.XLSX" # Edit this path
TRISK_INPUTS_BUCKET_URL <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs"


dir.create(OUTPUT_DIR, showWarnings = FALSE)

#' read Asset Resolution data
#'
#' @param path_ar_data_raw path to AR excel input
#'
#' @param sheet_name name of excel sheet
#'
read_asset_resolution <- function(path_ar_data_raw, sheet_name) {
  if (sheet_name %in% c("Company Activities", "Company Emissions")) {
    
  
  ar_data <- readxl::read_xlsx(path_ar_data_raw,
                               sheet = sheet_name) %>%
    # dplyr::select(-dplyr::starts_with("Direct Ownership")) %>%
    dplyr::rename(
      company_id = .data$`Company ID`,
      company_name = .data$`Company Name`,
      ald_sector = .data$`Asset Sector`,
      technology = .data$`Asset Technology`,
      technology_type = .data$`Asset Technology Type`,
      region = .data$`Asset Region`,
      ald_location = .data$`Asset Country`,
      activity_unit = .data$`Activity Unit`
    )

    
  } else if (sheet_name == "Company Information") {
    ar_data <- readxl::read_xlsx(path_ar_data_raw,
                                 sheet = sheet_name) %>%
      dplyr::rename(
        company_id = .data$`Company ID`,
        company_name = .data$`Company Name`,
        is_ultimate_parent = .data$`Is Ultimate Parent`,
        ald_location = .data$`Country of Domicile`,
        lei = .data$`LEI`
      )
  } else {
    stop("Sheet name not recognized")
  }

  return(ar_data)
}



#' Prepare Asset Impact data before transformation to abcd
#' @param ar_data_path file path to the source Asset Resolution xlsx
#'
#' @export
prepare_asset_impact_data <- function(ar_data_path) {
  
  # Asset Impact specific data preparation
  company_activities <-
    read_asset_resolution(ar_data_path,
                          sheet_name = "Company Activities")
  company_emissions <-
    read_asset_resolution(ar_data_path,
                          sheet_name = "Company Emissions")

  
  return(
    list(
      company_activities = company_activities,
      company_emissions = company_emissions
    )
  )

}

#' pivot values of Equity Ownership, to be used as yearly production/emissions
#'
#' @param ar_data ar_data
#'
pivot_equity_ownership_columns <- function(ar_data) {
  ar_data <- ar_data  %>%
    dplyr::select(-dplyr::starts_with("Direct Ownership"), -dplyr::starts_with("Financial Control ")) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Equity Ownership "),
      names_to = "year",
      values_to = "equity_ownership"
    ) %>%
    dplyr::mutate(year = stringr::str_extract(.data$year, stringr::regex("\\d+")))

  return(ar_data)
}

#' pivot values of Direct Ownership, to be used as yearly production/emissions
#'
#' @param ar_data ar_data
#'
pivot_direct_ownership_columns <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::select(-dplyr::starts_with("Equity Ownership"), -dplyr::starts_with("Financial Control ")) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Direct Ownership "),
      names_to = "year",
      values_to = "direct_ownership"
    ) %>%
    dplyr::mutate(year = stringr::str_extract(.data$year, stringr::regex("\\d+"))) 

  return(ar_data)
}

#' Combine equity and direct ownership based on sector rules
#'
#' @param equity_data Dataframe with equity ownership
#' @param direct_data Dataframe with direct ownership
#'
combine_ownership_data <- function(equity_data, direct_data) {
  # Join the two dataframes
  combined_data <- equity_data %>%
    dplyr::left_join(direct_data, by = c("company_id", "company_name", "ald_sector", "technology", 
                                        "technology_type", "region", "ald_location", "activity_unit", "year"))
  return(combined_data)
}


# ----------------------
# Define paths and filters


outputs_list <- prepare_asset_impact_data(ar_data_path = PATH_AR_DATA_RAW)
DB_company_activities <- outputs_list[["company_activities"]]
DB_company_emissions  <- outputs_list[["company_emissions"]]

# Process both equity and direct ownership
equity_activities <- pivot_equity_ownership_columns(DB_company_activities) 
direct_activities <- pivot_direct_ownership_columns(DB_company_activities) 
company_activities <- combine_ownership_data(equity_activities, direct_activities)

# REMOVE technology types that are DUPLICATED rows
equity_emissions <- pivot_equity_ownership_columns(DB_company_emissions) %>%
    dplyr::filter(activity_unit %in% c("tCO2", "tCO2e"))
direct_emissions <- pivot_direct_ownership_columns(DB_company_emissions) %>%
    dplyr::filter(activity_unit %in% c("tCO2", "tCO2e"))
company_emissions <- combine_ownership_data(equity_emissions, direct_emissions)

# Rename activity_unit column in respective dataframes
company_activities <- company_activities %>%
  dplyr::rename(production_unit = activity_unit)

company_emissions <- company_emissions %>%
  dplyr::rename(emission_unit = activity_unit)

# Merge activities and emissions
merged_data <- company_activities %>%
  dplyr::inner_join(
    company_emissions,
    by = c("company_id", "company_name", "ald_sector", "technology", 
           "technology_type", "region", "ald_location", "year")
  ) %>%
  # Create the necessary production and emission columns
  dplyr::transmute(
    company_id, company_name, ald_sector, technology, technology_type, 
    region, ald_location, year,
    production_unit, emission_unit,
    # Rename to match what was requested
    direct_ownership_production = direct_ownership.x,
    equity_ownership_production = equity_ownership.x,
    direct_ownership_emissions = direct_ownership.y,
    equity_ownership_emissions = equity_ownership.y
  )


#' rename technology column according to some rules
#' @param ar_data ar_data
#'
rename_technology <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "Coal",
        .data$technology %in% c("Gas", "Natural Gas Liquids") ~ "Gas",
        .data$technology == "Oil and Condensate" ~ "Oil",
        .data$technology == "ICE Diesel" ~ "ICE",
        .data$technology == "ICE Gasoline" ~ "ICE",
        .data$technology == "ICE CNG" ~ "ICE",
        .data$technology == "ICE Propane" ~ "ICE",
        .data$technology == "ICE E85+" ~ "ICE",
        .data$technology == "Hybrid No-Plug" ~ "Hybrid",
        .data$technology == "Hybrid Plug-In" ~ "Hybrid",
        .data$technology == "Fuel Cell" ~ "FuelCell",
        TRUE ~ .data$technology
      )
    ) |>
      # hardcoded renaming for Steel sector
      dplyr::mutate(technology = dplyr::case_when(
        technology == 'Basic Oxygen Furnace' & technology_type == 'Integrated Blast Furnace' ~ 'BF-BOF',
        technology == 'Basic Oxygen Furnace' & technology_type == 'Stand-Alone Steel Furnace' ~ 'BOF',
        technology == 'Basic Oxygen Furnace' & technology_type == 'Integrated DRI Furnace' ~ 'DRI-BOF',
        technology == 'Electric Arc Furnace' & technology_type == 'Integrated Blast Furnace' ~ 'EAF',
        technology == 'Electric Arc Furnace' & technology_type == 'Integrated DRI Furnace' ~ 'DRI-EAF',
        technology == 'Electric Arc Furnace' & technology_type == 'Integrated Open Hearth Furnace' ~ 'EAF',
        technology == 'Electric Arc Furnace' & technology_type == 'Mini-Mill' ~ 'EAF',
        technology == 'Open Hearth Furnace' & technology_type == 'Integrated Blast Furnace' ~ 'BF-BOF',
        TRUE ~ technology  # Default case to keep existing value
      ))
  return(ar_data)
}
#' rename ald_sector column according to some rules
#' @param ar_data ar_data
#'
rename_ald_sector <- function(ar_data) {
  ar_data <- ar_data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$ald_sector == "LDV" ~ "Automotive",
        .data$ald_sector %in% c("Upstream O&G") ~ "Oil&Gas",
        TRUE ~ .data$ald_sector
      )
    )   
  return(ar_data)
}

renamed_merged_data <- rename_ald_sector(merged_data)
renamed_merged_data <- rename_technology(renamed_merged_data)

grouped_renamed_merged_data <- renamed_merged_data %>%
  dplyr::group_by(company_id, company_name, ald_sector, technology, region, ald_location, year, production_unit, emission_unit) %>%
  dplyr::summarise(
    direct_ownership_production = sum(direct_ownership_production, na.rm = TRUE),
    equity_ownership_production = sum(equity_ownership_production, na.rm = TRUE),
    direct_ownership_emissions = sum(direct_ownership_emissions, na.rm = TRUE),
    equity_ownership_emissions = sum(equity_ownership_emissions, na.rm = TRUE),
    .groups = "drop"
  )

# For Power sector, we need to handle MW and MWh differently
# First, calculate emission factors using MWh data for Power sector
power_emission_factors <- grouped_renamed_merged_data %>%
  dplyr::filter(ald_sector == "Power" & production_unit == "MWh") %>%
  dplyr::mutate(
    direct_emission_factor = direct_ownership_emissions / direct_ownership_production,
    equity_emission_factor = equity_ownership_emissions / equity_ownership_production,
    emission_factor_unit = "tCO2/MWh"
  ) %>%
  dplyr::select(
    company_id, company_name, ald_sector, technology, region, ald_location, year,
    direct_emission_factor, equity_emission_factor, emission_factor_unit
  )

# Then, keep just the MW rows for Power capacity data
power_capacity_data <- grouped_renamed_merged_data %>%
  dplyr::filter(ald_sector == "Power" & production_unit == "MW") %>%
  dplyr::select(-c(direct_ownership_emissions, equity_ownership_emissions, emission_unit))

# Join the capacity data with emission factors
power_data <- power_capacity_data %>%
  dplyr::left_join(
    power_emission_factors,
    by = c("company_id", "company_name", "ald_sector", "technology", "region", "ald_location", "year")
  )

# Handle other sectors normally
non_power_data <- grouped_renamed_merged_data %>%
  dplyr::filter(ald_sector != "Power") %>%
  dplyr::mutate(
    direct_emission_factor = direct_ownership_emissions / direct_ownership_production,
    equity_emission_factor = equity_ownership_emissions / equity_ownership_production,
    emission_factor_unit = paste0(emission_unit, "/", production_unit)
  )

# Combine Power and non-Power data
emission_factors <- dplyr::bind_rows(power_data, non_power_data)
emission_factors  <- emission_factors %>%
    dplyr::mutate(
        direct_emission_factor = tidyr::replace_na(direct_emission_factor, 0),
        equity_emission_factor = tidyr::replace_na(equity_emission_factor, 0)
    )

# Final table with power in MW for production and emission factor in tCO2/MWh
assets_data <- emission_factors %>%
  # First group by company_id to handle duplicates within each company
  group_by(company_id) %>%
  # Create a unique identifier for each combination within the company
  mutate(
    unique_key = paste(technology, ald_sector, ald_location, sep = "_"),
    # Create a counter for each unique combination within the company
    counter = match(unique_key, unique(unique_key)),
    asset_id = paste(company_id, counter, sep = "_")
  ) %>%
  ungroup() %>%
    mutate(
    asset_id = as.character(asset_id),
    company_id = as.character(company_id),
    asset_name = company_name,
    company_name = company_name,
    country_iso2 = ald_location,
    country_name = NA,
    plant_age_years = NA,
    workforce_size = NA,
    capacity_factor = 1,  # Default value, adjust if you have actual data
    capacity = equity_ownership_production,
    production_year = year,
    production_unit = production_unit,
    emission_factor = equity_emission_factor,
    emission_factor_unit = emission_factor_unit,
    # Standardize sector and technology names
    sector = ald_sector,
    technology = technology
  ) %>%
  select(
    asset_id,
    asset_name,
    company_id,
    company_name,
    country_iso2,
    country_name,
    technology,
    sector,
    plant_age_years,
    workforce_size,
    capacity_factor,
    capacity,
    production_year,
    production_unit,
    emission_factor
  ) %>% 
  filter(production_year >= 2023, production_year <= 2029)
# Write to csv
readr::write_csv(assets_data, fs::path(OUTPUT_DIR, "assets_data.csv"))


# FINANCIAL DATA ------------------------------------------------------------


# Define local path to save the file
destfile <- tempfile(fileext = ".csv")

# Download the file
utils::download.file(fs::path(TRISK_INPUTS_BUCKET_URL, "financial_averages.csv"), destfile, mode = "wb")

# Read into a data frame
financial_averages_csv <- readr::read_csv(destfile)


# Create new financial_data by merging with assets_data and aggregating
financial_data <- assets_data %>%
  # Get unique combinations of company_id, sector, technology
  distinct(company_id, sector, technology) %>%
  # Join with financial averages
  left_join(financial_averages_csv, by = c("sector", "technology")) %>%
  # Group by company to aggregate
  group_by(company_id) %>%
  summarise(
    # Take weighted averages based on number of technologies
    pd = mean(pd, na.rm = TRUE),
    debt_equity_ratio = mean(debt_equity_ratio, na.rm = TRUE),
    net_profit_margin = mean(net_profit_margin, na.rm = TRUE),
    volatility = mean(volatility, na.rm = TRUE),
    .groups = "drop"
  )


financial_data %>% readr::write_csv(fs::path(OUTPUT_DIR, "financial_data.csv"))




# SCENARIOS DATA ------------------------------------------------------------


# Define local path to save the file
destfile <- tempfile(fileext = ".csv")

# Download the file
utils::download.file(fs::path(TRISK_INPUTS_BUCKET_URL, "scenarios.csv"), destfile, mode = "wb")

# Read into a data frame
scenarios_csv <- readr::read_csv(destfile)

scenarios_csv %>% readr::write_csv(fs::path(OUTPUT_DIR, "scenarios.csv"))





# CARBON TAX DATA ------------------------------------------------------------



# Define local path to save the file
destfile <- tempfile(fileext = ".csv")

# Download the file
utils::download.file(fs::path(TRISK_INPUTS_BUCKET_URL, "ngfs_carbon_price.csv"), destfile, mode = "wb")

# Read into a data frame
ngfs_carbon_price_csv <- readr::read_csv(destfile)

ngfs_carbon_price_csv %>% readr::write_csv(fs::path(OUTPUT_DIR, "ngfs_carbon_price.csv"))
