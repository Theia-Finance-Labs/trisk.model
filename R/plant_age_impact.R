apply_age_cutoff <- function(trisk_model_output, shock_year, start_year=2024) {

  # Define the cutoff age for each technology
  cutoff_ages <- tibble::tibble(
    technology = c("CoalCap", "GasCap", "RenewablesCap","NuclearCap","HydroCap","BatteryCap"),  # Replace with actual tech names
    maxage = c(40, 30, 25,60,80,10)  # Replace with appropriate cutoff values
  )
  
  # Fill the initial age column
  trisk_model_output <- trisk_model_output %>%
    # Fill the initial age column
    tidyr::fill(plant_age_years, .direction = "down") %>%
    # Convert to numeric in case there are strings
    dplyr::mutate(plant_age_years = as.numeric(plant_age_years)) %>%
    # Make the age column count
    dplyr::mutate(incr_plant_age = plant_age_years + year - start_year + 1) %>%
    # Join with cutoff age mapping
    dplyr::left_join(cutoff_ages, by = "technology") %>%
    # Compute how many full renewal cycles have passed
    dplyr::mutate(
      n_cycles = floor(plant_age_years / maxage),  # Number of full renewals before start
      last_renewal_age = n_cycles * maxage,  # Last renewal age
      next_renewal_year = (last_renewal_age + maxage)-plant_age_years+2024,
      next_renewal_age = dplyr::case_when(next_renewal_year<=shock_year~last_renewal_age + 2*maxage,
                                          T~last_renewal_age + maxage),  # Next renewal threshold
      # Set production_asset_baseline to 0 when conditions are met
      production_asset_baseline = dplyr::case_when(
        incr_plant_age >= next_renewal_age & year>=shock_year ~ 0,
        TRUE ~ production_asset_baseline
      ),
      late_sudden = dplyr::case_when(
        incr_plant_age >= next_renewal_age & year>=shock_year ~ 0,
        TRUE ~ late_sudden
      )
    )
  return(trisk_model_output)
}

#' Apply Staggered Shock to TRISK Model Output
#'
#' This function applies a staggered shock to the TRISK model output based on the age of assets and their age differences.
#' The shock is designed to be more severe for younger assets and those with smaller age differences between them.
#'
#' @param trisk_model_output A dataframe containing the TRISK model output with columns:
#'   - company_name: Name of the company
#'   - asset_id: Unique identifier for the asset
#'   - technology: Type of technology
#'   - plant_age_years: Age of the plant in years
#'   - production_asset_baseline: Baseline production value
#'   - late_sudden: Late sudden shock value
#'
#' @return A modified version of the input dataframe with:
#'   - staggered_coefficient: The coefficient applied to each asset based on its age and age difference
#'   - production_asset_baseline: Updated baseline production after applying the staggered coefficient
#'   - late_sudden: Updated late sudden shock after applying the staggered coefficient
#'
#' @details
#' The function works as follows:
#' 1. Groups assets by company and technology
#' 2. For each asset, calculates the minimum age difference with other assets in the same group
#' 3. Assigns assets to age bins (16-year intervals from 0 to 100 years)
#' 4. Assigns age differences to similar bins
#' 5. Applies a coefficient matrix where:
#'    - Rows represent asset age bins (youngest to oldest)
#'    - Columns represent age difference bins (smallest to largest)
#'    - Coefficients follow the formula a * k^distance, where:
#'      - a = 0.9 (base coefficient)
#'      - k = 0.8 (decay factor)
#'      - distance is the Manhattan distance from the top-left corner
#' 6. For assets with no comparable assets (Inf age difference), applies a coefficient of 1
#' 7. Applies the coefficients to both production_asset_baseline and late_sudden values
#'
#' @examples
#' \dontrun{
#' # Apply staggered shock to model output
#' result <- apply_staggered_shock(trisk_model_output)
#' }
#'
#' @export
apply_staggered_shock <- function(trisk_model_output) {
  # Define constants
  a <- 0.9
  k <- 0.8
  
  # Create age bins (16-year intervals)
  age_bins <- seq(0, 100, by = 16)
  n_bins <- length(age_bins) - 1  # We have n_bins intervals
  
  # Create coefficient matrix
  coeff_matrix <- matrix(NA, nrow = n_bins, ncol = n_bins)
  
  # Fill the matrix with coefficients
  for (i in 1:n_bins) {
    for (j in 1:n_bins) {
      # Calculate the Manhattan distance from the top-left corner
      distance <- (i - 1) + (j - 1)
      # Apply the formula a * k^distance
      coeff_matrix[i, j] <- a * (k ^ distance)
    }
  }
  
  # Function to get coefficient from matrix
  get_coefficient <- function(age_bin, diff_bin) {
    if (is.na(age_bin) || is.na(diff_bin) || age_bin < 1 || diff_bin < 1 || 
        age_bin > n_bins || diff_bin > n_bins) {
      return(NA_real_)
    }
    return(coeff_matrix[age_bin, diff_bin])
  }
  
  # Calculate minimum age difference for each asset within company+technology groups
  result <- trisk_model_output %>%
    dplyr::distinct(company_name, asset_id, technology, plant_age_years) %>%
    dplyr::group_by(company_name, technology) %>%
    dplyr::mutate(
      # For each asset, calculate absolute age differences with all other assets in the group
      min_age_diff = purrr::map_dbl(
        plant_age_years,
        ~ {
          other_ages <- plant_age_years[plant_age_years != .x]
          if (length(other_ages) == 0) {
            Inf
          } else {
            min(abs(.x - other_ages), na.rm = TRUE)
          }
        }
      ),
      # Handle edge cases for plant_age_years
      plant_age_adjusted = dplyr::case_when(
        plant_age_years > 100 ~ 100,
        TRUE ~ plant_age_years
      ),
      # Handle edge cases for min_age_diff
      min_age_diff_adjusted = dplyr::case_when(
        is.infinite(min_age_diff) ~ NA_real_,
        min_age_diff > 100 ~ 100,
        TRUE ~ min_age_diff
      ),
      # Assign age bins (youngest to oldest)
      age_bin = cut(plant_age_adjusted, breaks = age_bins, labels = FALSE, include.lowest = TRUE),
      # Assign diff bins (youngest to oldest)
      diff_bin = dplyr::case_when(
        is.infinite(min_age_diff) ~ NA_real_,
        TRUE ~ cut(min_age_diff_adjusted, breaks = age_bins, labels = FALSE, include.lowest = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    # Directly calculate staggered coefficient
    dplyr::rowwise() %>%
    dplyr::mutate(
      staggered_coefficient = dplyr::case_when(
        is.infinite(min_age_diff) ~ 1,
        TRUE ~ get_coefficient(age_bin, diff_bin)
      )
    ) %>%
    dplyr::ungroup()
  
  trisk_model_output <- trisk_model_output %>%
    dplyr::left_join(
      result %>% dplyr::select(company_name, asset_id, technology, staggered_coefficient), 
      by = c("company_name", "asset_id", "technology")) %>%
    dplyr::mutate(
      production_asset_baseline = production_asset_baseline * staggered_coefficient,
      late_sudden = late_sudden * staggered_coefficient
    )
  
  return(trisk_model_output)
}
