apply_age_cutoff <- function(trisk_model_output, shock_year) {

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
    # Replace missing values with zero (to be improved with median age)
    tidyr::replace_na(list(plant_age_years = 0)) %>%                            #TODO: add median age
    # Make the age column count
    dplyr::mutate(incr_plant_age = plant_age_years + year - 2024 + 1) %>%
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
      )
    )
  return(trisk_model_output)
}


apply_staggered_shock <- function(trisk_model_output) {
    
    return(trisk_model_output)
}
