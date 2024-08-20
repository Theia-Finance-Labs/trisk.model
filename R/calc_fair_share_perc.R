add_technology_fair_share_ratio <- function(data) {
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$scenario, .data$sector, .data$scenario_geography, .data$technology) %>%
    dplyr::arrange(.data$scenario_year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$scenario_pathway - dplyr::first(.data$scenario_pathway)) / dplyr::first(.data$scenario_pathway)) %>%
    dplyr::ungroup()
  return(data)
}

add_market_fair_share_percentage <- function(data) {
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$scenario, .data$sector, .data$scenario_geography, .data$scenario_year) %>%
    dplyr::arrange(.data$scenario_year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$scenario_pathway)) %>%
    dplyr::group_by(.data$scenario, .data$sector, .data$scenario_geography, .data$technology) %>%
    dplyr::mutate(
      smsp = (.data$scenario_pathway - dplyr::first(.data$scenario_pathway)) /
        dplyr::first(.data$sector_total_by_year),
      sector_total_by_year = NULL
    ) %>%
    dplyr::ungroup()
  return(data)
}

calculate_fair_share_perc <- function(data) {
  data <- data %>%
    dplyr::mutate(
      fair_share_perc = dplyr::case_when(
        .data$technology_type == "carbontech" ~ .data$tmsr,
        .data$technology_type == "greentech" ~ .data$smsp,
        TRUE ~ NA_real_ # Use NA_real_ for numeric columns or NA_character_ for character columns
      )
    )

  # replace nan fair_share_perc by 0. Nans appear when dividing per 0 in the tmsr computation
  data <- data %>%
    dplyr::mutate(fair_share_perc = dplyr::if_else(is.na(fair_share_perc), 0, fair_share_perc))

  return(data)
}
