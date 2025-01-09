add_technology_fair_share_ratio <- function(data) {
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$scenario, .data$sector, .data$scenario_geography, .data$technology) %>%
    dplyr::arrange(.data$scenario_year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$scenario_pathway - dplyr::first(.data$scenario_pathway)) / dplyr::first(.data$scenario_pathway)) %>%
    dplyr::ungroup()
  return(data)
}

calculate_fair_share_perc <- function(data) {
  data <- data %>%
    dplyr::mutate(
      fair_share_perc = .data$tmsr
    )
  
  # replace nan fair_share_perc by 0. Nans appear when dividing per 0 in the tmsr computation
  data <- data %>%
    dplyr::mutate(fair_share_perc = dplyr::if_else(is.na(.data$fair_share_perc), 0, .data$fair_share_perc))

  return(data)
}
