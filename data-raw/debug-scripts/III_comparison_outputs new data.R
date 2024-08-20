# OLD_CRISPY_PATH <- "workspace/st_outputs/THEIA_main/crispy_output_standard_NGFS2023MESSAGE_B2DS_Global_no_carbon_tax.csv"
# OLD_TRAJ_PATH <- "workspace/st_outputs/THEIA_main/company_trajectories_standard_NGFS2023MESSAGE_B2DS_Global_no_carbon_tax.csv"

# OLD_CRISPY_PATH <- "workspace/st_outputs/THEIA_main_WEO2023/crispy_output_standard_WEO2023_APS_Global_no_carbon_tax.csv"
# OLD_TRAJ_PATH <- "workspace/st_outputs/THEIA_main_WEO2023/company_trajectories_standard_WEO2023_APS_Global_no_carbon_tax.csv"

OLD_CRISPY_PATH <- "workspace/st_outputs/THEIA_MAIN_GCAM_CP_NZ2050/crispy_output_standard_NGFS2023GCAM_NZ2050_Global_no_carbon_tax.csv"
OLD_TRAJ_PATH <- "workspace/st_outputs/THEIA_MAIN_GCAM_CP_NZ2050/company_trajectories_standard_NGFS2023GCAM_NZ2050_Global_no_carbon_tax.csv"


library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)


devtools::load_all()


npv_check <- function() {
  source("data-raw/debug-scripts/II_debug_workflow.R")
  latest_output <- get_latest_timestamped_folder("workspace/st_outputs")

  # NPV COMPARISON ============================================================

  new_npv <- readr::read_csv(paste0(latest_output, "/npv_results.csv")) %>% dplyr::rename(shock_scenario = target_scenario)
  new_pd <- readr::read_csv(paste0(latest_output, "/pd_results.csv")) %>% dplyr::rename(shock_scenario = target_scenario)


  old <- readr::read_csv(OLD_CRISPY_PATH)

  old_npv <- old %>%
    dplyr::filter(term == 1) %>%
    dplyr::select_at(c(
      "company_id", "ald_sector", "ald_business_unit", "start_year", "carbon_price_model",
      "lgd", "risk_free_rate", "discount_rate", "growth_rate", "div_netprofit_prop_coef",
      "shock_year", "market_passthrough", "baseline_scenario",
      "shock_scenario", "scenario_geography", "net_present_value_baseline", "net_present_value_shock"
    )) %>%
    dplyr::rename(technology = ald_business_unit)

  new_npv <- new_npv %>%
    dplyr::inner_join(old_npv, by = c(
      "company_id", "sector", "technology", "start_year", "carbon_price_model",
      "lgd", "risk_free_rate", "discount_rate", "growth_rate", "div_netprofit_prop_coef",
      "shock_year", "market_passthrough", "baseline_scenario",
      "target_scenario", "scenario_geography"
    ))

  prepare_for_npv_barplot <- function() {
    mix <- dplyr::inner_join(old_npv, new_npv, by = c("company_id", "ald_sector" = "sector", "technology")) # , "term"))

    # List of root names based on your data frame
    roots <- c("net_present_value_baseline", "net_present_value_shock") # , "pd_baseline", "pd_shock")

    # Iterate over each root name
    for (root in roots) {
      x_col <- paste0(root, ".x")
      y_col <- paste0(root, ".y")

      # Subtract and create new column, remove old columns
      mix <- mix %>%
        mutate(!!paste0(root, "_diff") := !!sym(x_col) - !!sym(y_col)) %>%
        select(-!!sym(x_col), -!!sym(y_col))
      return(mix)
    }
  }
  mix <- prepare_for_npv_barplot()

  plot_npv_bars <- function() {
    # Assuming your dataframe is named df
    df <- mix

    # Replace zeros with NAs in _diff columns, except those completely filled with zeros
    diff_cols <- grep("_diff$", names(df), value = TRUE)
    for (col in diff_cols) {
      if (all(df[[col]] == 0)) next
      df[[col]][df[[col]] == 0] <- NA
    }

    # Reshape the dataframe for plotting
    long_df <- df %>%
      select(contains("_diff")) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value")



    # Plot distribution plots for each _diff column with scientific notation on x-axis
    g <- ggplot(long_df, aes(x = value, fill = variable)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
      facet_wrap(~variable, scales = "free") +
      theme_minimal() +
      scale_x_continuous(
        labels = scientific_format(),
        breaks = scales::pretty_breaks()
      ) +
      labs(title = "Distribution of '_diff' Columns", x = "Value (Scientific Notation)", y = "Count") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    return(g)
  }

  print(plot_npv_bars())
}





# TRAJECTORIES COMPARISON ============================================================
traj_check <- function(traj) {
  source("data-raw/debug-scripts/II_debug_workflow.R")
  latest_output <- get_latest_timestamped_folder("workspace/st_outputs")
  old_traj <- readr::read_csv(OLD_TRAJ_PATH) %>%
    dplyr::select(
      company_id,
      company_name,
      ald_sector,
      ald_business_unit,
      year,
      production_plan_company_technology,
      production_baseline_scenario,
      production_target_scenario,
      production_shock_scenario
    ) %>%
    dplyr::rename(
      sector = ald_sector,
      technology = ald_business_unit
    )

  new_traj <- readr::read_csv(paste0(latest_output, "/company_trajectories.csv")) %>%
    dplyr::select(
      company_id,
      company_name,
      sector,
      technology,
      year,
      production_plan_company_technology,
      production_baseline_scenario,
      production_target_scenario,
      production_shock_scenario
    )



  mix <- dplyr::inner_join(old_traj, new_traj, by = dplyr::join_by(
    company_id,
    company_name,
    sector,
    technology,
    year
  ))

  # Assuming your dataframe is named mix

  # Function to compute RMSE
  compute_rmse <- function(x, y) {
    sqrt(mean((x - y)^2, na.rm = TRUE))
  }

  # Function to process and plot data for each root
  process_and_plot <- function(root) {
    x_col <- paste0(root, ".x")
    y_col <- paste0(root, ".y")

    # Compute RMSE
    rmse <- mix %>%
      select(company_id, company_name, sector, technology, year, x_col, y_col) %>%
      group_by(company_id, company_name, sector, technology) %>%
      summarise(RMSE = compute_rmse(!!sym(x_col), !!sym(y_col)), .groups = "drop") %>%
      mutate(root = root)

    # Select the top 10 RMSEs
    top_rmse <- rmse %>%
      arrange(desc(RMSE), company_id) %>%
      slice_head(n = 6)

    # Join with original data to get values for plotting
    plot_data <- mix %>%
      pivot_longer(
        cols = -c(company_id, company_name, sector, technology, year),
        names_to = c("root", "set"), names_pattern = "(.*)\\.(.*)"
      ) %>%
      filter(.data$root == root) %>%
      inner_join(top_rmse, by = c("company_id", "company_name", "sector", "technology", "root"))

    # Plotting
    p <- ggplot(plot_data, aes(x = year, y = value, group = set, color = set)) +
      geom_line() +
      facet_wrap(~ company_id + company_name + sector + technology + root, scales = "free_y") +
      labs(title = paste("Top 10 RMSE for", root, "- by Group"), x = "Year", y = "Value") +
      theme_minimal()

    return(p)
  }

  print(process_and_plot(traj))
}

# npv_check()

# traj_check("production_plan_company_technology")
# traj_check("production_baseline_scenario")
# traj_check("production_target_scenario")
traj_check("production_shock_scenario")
