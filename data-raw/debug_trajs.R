
# Load required libraries for plotting
library(ggplot2)
library(stringr)
library(dplyr)
devtools::load_all()


baseline_scenario <- "NGFS2024GCAM_CP"
target_scenario <- "NGFS2024GCAM_NZ2050"
scenario_geography <- "Global"

assets_data <- readr::read_csv(fs::path("workspace", "ST_INPUTS_IFC", "assets.csv"))
scenarios_data <- readr::read_csv(fs::path("workspace", "ST_INPUTS_IFC", "scenarios.csv"))
financial_data <- readr::read_csv(fs::path("workspace", "ST_INPUTS_IFC", "financial_features.csv"))
ngfs_carbon_price_data <- readr::read_csv(fs::path("workspace", "ST_INPUTS_IFC", "ngfs_carbon_price.csv"))


trisk_results <- run_trisk_model(
    assets_data=assets_data ,
    scenarios_data=scenarios_data,
    financial_data=financial_data,
    carbon_data=ngfs_carbon_price_data,
    baseline_scenario = baseline_scenario,
    target_scenario = target_scenario,
    scenario_geography = scenario_geography)



# Function to plot company trajectories
plot_company_trajectories <- function(trajectories_data, output_base_dir = file.path(Sys.getenv("OUTPUT_BASE_DIR", unset = "workspace"), "company_plots")) {
  
  # Create base directory
  if (!dir.exists(output_base_dir)) {
    dir.create(output_base_dir, recursive = TRUE)
  }
  
  # Clean names for file system
  clean_name_for_folder <- function(name) {
    if (is.na(name)) return("Unknown")
    # Replace special characters with underscores and limit length
    cleaned <- str_replace_all(name, '[<>:"/\\\\|?*]', "_")
    cleaned <- str_replace_all(cleaned, "[^\\w\\s-]", "_")
    cleaned <- str_replace_all(cleaned, "\\s+", "_")
    return(substr(cleaned, 1, 100))  # Limit length
  }
  
  # Filter data for plotting (limit to 2050 and remove any missing essential data)
  trajectories_data_filtered <- trajectories_data %>%
    filter(year <= 2050) %>%
    filter(!is.na(company_name), !is.na(technology), !is.na(year))
  
  # Prepare data for plotting
  plot_data <- trajectories_data_filtered %>%
    mutate(
      company_name_clean = sapply(company_name, clean_name_for_folder),
      technology_clean = sapply(technology, clean_name_for_folder)
    ) %>%
    # Remove any rows with missing essential data
    filter(
      !is.na(company_name_clean), 
      !is.na(technology_clean), 
      !is.na(year)
    )
  
  # Create output directory structure
  cat("Output directory:", output_base_dir, "\n")
  
  # Prepare data for dual-axis plotting (NPV + Production)
  plot_data_npv <- plot_data %>%
    select(
      company_id, company_name, company_name_clean, technology, technology_clean,
      year, discounted_net_profits_baseline_scenario, discounted_net_profits_shock_scenario
    ) %>%
    tidyr::pivot_longer(
      cols = c(discounted_net_profits_baseline_scenario, discounted_net_profits_shock_scenario),
      names_to = "trajectory_type",
      values_to = "value"
    ) %>%
    mutate(
      trajectory_type = case_when(
        trajectory_type == "discounted_net_profits_baseline_scenario" ~ "Discounted NPV Baseline", 
        trajectory_type == "discounted_net_profits_shock_scenario" ~ "Discounted NPV Shock",
        TRUE ~ trajectory_type
      ),
      data_type = "NPV"
    ) %>%
    filter(!is.na(value))
  
  plot_data_production <- plot_data %>%
    select(
      company_id, company_name, company_name_clean, technology, technology_clean,
      year, production_baseline_scenario, production_target_scenario, production_shock_scenario
    ) %>%
    tidyr::pivot_longer(
      cols = c(production_baseline_scenario, production_target_scenario, production_shock_scenario),
      names_to = "trajectory_type",
      values_to = "value"
    ) %>%
    mutate(
      trajectory_type = case_when(
        trajectory_type == "production_baseline_scenario" ~ "Production Baseline",
        trajectory_type == "production_target_scenario" ~ "Production Target", 
        trajectory_type == "production_shock_scenario" ~ "Production Shock",
        TRUE ~ trajectory_type
      ),
      data_type = "Production"
    ) %>%
    filter(!is.na(value))
  
  # Combine NPV and Production data
  plot_data_long <- bind_rows(plot_data_npv, plot_data_production)
  
  # Group by company and technology for plotting
  group_combinations <- plot_data_long %>%
    distinct(company_id, company_name, company_name_clean, technology, technology_clean)
  
  cat("Creating", nrow(group_combinations), "trajectory plots...\n")
  
  # Plot for each combination
  for (i in 1:nrow(group_combinations)) {
    
    combo <- group_combinations[i, ]
    
    # Filter data for this combination
    combo_data <- plot_data_long %>%
      filter(
        company_id == combo$company_id,
        technology == combo$technology
      ) %>%
      arrange(year)
    
    if (nrow(combo_data) == 0) next
    
    # Create filename starting with technology
    filename <- paste0(
      combo$technology_clean, "_",
      combo$company_name_clean,
      ".png"
    )
    
    # Set file path
    filepath <- file.path(output_base_dir, filename)
    
    # Split data by type for dual-axis plotting
    npv_data <- combo_data %>% filter(data_type == "NPV")
    production_data <- combo_data %>% filter(data_type == "Production")
    
    # Initialize default ranges
    npv_range <- c(0, 1)
    prod_range <- c(0, 1)
    scale_factor <- 1
    
    # Calculate scaling factor for secondary axis
    if (nrow(npv_data) > 0 && nrow(production_data) > 0) {
      npv_range <- range(npv_data$value, na.rm = TRUE)
      prod_range <- range(production_data$value, na.rm = TRUE)
      
      # Check for valid ranges
      npv_diff <- diff(npv_range)
      prod_diff <- diff(prod_range)
      
      if (prod_diff > 0 && npv_diff > 0 && !is.na(prod_diff) && !is.na(npv_diff)) {
        scale_factor <- npv_diff / prod_diff
        # Scale production data to match NPV range
        production_data_scaled <- production_data %>%
          mutate(value_scaled = (value - min(prod_range)) * scale_factor + min(npv_range))
      } else {
        production_data_scaled <- production_data %>% mutate(value_scaled = value)
      }
    } else {
      production_data_scaled <- production_data %>% mutate(value_scaled = value)
    }
    
    # Create the plot with dual axes
    p <- ggplot() +
      # NPV trajectories (primary axis)
      geom_line(data = npv_data, aes(x = year, y = value, color = trajectory_type), 
                size = 1.2, alpha = 0.8) +
      geom_point(data = npv_data, aes(x = year, y = value, color = trajectory_type), 
                 size = 2, alpha = 0.7) +
      # Production trajectories (scaled to primary axis)
      geom_line(data = production_data_scaled, aes(x = year, y = value_scaled, color = trajectory_type), 
                size = 1.2, alpha = 0.8, linetype = "dashed") +
      geom_point(data = production_data_scaled, aes(x = year, y = value_scaled, color = trajectory_type), 
                 size = 2, alpha = 0.7, shape = 17) +
      scale_color_manual(
        values = c(
          "Discounted NPV Baseline" = "#1f77b4",
          "Discounted NPV Shock" = "#ff7f0e",
          "Production Baseline" = "#2ca02c",
          "Production Target" = "#9467bd", 
          "Production Shock" = "#d62728"
        )
      ) +
      # Secondary axis for production
      scale_y_continuous(
        name = "Net Present Value",
        sec.axis = if(nrow(production_data) > 0 && !is.infinite(scale_factor) && scale_factor != 0) {
          sec_axis(~ (. - min(npv_range)) / scale_factor + min(prod_range), 
                   name = "Production/Activity")
        } else {
          NULL
        }
      ) +
      labs(
        title = paste0(combo$company_name, "\n", combo$technology),
        subtitle = "Solid lines = NPV, Dashed lines = Production",
        x = "Year",
        color = "Trajectory Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
    
    # Save the plot
    tryCatch({
      ggsave(
        filename = filepath,
        plot = p,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white"
      )
      
      if (i %% 10 == 0) {
        cat("Saved", i, "plots...\n")
      }
      
    }, error = function(e) {
      cat("Error saving plot for", combo$company_name, "-", combo$technology, ":", e$message, "\n")
    })
  }
  
  cat("Plotting completed! Company trajectory plots saved in:", output_base_dir, "\n")
  cat("Total plots created:", nrow(group_combinations), "\n")
}

# Create trajectory plots for all companies
cat("\n\n=== CREATING TRAJECTORY PLOTS ===\n")

selected_trajs <- trisk_results$company_trajectories %>% dplyr::filter(asset_id %in% c("10090_2", "11154_1"))

plot_company_trajectories(selected_trajs)
