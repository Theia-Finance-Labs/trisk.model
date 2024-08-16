

# Load necessary package
if (!require("httr")) {
  install.packages("httr")
  library(httr)
}

# Define the output directory
output_dir <- "workspace/st_inputs_trisk_v2"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# URLs of the files
urls <- c(
  "https://trisk-input-files.fra1.digitaloceanspaces.com/trisk-input-files//app/trisk2_inputs/csv/assets.csv",
  "https://trisk-input-files.fra1.digitaloceanspaces.com/trisk-input-files//app/trisk2_inputs/csv/financial_features.csv",
  "https://trisk-input-files.fra1.digitaloceanspaces.com/trisk-input-files//app/trisk2_inputs/csv/scenarios.csv"
)

# File names for saving the files
file_names <- c(
  "assets.csv",
  "financial_features.csv",
  "scenarios.csv"
)

# Full paths where the files will be saved
file_paths <- file.path(output_dir, file_names)

# Loop through each URL and download the file
for (i in 1:length(urls)) {
  response <- GET(urls[i])
  
  # Check if the download was successful
  if (status_code(response) == 200) {
    # Save the content to a file
    writeBin(content(response, "raw"), file_paths[i])
    cat("Downloaded:", file_paths[i], "\n")
  } else {
    cat("Failed to download:", file_paths[i], "\n")
  }
}

devtools::load_all()
run_trisk(
  input_path = output_dir,
  output_path = "workspace/st_outputs",
  baseline_scenario = "Steel_baseline",
  target_scenario = "Steel_NZ",
  scenario_geography = "Global"
)