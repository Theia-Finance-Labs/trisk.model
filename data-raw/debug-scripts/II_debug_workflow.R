devtools::load_all()

get_latest_timestamped_folder <- function(path) {
  # Check if the path is a directory
  if (!dir.exists(path)) {
    stop("The specified path does not exist or is not a directory.")
  }

  # List all the directories in the specified path
  directories <- list.dirs(path, full.names = FALSE, recursive = FALSE)

  # Filter out only those directories that start with a date in the specified format
  valid_directories <- directories[grepl("^\\d{8}_\\d{6}", directories)]

  # If no valid directories are found, return a message
  if (length(valid_directories) == 0) {
    return("No directories with the specified date format found.")
  }

  # Extract the dates from the directory names
  dates <- as.POSIXct(substring(valid_directories, 1, 15), format = "%Y%m%d_%H%M%S")

  # Find the directory with the newest date
  newest_index <- which.max(dates)
  newest_directory <- valid_directories[newest_index]
  # Return the full relative path
  newest_directory <- file.path(path, newest_directory)


  return(newest_directory)
}



start_time <- Sys.time()

run_trisk(
  input_path = get_latest_timestamped_folder("workspace/st_inputs"),
  output_path = "workspace/st_outputs",
  baseline_scenario = "NGFS2023MESSAGE_NDC",
  target_scenario = "NGFS2023MESSAGE_B2DS",
  scenario_geography = "Global"
)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
