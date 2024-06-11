# Install and load the necessary package
if (!requireNamespace("aws.s3", quietly = TRUE)) {
  install.packages("aws.s3")
}
library(aws.s3)

# # Set your S3 bucket credentials
# Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY,
#            "AWS_SECRET_ACCESS_KEY" = AWS_SECRET,
#            "AWS_DEFAULT_REGION" = AWS_BUCKET_DEFAULT_REGION)


# Function to download files from a DigitalOcean Space bucket into a timestamped subfolder
download_files_with_timestamp <- function(bucket_name, local_directory, base_url) {
  # List all objects in the bucket
  objects <- get_bucket(bucket_name, base_url = base_url)

  if (is.null(objects)) {
    stop("Failed to retrieve bucket contents.")
  }

  # Create a timestamped subfolder within the local directory
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  timestamped_directory <- file.path(local_directory, timestamp)

  # Ensure the timestamped directory exists
  if (!dir.exists(timestamped_directory)) {
    dir.create(timestamped_directory, recursive = TRUE)
  }

  # Iterate over each object in the bucket
  for (object in objects) {
    file_name <- object$Key
    local_path <- file.path(timestamped_directory, file_name)

    # Ensure the directory structure exists for the file
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)

    # Download the file
    save_object(file_name, bucket = bucket_name, file = local_path, base_url = base_url)
    message(paste("Downloaded:", file_name))
  }
}

# Usage
bucket_name <- "trisk-input-files"
local_directory <- "./workspace/st_inputs"
base_url  <- "digitaloceanspaces.com"

download_files_with_timestamp(bucket_name, local_directory, base_url)
