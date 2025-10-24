#!/usr/bin/env Rscript

# ==============================================================================
# Configuration: Set your repository root path here
# ==============================================================================
# For Windows, use forward slashes or double backslashes
# Example: "C:/Users/YourName/path/to/trisk.model"
# or: "C:\\Users\\YourName\\path\\to\\trisk.model"

repo_root <- "C:/path/to/trisk.model"  # CHANGE THIS PATH

# ==============================================================================
# Script: Source all R files in the R/ folder
# ==============================================================================

# Construct path to R folder
r_folder <- file.path(repo_root, "R")

# Check if R folder exists
if (!dir.exists(r_folder)) {
  stop(sprintf("R folder not found at: %s", r_folder))
}

# List all .R files in the R folder
r_files <- list.files(
  path = r_folder,
  pattern = "\\.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Check if any R files were found
if (length(r_files) == 0) {
  warning(sprintf("No R files found in: %s", r_folder))
} else {
  cat(sprintf("Found %d R file(s) in %s\n\n", length(r_files), r_folder))
  
  # Source each file
  for (file in r_files) {
    cat(sprintf("Sourcing: %s\n", basename(file)))
    tryCatch(
      {
        source(file)
        cat("  ✓ Success\n")
      },
      error = function(e) {
        cat(sprintf("  ✗ Error: %s\n", e$message))
      }
    )
  }
  
  cat("\n=== Completed sourcing all R files ===\n")
}

