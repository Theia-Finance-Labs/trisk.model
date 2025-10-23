#!/usr/bin/env Rscript

# ==============================================================================
# Configuration: Set your repository root path here
# ==============================================================================
# For Windows, use forward slashes or double backslashes
# Example: "C:/Users/YourName/path/to/trisk.model"
# or: "C:\\Users\\YourName\\path\\to\\trisk.model"
# Use "." for current directory

repo_root <- "C:/path/to/trisk.model"  # CHANGE THIS PATH

# ==============================================================================
# Script: Install all packages from DESCRIPTION file
# ==============================================================================

# Construct path to DESCRIPTION file
description_file <- file.path(repo_root, "DESCRIPTION")

# Check if DESCRIPTION file exists
if (!file.exists(description_file)) {
  stop(sprintf("DESCRIPTION file not found at: %s", description_file))
}

cat("Reading DESCRIPTION file...\n")

# Read the DESCRIPTION file
desc_content <- readLines(description_file)

# Function to extract packages from a section
extract_packages <- function(content, section_name) {
  # Find the line where the section starts
  section_start <- grep(paste0("^", section_name, ":"), content)
  
  if (length(section_start) == 0) {
    return(character(0))
  }
  
  # Find where the section ends (next section or end of file)
  section_end <- length(content)
  next_sections <- grep("^[A-Z][a-zA-Z]+:", content)
  next_sections <- next_sections[next_sections > section_start]
  if (length(next_sections) > 0) {
    section_end <- next_sections[1] - 1
  }
  
  # Extract the section content
  section_content <- content[section_start:section_end]
  
  # Remove the section header
  section_content <- section_content[-1]
  
  # Extract package names (remove version constraints and clean up)
  packages <- gsub("^\\s+", "", section_content)  # Remove leading whitespace
  packages <- gsub(",\\s*$", "", packages)         # Remove trailing commas
  packages <- gsub("\\s*\\(.*\\)", "", packages)   # Remove version constraints
  packages <- packages[packages != ""]             # Remove empty strings
  packages <- packages[!grepl("^[A-Z][a-zA-Z]+:", packages)]  # Remove section headers
  
  return(packages)
}

# Extract Imports and Suggests
imports <- extract_packages(desc_content, "Imports")
suggests <- extract_packages(desc_content, "Suggests")

# Combine all packages
all_packages <- unique(c(imports, suggests))

if (length(all_packages) == 0) {
  warning("No packages found in DESCRIPTION file")
} else {
  cat(sprintf("\nFound %d package(s) to install:\n", length(all_packages)))
  cat(paste("  -", all_packages, collapse = "\n"), "\n\n")
  
  # Ask for confirmation
  cat("Do you want to install these packages? [y/N]: ")
  response <- tolower(trimws(readLines(con = "stdin", n = 1)))
  
  if (response %in% c("y", "yes")) {
    cat("\nInstalling packages...\n\n")
    
    # Install each package
    for (pkg in all_packages) {
      cat(sprintf("Installing %s...\n", pkg))
      tryCatch(
        {
          if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
            install.packages(pkg, dependencies = TRUE)
            cat(sprintf("  ✓ %s installed successfully\n", pkg))
          } else {
            cat(sprintf("  ✓ %s already installed\n", pkg))
          }
        },
        error = function(e) {
          cat(sprintf("  ✗ Error installing %s: %s\n", pkg, e$message))
        }
      )
    }
    
    cat("\n=== Package installation completed ===\n")
  } else {
    cat("\nInstallation cancelled.\n")
  }
}

