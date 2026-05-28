# data-raw/03_extend_scenarios_testdata.R
#
# Extends inst/testdata/scenarios_testdata.csv with NGFS2023 IAM and ambition
# variants so the sensitivity-analysis vignette in trisk.analysis can demo
# IAM and ambition sensitivity on bundled data. Existing rows are preserved
# (the existing NGFS2023GCAM_* scenarios are referenced by the
# snapshot-continuity test and other vignettes; do not touch them).

library(magrittr)

TRISK_INPUTS_BUCKET_URL <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs"
EXISTING_TESTDATA <- "inst/testdata/scenarios_testdata.csv"

NEW_SCENARIOS <- c(
  # IAM sensitivity demo (GCAM is already present via NGFS2023GCAM_*; add REMIND + MESSAGE
  # under the underscore namespace, with both CP and NZ2050 ambitions so vignette can
  # hold ambition fixed while varying IAM)
  "NGFS2023_GCAM_CP", "NGFS2023_GCAM_NZ2050",
  "NGFS2023_REMIND_CP", "NGFS2023_REMIND_NZ2050",
  "NGFS2023_MESSAGE_CP", "NGFS2023_MESSAGE_NZ2050",
  # Ambition sensitivity demo (GCAM only; CP and NZ2050 are above, add B2DS and DT)
  "NGFS2023_GCAM_B2DS", "NGFS2023_GCAM_DT"
)

# Step A: Download the canonical upstream scenarios file
upstream_path <- tempfile(fileext = ".csv")
utils::download.file(file.path(TRISK_INPUTS_BUCKET_URL, "scenarios.csv"),
                     upstream_path, mode = "wb")
upstream <- readr::read_csv(upstream_path, show_col_types = FALSE)

# Step B: Subset to the new scenarios, Global only
new_rows <- upstream %>%
  dplyr::filter(.data$scenario %in% NEW_SCENARIOS,
                .data$scenario_geography == "Global")
stopifnot("Expected at least one row per requested scenario." =
            length(setdiff(NEW_SCENARIOS, unique(new_rows$scenario))) == 0)

# Step C: Append to the existing testdata file
existing <- readr::read_csv(EXISTING_TESTDATA, show_col_types = FALSE)
stopifnot("Schema mismatch between upstream and existing testdata." =
            identical(sort(colnames(existing)), sort(colnames(new_rows))))

combined <- dplyr::bind_rows(existing, new_rows[, colnames(existing)]) %>%
  dplyr::distinct()
readr::write_csv(combined, EXISTING_TESTDATA)

cat("Wrote ", nrow(combined), " rows to ", EXISTING_TESTDATA,
    " (was ", nrow(existing), ").\n", sep = "")
