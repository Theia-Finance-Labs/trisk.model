# data-raw/05_add_ngfs2024_testdata.R
#
# Extends inst/testdata/scenarios_testdata.csv with the NGFS 2024 (Phase 5) GCAM
# multi-sector pair plus the Power-only per-IAM / ambition variants, mirroring the
# 2023 footprint added by 03_extend_scenarios_testdata.R. This lets the
# trisk.analysis bank vignettes migrate their baseline/shock pair to the 2024
# vintage (NGFS2024GCAM_CP vs NGFS2024GCAM_NZ2050) and run IAM / ambition
# sensitivity on 2024 data.
#
# Existing rows are preserved. The NGFS2023* scenarios are referenced by the
# snapshot-continuity test and the current vignettes; this script only appends.

library(magrittr)

TRISK_INPUTS_BUCKET_URL <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs"
EXISTING_TESTDATA <- "inst/testdata/scenarios_testdata.csv"

NEW_SCENARIOS <- c(
  # Main multi-sector pair (mirror of NGFS2023GCAM_CP / _NZ2050): Coal, Oil&Gas,
  # Power (+ Cement, which simply does not join the bundled assets and is dropped).
  "NGFS2024GCAM_CP", "NGFS2024GCAM_NZ2050",
  # IAM sensitivity (Power-only, underscore namespace): GCAM + REMIND + MESSAGE,
  # CP and NZ2050, so the vignette can hold ambition fixed while varying IAM.
  "NGFS2024_GCAM_CP", "NGFS2024_GCAM_NZ2050",
  "NGFS2024_REMIND_CP", "NGFS2024_REMIND_NZ2050",
  "NGFS2024_MESSAGE_CP", "NGFS2024_MESSAGE_NZ2050",
  # Ambition sensitivity (GCAM only): add B2DS and DT alongside CP / NZ2050 above.
  "NGFS2024_GCAM_B2DS", "NGFS2024_GCAM_DT"
)

# Step A: download the canonical upstream scenarios file
upstream_path <- tempfile(fileext = ".csv")
utils::download.file(file.path(TRISK_INPUTS_BUCKET_URL, "scenarios.csv"),
                     upstream_path, mode = "wb")
upstream <- readr::read_csv(upstream_path, show_col_types = FALSE)

# Step B: subset to the new scenarios, Global only
new_rows <- upstream %>%
  dplyr::filter(.data$scenario %in% NEW_SCENARIOS,
                .data$scenario_geography == "Global")
stopifnot("Expected at least one row per requested scenario." =
            length(setdiff(NEW_SCENARIOS, unique(new_rows$scenario))) == 0)

# Step C: append to the existing testdata, preserving existing rows. Require only
# that the existing columns are a subset of upstream (upstream may carry extra
# columns, which we drop) rather than an exact schema match.
existing <- readr::read_csv(EXISTING_TESTDATA, show_col_types = FALSE)
missing_cols <- setdiff(colnames(existing), colnames(new_rows))
stopifnot("Upstream is missing columns present in the existing testdata." =
            length(missing_cols) == 0)

combined <- dplyr::bind_rows(existing, new_rows[, colnames(existing)]) %>%
  dplyr::distinct()
readr::write_csv(combined, EXISTING_TESTDATA)

cat("Wrote ", nrow(combined), " rows to ", EXISTING_TESTDATA,
    " (was ", nrow(existing), "; added ", nrow(combined) - nrow(existing), ").\n", sep = "")
cat("2024 scenarios now present:\n")
print(sort(intersect(NEW_SCENARIOS, unique(combined$scenario))))
