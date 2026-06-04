# Build the latest TRISK scenario providers in PACTA production format (smsp / tmsr),
# SPLIT INTO TWO FILES, each re-based to its own start year.
# -----------------------------------------------------------------------------
# Provenance: developed in trisk.analysis (feat/pacta-scenario-input) and relocated
#   here, as it produces the smsp/tmsr scenario representation that trisk.model
#   consumes. Run interactively; not part of the package build.
#
# Source : trisk.r.docker/data/input/scenarios.csv (14-col TRISK format, ABSOLUTE
#          scenario_pathway).
# Output : two files following the source-data ("PACTA") naming convention
#          "{vintage} {sources} {sectors} (start year {start}).csv":
#
#   File 1 - NGFS only, start year 2024:
#     "2024 NGFS Fossil Fuel Power (start year 2024).csv"
#       NGFS2024 GCAM/MESSAGE/REMIND ; sectors power, coal, oil and gas
#
#   File 2 - the rest, start year 2023:
#     "2023 IPR GECO Mission Possible Auto Fossil Fuel Power Steel (start year 2023).csv"
#       IPR2023, IPR2023Automotive, GECO2023, mission_possible
#       sectors power, coal, oil and gas, automotive, steel
#
# Each file is re-based to its own start year (base year t0). tmsr/smsp are defined
# relative to t0, so the base row has tmsr=1, smsp=0:
#   tmsr = pathway_t / pathway_t0                       -- high-carbon (carbontech) techs
#   smsp = (pathway_t - pathway_t0) / sector_total_t0   -- low-carbon  (greentech)  techs
# Horizon t0..2030.  (PACTA for Banks v1.2.3 ss 2.4.3/2.4.4; checked vs r2dii
# target_market_share and the internal consistency of the supplied template files.)
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

scenarios_path <- "/Users/jakub/Documents/repos/trisk.r.docker/data/input/scenarios.csv"
out_dir        <- "/Users/jakub/Documents/repos/trisk.model"
end_year       <- 2030L

# TRISK sector -> PACTA template vocabulary (lowercase). Sectors not listed are
# dropped on filter (this is how NGFS2024 Cement is excluded).
sector_map <- c(
  "Power"      = "power",
  "Coal"       = "coal",
  "Oil&Gas"    = "oil and gas",
  "Automotive" = "automotive",
  "Steel"      = "steel"
)

raw <- read_csv(scenarios_path, show_col_types = FALSE)

# --- one self-contained build per output file --------------------------------
build_pacta_file <- function(raw, providers, start_year, filename) {
  scen_long <- raw |>
    filter(
      .data$scenario_provider %in% providers,
      .data$sector %in% names(sector_map),
      .data$scenario_year >= start_year,
      .data$scenario_year <= end_year
    ) |>
    transmute(
      scenario_source = str_to_lower(.data$scenario_provider),
      region          = str_to_lower(.data$scenario_geography),
      scenario        = str_to_lower(str_remove(.data$scenario, paste0(.data$scenario_provider, "_"))),
      sector          = unname(sector_map[.data$sector]),
      technology      = str_to_lower(.data$technology),
      year            = as.integer(.data$scenario_year),
      scenario_pathway = .data$scenario_pathway
    )

  # Drop corrupted series: IPR2023 / IPR2023Automotive "India" carry two conflicting
  # absolute pathways for the same key with no distinguishing column. An ambiguous
  # base year breaks tmsr/smsp and the sector total, so drop the affected
  # (source, region) blocks and report. Detection is generic.
  dropped_regions <- scen_long |>
    count(.data$scenario_source, .data$region, .data$scenario, .data$sector,
          .data$technology, .data$year, name = "n") |>
    filter(.data$n > 1) |>
    distinct(.data$scenario_source, .data$region)
  scen_long <- scen_long |> anti_join(dropped_regions, by = c("scenario_source", "region"))

  stopifnot("duplicate scenario keys remain after de-dup" =
    (scen_long |>
       count(.data$scenario_source, .data$region, .data$scenario, .data$sector,
             .data$technology, .data$year, name = "n") |>
       summarise(m = max(.data$n)) |> pull(.data$m)) == 1)

  # Every series must include the base year, else tmsr/smsp are ill-defined.
  stopifnot("every technology series must start at the base year" =
    (scen_long |>
       group_by(.data$scenario_source, .data$region, .data$scenario, .data$sector, .data$technology) |>
       summarise(min_year = min(.data$year), .groups = "drop") |>
       summarise(ok = all(.data$min_year == start_year)) |> pull(.data$ok)))

  base_tech <- scen_long |>
    filter(.data$year == start_year) |>
    select(scenario_source, region, scenario, sector, technology,
           first_pathway = scenario_pathway)
  base_sector <- base_tech |>
    group_by(.data$scenario_source, .data$region, .data$scenario, .data$sector) |>
    summarise(sector_total_first = sum(.data$first_pathway), .groups = "drop")

  production_full <- scen_long |>
    left_join(base_tech, by = c("scenario_source", "region", "scenario", "sector", "technology")) |>
    left_join(base_sector, by = c("scenario_source", "region", "scenario", "sector")) |>
    mutate(
      # Template convention for a zero base-year pathway: pathway>0 over 0 -> Inf;
      # 0/0 -> 1 ("no change"; only hits greentech/SMSP-driven techs, tmsr unused).
      tmsr = .data$scenario_pathway / .data$first_pathway,
      tmsr = if_else(is.nan(.data$tmsr), 1, .data$tmsr),
      smsp = if_else(.data$sector_total_first == 0, NA_real_,
                     (.data$scenario_pathway - .data$first_pathway) / .data$sector_total_first)
    ) |>
    arrange(.data$scenario_source, .data$region, .data$scenario,
            .data$sector, .data$technology, .data$year)

  # assertions
  base_rows <- production_full |> filter(.data$year == start_year)
  stopifnot(
    "tmsr must equal 1 at base year (finite rows)" =
      all(abs(base_rows$tmsr[is.finite(base_rows$tmsr)] - 1) < 1e-9),
    "smsp must equal 0 at base year" = all(abs(base_rows$smsp) < 1e-9, na.rm = TRUE),
    "non-finite tmsr only where base pathway is 0" =
      all(production_full$first_pathway[!is.finite(production_full$tmsr)] == 0)
  )
  share_ok <- production_full |>
    filter(.data$year == start_year, .data$sector_total_first != 0) |>
    mutate(base_share = .data$first_pathway / .data$sector_total_first) |>
    group_by(.data$scenario_source, .data$region, .data$scenario, .data$sector) |>
    summarise(share_sum = sum(.data$base_share), .groups = "drop")
  stopifnot("per-sector base shares must sum to 1" =
              all(abs(share_ok$share_sum - 1) < 1e-6))

  out <- production_full |>
    select(scenario_source, region, scenario, sector, technology, year, smsp, tmsr)
  out_path <- file.path(out_dir, filename)
  write_csv(out, out_path)

  cat(sprintf("\nWrote %s\n", filename))
  cat("  rows:", nrow(out), "| years:", min(out$year), "-", max(out$year), "\n")
  cat("  sources:", paste(sort(unique(out$scenario_source)), collapse = ", "), "\n")
  cat("  sectors:", paste(sort(unique(out$sector)), collapse = ", "), "\n")
  if (nrow(dropped_regions) > 0) {
    for (i in seq_len(nrow(dropped_regions)))
      cat(sprintf("  DROPPED (source defect): %s / region '%s'\n",
                  dropped_regions$scenario_source[i], dropped_regions$region[i]))
  }
  invisible(out)
}

# File 1: NGFS only, start year 2024.
build_pacta_file(
  raw,
  providers  = c("NGFS2024GCAM", "NGFS2024MESSAGE", "NGFS2024REMIND"),
  start_year = 2024L,
  filename   = "2024 NGFS Fossil Fuel Power (start year 2024).csv"
)

# File 2: the rest, start year 2023.
build_pacta_file(
  raw,
  providers  = c("IPR2023", "IPR2023Automotive", "GECO2023", "mission_possible"),
  start_year = 2023L,
  filename   = "2023 IPR GECO Mission Possible Auto Fossil Fuel Power Steel (start year 2023).csv"
)
