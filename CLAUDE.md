# trisk.model

> **CLIENT REPO (Theia Finance Labs).** No autonomous, headless, or
> scheduled agents. Interactive sessions with Jakub only; never push,
> tag, release, or modify CI/deployment without Jakub in the loop.

## Why

Climate transition risk is a key concern for financial institutions
holding equity, bond, and loan portfolios. TRISK (Transition Risk Stress
Test) quantifies how climate policy scenarios - carbon taxes, technology
shifts, energy transitions - translate into financial losses at the
company and asset level. This package is the core calculation engine
behind Theia Finance Labs’ stress testing methodology, used by APL and
partners to assess portfolio-level climate risk exposure.

## What

An R package that takes four input datasets (assets, scenarios,
financial features, carbon prices) and produces three outputs:

- **NPV results** - Net present value changes per company/technology
  under shock vs. baseline scenarios
- **PD results** - Probability of default changes using Merton
  structural credit model
- **Company trajectories** - Year-by-year production and profit paths
  under baseline, target, and shock scenarios

**Key pipeline** (in
[`run_trisk_model()`](reference/run_trisk_model.md)): 1. Retype &
validate inputs -\> 2. Merge assets with scenario pathways -\> 3.
Compute TRISK trajectories (baseline/target/shock) -\> 4. Calculate net
profits (incl. carbon tax impact) -\> 5. Discount via DCF -\> 6. Derive
market risk (NPV/VaR) and credit risk (PD changes via Merton)

**Main entry points:** [`run_trisk()`](reference/run_trisk.md) (file I/O
wrapper) and [`run_trisk_model()`](reference/run_trisk_model.md)
(dataframe in/out).

## How

- **Language:** R (\>= 3.5), tidyverse stack (dplyr, tidyr, purrr,
  readr, tibble)
- **Package structure:** Standard R package with roxygen2 docs, testthat
  tests, pkgdown site
- **Source layout:** `R/` contains ~15 modules following a `calc_*`,
  `proc_*`, `read_*` naming convention
- **Tests:** Snapshot-based continuity test comparing model outputs
  against saved `.rds` baseline (gated behind `R_USE_TESTS=TRUE` env
  var)
- **CI:** GitHub Actions R-CMD-check on macOS, Windows, Ubuntu (release,
  devel, oldrel)
- **Repo:** `Theia-Finance-Labs/trisk.model` on GitHub

## Workflow Commands

``` bash
# Install dependencies
Rscript -e 'pak::pak(".")'

# Run R CMD check (the CI check)
Rscript -e 'devtools::check()'

# Run tests
Rscript -e 'devtools::test()'

# Run tests including snapshot continuity test
R_USE_TESTS=TRUE Rscript -e 'devtools::test()'

# Build documentation
Rscript -e 'devtools::document()'

# Build pkgdown site
Rscript -e 'pkgdown::build_site()'
```

## Conventions

- Pipe-heavy tidyverse style with `%>%` (magrittr)
- Use `.data$col` pronoun for tidy evaluation (rlang)
- Function naming: `calc_*` (calculations), `proc_*`
  (processing/transforms), `read_*` (I/O)
- Test data lives in `inst/testdata/` as CSV files
- Version follows semver (see DESCRIPTION)
