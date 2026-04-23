# trisk-run-from-folder

    #> Files have been copied and renamed in ./trisk_inputs.

## Input folder structure

The files inside the `trisk_inputs` folder are now structured as
follows:

``` r
dir_tree(trisk_input_dir)
#> ./trisk_inputs
#> ├── assets.csv
#> ├── financial_features.csv
#> ├── ngfs_carbon_price.csv
#> └── scenarios.csv
```

## Generate outputs using the Trisk model

``` r
# Define the scenarios to use
baseline_scenario <- "NGFS2023GCAM_CP"
target_scenario <- "NGFS2023GCAM_NZ2050"
scenario_geography <- "Global"
```

Run the model with the downloaded data :

``` r
saved_path <- run_trisk(
  input_path = trisk_input_dir,
  output_path = trisk_output_dir,
  baseline_scenario = baseline_scenario,
  target_scenario = target_scenario,
  scenario_geography = scenario_geography
)
#> -- Retyping Dataframes. 
#> -- Processing Assets and Scenarios. 
#> -- Transforming to Trisk model input. 
#> -- Calculating baseline, target, and shock trajectories. 
#> -- Applying zero-trajectory logic to production trajectories. 
#> -- Calculating net profits.
#> Joining with `by = join_by(asset_id, company_id, sector, technology)`
#> -- Calculating market risk. 
#> -- Calculating credit risk. 
#> [1] "Outputs saved in folder: ./trisk_outputs/20260423_221025__874b4c69-e909-4b3a-84a7-473d82f2687d"
```

Each new run generates a folder with a unique name. The folders are
named following this convention : DATE_TIME\_\_RUNID. The full path of
this run is :

    #> [1] "/home/runner/work/trisk.model/trisk.model/vignettes/trisk_outputs/20260423_221025__874b4c69-e909-4b3a-84a7-473d82f2687d"

## Output folder structure

The generated outputs are saved in the `trisk_outputs` folder,
structured as:

``` r
dir_tree(trisk_output_dir)
#> ./trisk_outputs
#> └── 20260423_221025__874b4c69-e909-4b3a-84a7-473d82f2687d
#>     ├── company_trajectories.csv
#>     ├── npv_results.csv
#>     ├── params.csv
#>     └── pd_results.csv
```
