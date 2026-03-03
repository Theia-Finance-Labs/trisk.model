# Defines which scenario values to use for the baseline trajectory in the stress test.

Picks the corresponding values from the original scenario column
indicated in the input and has the option to include PACTA based
production forecast for the first few years of the baseline trajectory.
If included, the trajectory after the end of the production forecast is
offset by the initial production forecast so that the remainder of the
baseline trajectory now is a parallel shift of the original scenario
values. If not included, the trajectories replicate externally provided
scenario trajectories. Trajectories are furthermore differentiated by
scenario_geography, if multiple are passed. If no "company_id" or
"company_name" are provided, the calculation switches to
portfolio/technology level.

## Usage

``` r
set_baseline_trajectory(data)
```

## Arguments

- data:

  A dataframe that contains scenario trajectories by technology until
  2040 for all the scenarios included in the analysis and production
  build out plans by technology or company and technology, usually for 5
  years, based on PACTA results.

## Value

dataframe.

## See also

Other scenario definition:
[`set_trisk_trajectory()`](set_trisk_trajectory.md)
