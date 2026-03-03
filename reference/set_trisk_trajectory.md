# Defines which scenario values to use for the late & sudden trajectory in the stress test.

Picks the corresponding values from the original scenario column
indicated in the input and has the option to include PACTA based
production forecast for the first few years of the late & sudden
trajectory. Similarly, it is possible to define another input scenario
in case the company is already aligned after the production forecast. If
the production forecast is included, the trajectory after the end of the
production forecast is offset by the initial production forecast so that
the remainder of the late & sudden trajectory now is a parallel shift of
the original scenario values. If not included, the trajectories
replicate externally provided scenario trajectories at least until the
year of the policy shock. Trajectories are calculated for each company
by sector, scenario_geography, technology, year. If no "company_id" or
"company_name" are provided, the calculation switches to
portfolio/technology level.

## Usage

``` r
set_trisk_trajectory(data, start_year, shock_year)
```

## Arguments

- data:

  A dataframe that contains the scenario data prepared until the step
  after the baseline trajectories are calculated.

- start_year:

  Numeric. A numeric vector of length 1 that contains the start year of
  the analysis.

- shock_year:

  Numeric. the shock year.

## Value

data frame

## See also

Other scenario definition:
[`set_baseline_trajectory()`](set_baseline_trajectory.md)
