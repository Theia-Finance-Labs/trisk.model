# Read agnostic data from a directory

This function reads scenario, financial, assets, and carbon price data
from CSV files located in a specified directory.

## Usage

``` r
st_read_agnostic(dir)
```

## Arguments

- dir:

  A string representing the directory where the CSV files are stored.

## Value

A list containing four elements: `scenarios_data`, `financial_data`,
`assets_data`, and `carbon_data`.
