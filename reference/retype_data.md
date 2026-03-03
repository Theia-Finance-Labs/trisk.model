# Retype Data Columns

This function retypes the columns of assets_data, scenarios_data,
financial_data, and carbon_data according to their intended column
types.

## Usage

``` r
retype_data(assets_data, scenarios_data, financial_data, carbon_data)
```

## Arguments

- assets_data:

  A dataframe containing assets data.

- scenarios_data:

  A dataframe containing scenarios data.

- financial_data:

  A dataframe containing financial data.

- carbon_data:

  A dataframe containing carbon price data.

## Value

A list with retyped dataframes: `assets_data`, `scenarios_data`,
`financial_data`, and `carbon_data`.
