# Process function parameters

This function extracts the default parameters from a function and
replaces them with user-provided values where applicable. /!\\ It ignore
params without default, or with a NULL default

## Usage

``` r
process_params(fun, ...)
```

## Arguments

- fun:

  A function whose parameters are to be processed.

- ...:

  User-defined arguments to override the function's default parameters.

## Value

A list of final parameters.
