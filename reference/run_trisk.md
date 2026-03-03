# Run stress testing for provided asset type

This function executes the transition risk stress test. It reads data
from the input path, runs the transition risk model using the provided
parameters, and writes results to the output path if provided.

## Usage

``` r
run_trisk(input_path, output_path, show_params_cols = TRUE, ...)
```

## Arguments

- input_path:

  String containing the path to project-agnostic data.

- output_path:

  String containing the path to which output files are written. If NULL,
  the function returns the results list. Results and logs per run are
  saved to a subdirectory of `output_path` that will be generated
  automatically with a timestamp.

- show_params_cols:

  Logical indicating whether parameter columns should be shown in the
  output. Default is TRUE.

- ...:

  Additional arguments passed to [`run_trisk_model`](run_trisk_model.md)
  (clic for parameters detail).

## Value

Character, folder path containing output dataframes
