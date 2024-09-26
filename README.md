  <!-- badges: start -->
  [![R-CMD-check](https://github.com/Theia-Finance-Labs/trisk.model/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Theia-Finance-Labs/trisk.model/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# trisk.model

This repository provides a comprehensive toolkit for conducting transition risk stress tests on financial assets. The tools enable users to analyze the impact of various climate scenarios on company trajectories, technology valuations, and default probabilities.


- [trisk.model](#triskmodel)
  - [Prequisites](#prequisites)
    - [Installation](#installation)
    - [Data Setup and Project Structure](#data-setup-and-project-structure)
  - [Main Functions](#main-functions)
    - [`run_trisk()` doc](#run_trisk-doc)
    - [`run_trisk_model()` doc](#run_trisk_model-doc)
  - [See Also](#see-also)

---

## Prequisites

### Installation

You can install this package directly from GitHub using the `remotes` package in R.

1. Install the `remotes` package if you don't have it:

    ```r
    install.packages("remotes")
    ```

2. Install the transition risk stress testing suite from GitHub:

    ```r
    remotes::install_github("Theia-Finance-Labs/trisk.model")
    ```


### Data Setup and Project Structure

The input data files for the stress test must be structured as CSV files. These files should follow a specific naming convention and format to be compatible with the `trisk.model` functions. Here’s the project structure expected:

```plaintext
project_input/
    ├── assets.csv
    ├── financial_features.csv
    ├── ngfs_carbon_price.csv
    └── scenarios.csv
```

1. **assets.csv** - Contains data on company assets, including production, technology, and geographical details.
2. **financial_features.csv** - Contains financial metrics needed for the stress test.
3. **ngfs_carbon_price.csv** - Provides carbon pricing data for the climate scenarios.
4. **scenarios.csv** - Holds the details of the climate scenarios including the baseline and shock scenarios.

For details on columns structure, refer to the [Vignette on Data Overview](https://theia-finance-labs.github.io/trisk.model/doc/data-input-description.html).


## Main Functions

### `run_trisk()` [doc](https://theia-finance-labs.github.io/trisk.model/reference/run_trisk.html)

The primary function for running the transition risk stress test. It processes the input data from specified directories, performs the stress test, and returns key outputs such as company default probability changes and profit trajectories.
> **Note 1**:  It is possible to change the scenario geography but that won't affect the assets being analyzed. Your input dataset should then only contain assets covered by the geography you are analyzing. A more user-friendly approach can be found in the package trisk.analysis.


> **Note 2**: The selected scenario geography needs to be available for the selected baseline and target scenarios. This can be determined from the input scenario data.

**Example Usage:**

```r
# Load the package
library(trisk.model)

# Run the stress test and save results to disk
run_trisk(
  input_path = "path/to/project_input/",
  output_path = "path/to/output",
  baseline_scenario = "NGFS2023GCAM_CP",
  target_scenario = "NGFS2023GCAM_NZ2050",
  scenario_geography = "Global"
)
```

The results can be retrieved in the new folder created inside the output folder you defined as input parameter.


### `run_trisk_model()` [doc](https://theia-finance-labs.github.io/trisk.model/reference/run_trisk_model.html)

This function runs the core transition risk stress test model without handling file read/write operations. It is used to execute the transition risk stress test when data is already loaded into memory. 

> **Note** The results are not saved to disk with this function but are instead returned by the function. They can be then handled for more complex operations.

**Example Usage:**

```r
# Load the package
library(trisk.model)

# Load the internal datasets from the package
data(assets_testdata, package = "trisk.model")
data(scenarios_testdata, package = "trisk.model")
data(financial_features_testdata, package = "trisk.model")
data(carbon_price_testdata, package = "trisk.model")

# Run the transition risk model
results <- run_trisk_model(
  assets_data = assets_testdata,
  scenarios_data = scenarios_testdata,
  financial_data = financial_features_testdata,
  carbon_data = carbon_price_testdata,
  risk_free_rate = 0.02,
  discount_rate = 0.07,
  growth_rate = 0.03,
  div_netprofit_prop_coef = 1,
  shock_year = 2030,
  show_params_cols = TRUE
)
```

## See Also

- Package [trisk.analysis](https://theia-finance-labs.github.io/trisk.analysis/) for complex analyses of Trisk outputs

---

This project is licensed under the GNU License. See the [LICENSE](LICENSE) file for more information.
