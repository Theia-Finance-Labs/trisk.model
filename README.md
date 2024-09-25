# trisk.model

This repository provides a comprehensive toolkit for conducting transition risk stress tests on financial assets. The tools enable users to analyze the impact of various climate scenarios on company trajectories, technology valuations, and default probabilities.

## Table of Contents

- [trisk.model](#triskmodel)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
    - [Steps:](#steps)
  - [Prequisites](#prequisites)
    - [Data Setup and Project Structure](#data-setup-and-project-structure)
    - [Loading Data](#loading-data)
  - [Core Functions](#core-functions)
    - [`run_trisk()`](#run_trisk)
    - [`run_trisk_model()`](#run_trisk_model)
  - [License](#license)

---

## Installation

You can install this package directly from GitHub using the `remotes` package in R.

### Steps:

1. Install the `remotes` package if you don't have it:

    ```{r}
    install.packages("remotes")
    ```

2. Install the transition risk stress testing suite from GitHub:

    ```{r}
    remotes::install_github("Theia-Finance-Labs/trisk.model")
    ```

---

## Prequisites

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

For more details on how to structure the input data, refer to the [Vignette on Data Overview](https://theia-finance-labs.github.io/trisk.model/articles/trisk-data-overview.html).


### Loading Data

In the `trisk.model` package, you can also load the prepackaged example datasets to run quick tests or model executions. These datasets are stored internally and can be loaded with:

```{r}
data(assets_testdata, package = "trisk.model")
data(scenarios_testdata, package = "trisk.model")
data(financial_features_testdata, package = "trisk.model")
data(carbon_price_testdata, package = "trisk.model")
```

If you are using your own CSV files, place them in a directory with the above structure and point to that directory in the `input_path` parameter when running the `run_trisk()` function.

---

## Core Functions

### `run_trisk()`

The primary function for running the transition risk stress test. It processes the input data from specified directories, performs the stress test, and returns key outputs such as company default probability changes and profit trajectories.

**Key Parameters:**

- `input_path`: Path to the directory containing input data.
- `output_path`: Directory to store the output files. If not provided, results are returned as a list.
- `baseline_scenario`: String specifying the name of the baseline scenario.
- `target_scenario`: String specifying the name of the shock scenario.
- `scenario_geography`: Character vector indicating which geographical region(s) to calculate results for.
- `carbon_price_model`: Character vector specifying which NGFS model to use for carbon prices. Default is `"no_carbon_tax"`.
- `risk_free_rate`: Numeric value for the risk-free interest rate. Default is `0.02`.
- `discount_rate`: Numeric value for the discount rate of dividends per year in the DCF. Default is `0.07`.
- `growth_rate`: Numeric value for the terminal growth rate of profits beyond the final year in the DCF. Must be positive and less than `discount_rate`. Default is `0.03`.
- `div_netprofit_prop_coef`: Numeric coefficient determining how strongly future dividends propagate to company value. Default is `1`.
- `shock_year`: Numeric value specifying the year when the shock is applied. Default is `2030`.
- `market_passthrough`: Numeric value representing the firm's ability to pass carbon tax onto the consumer. Default is `0`.

**Returns:**

- A list of result data frames or writes results to the disk based on the provided path.

**Example Usage:**

```{r}
# Load the package
library(trisk.model)

# Run the stress test and save results to disk
run_trisk(
  input_path = "path/to/project_input/",
  output_path = "path/to/output",
  baseline_scenario = "NGFS2023GCAM_CP",
  target_scenario = "NGFS2023GCAM_NZ2050",
  scenario_geography = "Global",
  risk_free_rate = 0.02,
  discount_rate = 0.07,
  growth_rate = 0.03,
  div_netprofit_prop_coef = 1,
  shock_year = 2030,
  show_params_cols = TRUE
)
```

---

### `run_trisk_model()`

This function runs the core transition risk stress test model without handling file read/write operations. It is used to execute the transition risk stress test when data is already loaded into memory.

**Example Usage:**

```{r}
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
  baseline_scenario = "NGFS2023GCAM_CP",
  target_scenario = "NGFS2023GCAM_NZ2050",
  scenario_geography = "Global",
  shock_year = 2030
)
```

---

## License

This project is licensed under the GNU License. See the [LICENSE](LICENSE) file for more information.
