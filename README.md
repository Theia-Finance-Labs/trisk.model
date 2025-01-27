  <!-- badges: start -->
  [![R-CMD-check](https://github.com/Theia-Finance-Labs/trisk.model/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Theia-Finance-Labs/trisk.model/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# trisk.model

This repository hosts the core TRISK model, allowing to conduct transition risk stress tests on financial assets. The tool enable users to analyze the impact of various climate scenarios on company trajectories, technology valuations, and default probabilities.

## Installation

You can install this package directly from GitHub using the `pak` package in R.

1. Install the `pak` package if you don't have it:

    ```r
    install.packages("pak")
    ```

2. Install the transition risk stress testing suite from GitHub:

    ```r
    pak::pak("Theia-Finance-Labs/trisk.model")
    ```


---


## Data inputs

  Trisk expects 4 dataframes in input:

1. **assets** - Contains data on company assets, including production, technology, and geographical details.
2. **financial_features** - Contains financial metrics needed for the stress test.
3. **ngfs_carbon_price** - Provides carbon pricing data for the climate scenarios.
4. **scenarios** - Holds the details of the climate scenarios including the baseline and shock scenarios.

For details on columns structure, refer to the [Vignette on data inputs](https://theia-finance-labs.github.io/trisk.model/articles/data-input-description.html).

## Data outputs

  Dataframes output structure is explained in the [Vignette on data outputs](https://theia-finance-labs.github.io/trisk.model/articles/data-output-description.html)


---


## Main Functions

The functions `run_trisk()`[(doc)](https://theia-finance-labs.github.io/trisk.model/reference/run_trisk.html) and `run_trisk_model()`[(doc)](https://theia-finance-labs.github.io/trisk.model/reference/run_trisk_model.html) are the main functions used to run Trisk simulations.

> **Note 1**: The selected scenario geography needs to be available for the selected baseline and target scenarios. This can be determined from the input scenario data.

> **Note 2**:  It is possible to change the scenario geography but that only filters the scenario data. It won't impact the assets being analyzed depending on their country. Please refer to the package [trisk.analysis](https://theia-finance-labs.github.io/trisk.analysis/) for complex analyses of Trisk outputs.


You can find an example usage of `run_trisk()` in the [Trisk run from folder Vignette](https://theia-finance-labs.github.io/trisk.model/articles/trisk-run-from-folder.html). Note that  `run_trisk()` is a wrapper function around `run_trisk_model()` that handles disk I/O operations


---

This project is licensed under the GNU License. See the [LICENSE](LICENSE) file for more information.
