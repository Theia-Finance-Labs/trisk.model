
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.climate.stress.test

<!-- badges: start -->

[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/actions)
<!-- badges: end -->

Beta version. More soon…

The goal of r2dii.climate.stress.test is to provide a tool that can be
used to conduct what-if climate stress test analyses for financial
institutions, supervisors, regulators and other stakeholders. The tool
aims at highlighting potential financial risk in especially climate
relevant sectors, split by production technology where required. The
sectors covered by the 2Dii climate stress test and therefore by this
package, follow mostly the logic of the Paris Agreement Capital
Transition Assessment (PACTA) tool, but can in principle be adapted to
other settings. W.I.P.

## Installation

You can install development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.climate.stress.test")
```

Tu run most of the analyses, you will need additional auxiliary data,
such as scenario data. The specific requirements are outlined below.
**TODO**

Before running the code on a local machine, set the path to the data
files either as an Environment Variable or an R option using:

    # shell
    export ST_DATA_PATH="<path to data>"

or in R:

``` r
options("st_data_path") <- "path to data"
```

## Scope

The repository covers climate risk calculations for the following types
of risks

  - Transition Risk
  - Litigation Risk It will be extended to also cover
  - Physical Risk

The methods for all of these are currently in the development phase,
meaning there are likely going to be changes to the calculations in
future releases.

The following financial asset types are currently covered:

  - Listed Equity (EQ)
  - Corporate Bonds (CB)
  - Corporate Loans (LBK)

Other asset types may be covered in the future.

## Work flows

The repository is structured in a way that provides dedicated work flows
for each of the risk types and some of the asset types.

### Transition Risk for listed equity and corporate bonds

…

### Transition Risk for corporate loan books

#### Methodological notes

Calculating the transition risk for corporate loan book related to a
sudden policy shock differs from the calculation of such a shock for
listed equity. The value change of loans is not captured well simply by
applying a DCF model. Dedicated credit risk models are required instead.
Specifically, we want to quantify the impact of a climate transition
shock on the probabilities of default (PD) of the loans at hand. This is
a common risk metric for loans and a change in PDs is therefore expected
to be of more use to anyone applying this software, then a shock based
on changes in market value of the issuing company. The PD can then be
used to derive impacts on the value of a loan book, by plugging the
changed PD into an expected loss (EL) calculation, which is in its
fundamental form defined as:

EL = PD \* LGD \* EAD

… with LGD being the loss given default (percentage loss) and EAD the
exposure at default (value of the loan in absolute currency in the loan
book at hand).

In our case, we aim at using a structural model. Data constraints are a
limiting factor with regard to using company level information that
would normally go into a case by case rating of credit worthiness.
Hence, we use the Merton model, which requires as inputs the asset value
in t = 0 (A\_0) and the debt value (D) of the company at hand. The debt
value is assumed constant over time, the asset value in t = 0 is derived
as A\_0 = E\_0 (equity value in t = 0) \* D. We further need the
volatility of the equity value, the risk free rate and the term
structure as inputs.

The outcome of the Merton model is a PD contignent to the inputs. For
us, the value of interest is not an absolute PD as a direct outcome of
the model, but rather how the PD changes between a baseline scenario and
a late and sudden scenario. We therefore keep all inputs equal except
the valuations under those two scenarios. We model a PD under the
baseline scenario and another under a late and sudden scenario and
calculate the relative change for the given set of parameters. This will
result in a change in PDs that does not reflect the magnitude of the
equity and debt values and can therefore be applied as a shock factor on
PDs that banks may have calculated with a different model themselves:

PD\_change = (PD\_late\_sudden - PD\_baseline) / PD\_baseline

##### Assumptions

  - For E\_0, we use the discounted values derived from the DCF model we
    use to calculate changes in market value. This builds on the capex
    plans of the companies in the calculation of the future profits that
    are used in the DCF model. We therefore get different company values
    for the business as usual and the late and sudden cases, which we
    can use to differentiate the scenarios in the credit risk
    calculation.
  - For D, we currently use sectoral equity-debt ratios and thus derive
    the value from the equity value of the business as usual scenario.
    The current values in use are mock values for test purposes that
    need to be replaced with empirical values. This may become a user
    input option in the future. **OPEN**
  - For volatility, we currently use a fixed value of 0.2. The current
    value in use is a mock value for test purposes that needs to be
    replaced with empirical values. This may become a user input option
    in the future. **OPEN**
  - The risk free rate is currently set at 0.05. The current value in
    use is a mock value for test purposes that needs to be replaced with
    empirical values. This may become a user input option in the future.
    **OPEN**
  - The PD changes are calculated for all maturities (in full years)
    between the year of the shock and the end year of the analysis. This
    allows the user to understand the sensitivity of the impact to the
    term structure, but is also required to adequately capture loans of
    different maturities to the same company. It remains to be confirmed
    that this is the appropriate way to cover the different maturities
    or if other considerations need to be reflected.
  - On a technical level, we use implementation of the Merton model from
    the CreditRisk R package, which is described in more detail here:
    <https://cran.r-project.org/web/packages/CreditRisk/CreditRisk.pdf>

#### Prerequisites

In order to calculate the impacts from transition risk on a corporate
loan book, the user needs to provide as input:

  - Parameter settings to locate the project in
    st\_project\_settings.yml
  - Model parameters to use in the financial valuation and to select
    scenarios in model\_parameters.yml
  - PACTA for banks output at the company level, enriched with company
    level loan shares. This can be created by following the steps in the
    script calc\_loan\_book.R in this repository. For more information
    on how to use PACTA for banks, consult:
    <https://2degreesinvesting.github.io/r2dii.match/> and
    <https://2degreesinvesting.github.io/r2dii.analysis/index.html>
  - Sector exposures to the relevant sectors of the analysis. This can
    be obtained by following the steps in the script calc\_loan\_book.R
    in this repository.
  - Specifications of transition scenarios for which to calculate shocks
    and impacts on the given portfolio. These are provided via a file,
    transition\_scenario\_input.csv
  - Capacity factors for the power sector, to transform power capacity
    into power production, using the file
    capacity\_factors\_WEO\_2017.csv. This is required in order to
    calculate profits on actual quantities produced, not theoretical
    capacities.
  - Scenario data that covers production road maps for all technologies
    that are to be analysed up until 2040. The scenario data need to
    cover those road maps for the scenarios selected in the parameter
    files and should follow the structure found in
    Scenarios\_AnalysisInput\_YYYY.csv. This data is used to extrapolate
    the production trajectories for companies beyond the PACTA time
    frame.
  - Price data trajectories found in the file prices\_data\_YYYYQQ.csv,
    which contains projections of market prices per technology from the
    start data of the analysis until the year 2040. This is another
    required input to obtain profits.
  - Net profit margins are loaded via the model\_parameters.yml file up
    until now. This may change in the future.
  - It is of particular importance that the start year of the analysis
    is available in the PACTA results, in the scenario data and in the
    price data. If either data set starts after the start year of the
    analysis, the work flow will not work.
  - Equally, the scenarios selected for the analysis must be given in
    the PACTA results as well as in the scenario data and the price
    data.

#### Steps

  - Initialize project
  - Load project parameters
  - Load all project input data
  - Wrangle input data
  - Initialise empty results object
  - Loop over transition scenarios (as defined in
    transition\_scenarios\_input.csv), calculating and row-binding
    results to the results object. Calculation entails:
      - plucking a specific scenario definition from the transition
        scenarios to obtain model inputs (1)
      - calculating the corresponding price trajectories (2)
      - calculating the corresponding production trajectories on the
        company level (3)
      - joining the price trajectories to the production trajectories
        (3)
      - joining net profit margins to the production and price
        trajectories (3)
      - calculating future net profits for the entire analysis time
        frame (3)
      - calculate discounted net profits for the entire analysis time
        frame, using the DCF model (3)
      - plucking the portfolio share (plan\_carsten) of each company
        from the pacta data input (4)
      - calculating value changes and percentage losses per transition
        scenario, by combining company level DCF output with portfolio
        share of the corresponding holdings (6)
      - Calculate PD changes per company-technology and maturity (7)
      - aggreagate PD changes to the loan book level to derive expected
        losses (7)
  - write company-tech level results to initialized project folder
  - write portfolio-tech level results to initialized project folder
