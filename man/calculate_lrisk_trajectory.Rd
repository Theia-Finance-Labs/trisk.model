% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate.R
\name{calculate_lrisk_trajectory}
\alias{calculate_lrisk_trajectory}
\title{Calculate litigation shock trajectory}
\usage{
calculate_lrisk_trajectory(
  input_data_list,
  baseline_scenario,
  target_scenario,
  litigation_scenario,
  start_year,
  end_year,
  time_horizon,
  log_path
)
}
\arguments{
\item{input_data_list}{List with project agnostic and project specific input data}

\item{baseline_scenario}{Character. A string that indicates which
of the scenarios included in the analysis should be used to set the
baseline ald_business_unit trajectories.}

\item{target_scenario}{Character. A string that indicates which
of the scenarios included in the analysis should be used to set the
late & sudden ald_business_unit trajectories.}

\item{litigation_scenario}{Tibble with 1 row holding at least variables
\code{year_of_shock}, \code{duration_of_shock}, \code{scc} and \code{exp_share_damages_paid}}

\item{start_year}{Numeric, holding start year of analysis.}

\item{end_year}{Numeric, holding end year of analysis.}

\item{time_horizon}{Considered timeframe for PACTA analysis.}

\item{log_path}{String holding path to log file.}
}
\value{
A tibble holding annual profits
}
\description{
Calculate litigation shock trajectory
}
