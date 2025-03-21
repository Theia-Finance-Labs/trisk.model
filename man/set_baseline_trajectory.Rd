% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_trisk_trajectory.R
\name{set_baseline_trajectory}
\alias{set_baseline_trajectory}
\title{Defines which scenario values to use for the baseline trajectory in the
stress test.}
\usage{
set_baseline_trajectory(data)
}
\arguments{
\item{data}{A dataframe that contains scenario trajectories by technology
until 2040 for all the scenarios included in the analysis and
production build out plans by technology or company and technology,
usually for 5 years, based on PACTA results.}
}
\value{
dataframe.
}
\description{
Picks the corresponding values from the original scenario
column indicated in the input and has the option to include PACTA based
production forecast for the first few years of the baseline
trajectory. If included, the trajectory after the end of the production
forecast is offset by the initial production forecast so that the
remainder of the baseline trajectory now is a parallel shift of the
original scenario values. If not included, the trajectories replicate
externally provided scenario trajectories.
Trajectories are furthermore differentiated by scenario_geography, if
multiple are passed.
If no "company_id" or "company_name" are provided, the calculation switches to
portfolio/technology level.
}
\seealso{
Other scenario definition: 
\code{\link{set_trisk_trajectory}()}
}
\concept{scenario definition}
