% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_pd_change_overall.R
\name{keep_merton_compatible_rows}
\alias{keep_merton_compatible_rows}
\title{Keep rows that fulfill constraints of the merton model}
\usage{
keep_merton_compatible_rows(data, stage)
}
\arguments{
\item{data}{A tibble holding at least the columns \code{debt},
\code{equity_0_baseline} or \code{equity_t_baseline}, \code{equity_0_late_sudden} or \code{equity_t_late_sudden}, \code{volatility}, \code{risk_free_rate}
and \code{term}.}

\item{stage}{String, indicating if checks are done for overall or annual
results.}
}
\value{
Tibble holding rows from \code{data} that are compatible with constraints
of \code{\link[=calc_survival_probability_merton]{calc_survival_probability_merton()}}.
}
\description{
Keep rows that fulfill constraints of the merton model as line out for
\code{\link[=calc_survival_probability_merton]{calc_survival_probability_merton()}}.
}
