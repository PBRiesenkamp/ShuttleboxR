% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_coreT_variance.R
\name{calc_coreT_variance}
\alias{calc_coreT_variance}
\title{Calculate the temperature preference}
\usage{
calc_coreT_variance(
  data,
  variance_type = c("std_error", "std_deviation", "coeff_variation"),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = F
)
}
\arguments{
\item{data}{An organised shuttle-box dataframe with corrected core body temperature}

\item{variance_type}{The method used for calculating the variance of core body temperature ("std_error", "std_deviation", "coeff_variation"). Default is "std_error".}

\item{exclude_start_minutes}{Exclusion of time from the start of the trial onwards, in minutes. Default is 0}

\item{exclude_end_minutes}{Exclusion of time from the end of the trial backwards, in minutes. Default is 0}

\item{exclude_acclimation}{Exclude the acclimation period from variable calculation, default = F}
}
\value{
variance in core body temperature
}
\description{
This function calculates the temperature preference for the trial
}
