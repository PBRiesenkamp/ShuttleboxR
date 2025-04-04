% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tracking.R
\name{plot_tracking}
\alias{plot_tracking}
\title{Plot number of lost tracks per interval}
\usage{
plot_tracking(
  data,
  interval_minutes = 60,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = F
)
}
\arguments{
\item{data}{An organised shuttle-box dataframe with corrected core body temperature}

\item{interval_minutes}{Specify the interval length in minutes to determine tracking frequency, default is 60}

\item{exclude_start_minutes}{Exclusion of time from the start of the trial onwards, in minutes. Default is 0}

\item{exclude_end_minutes}{Exclusion of time from the end of the trial backwards, in minutes. Default is 0}

\item{exclude_acclimation}{Exclude the acclimation period from variable calculation, default is TRUE}
}
\value{
Plot of the number of missed tracks per interval
}
\description{
This function plots the number of missing tracks per interval
}
