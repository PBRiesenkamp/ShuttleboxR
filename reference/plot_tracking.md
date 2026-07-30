# Plot number of lost tracks per interval

This function plots the number of missing tracks per interval

## Usage

``` r
plot_tracking(
  data,
  interval_minutes = 60,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = F
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- interval_minutes:

  Specify the interval length in minutes to determine tracking
  frequency, default is 60

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- exclude_acclimation:

  Exclude the acclimation period from variable calculation, default is
  TRUE

## Value

Plot of the number of missed tracks per interval
