# Plot the interval means for a selected column over time

This function plots the the interval means for a selected column over
time

## Usage

``` r
plot_interval(
  data,
  column,
  interval_minutes = 10,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- column:

  The name of the column to plot an interval for

- interval_minutes:

  The number of minutes per interval, default is 10

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

## Value

Plot of the interval means for a selected column over time
