# Plot the cumulative distance during the trial

This function plots the cumulative distance covered over time

## Usage

``` r
plot_distance(data, exclude_start_minutes = 0, exclude_end_minutes = 0)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

## Value

Plot of the cumulative distance over time
