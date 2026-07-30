# Plot the temperatures in each side of the shuttlebox over time

This function plots the temperature in the warm and cold chamber during
the trial

## Usage

``` r
plot_T_gradient(data, exclude_start_minutes = 0, exclude_end_minutes = 0)
```

## Arguments

- data:

  An organised shuttle-box dataframe

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

## Value

a plot with the temperatures in the warm and cold chambers
