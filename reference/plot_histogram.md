# Plot a histogram of a chosen shuttle-box metric

This function plots a histogram of the frequency of values observed for
a chosen shuttle-box metric

## Usage

``` r
plot_histogram(
  data,
  column,
  binwidth = 0.1,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- column:

  Select the name of the column with the variable the histogram needs to
  be plotted from

- binwidth:

  Binwidth of the histogram

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

## Value

A histogram of a chosen shuttle-box metric
