# Plot relation between velocity and core body temperature

This function plots the velocity of the subject against the core body
temperature

## Usage

``` r
plot_speed_coreT(data, exclude_start_minutes = 0, exclude_end_minutes = 0)
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

Plot of velocity versus core body temperature
