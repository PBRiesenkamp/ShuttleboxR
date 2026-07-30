# Calculate the temperature preference

This function calculates the temperature preference for the trial

## Usage

``` r
calc_coreT_variance(
  data,
  variance_type = c("std_error", "std_deviation", "coeff_variation"),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = F
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- variance_type:

  The method used for calculating the variance of core body temperature
  ("std_error", "std_deviation", "coeff_variation"). Default is
  "std_error".

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- exclude_acclimation:

  Exclude the acclimation period from variable calculation, default = F

## Value

variance in core body temperature
