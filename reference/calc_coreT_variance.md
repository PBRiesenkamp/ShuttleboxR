# Calculate variation in core body temperature

Calculates variation in core temperature within the selected analysis
window.

## Usage

``` r
calc_coreT_variance(
  data,
  variance_type = c("std_error", "std_deviation", "coeff_variation"),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  exclude_gravitation = FALSE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- variance_type:

  One of `"std_error"`, `"std_deviation"`, or `"coeff_variation"`.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Optional gravitation duration in hours.

## Value

A single measure of core-temperature variation.
