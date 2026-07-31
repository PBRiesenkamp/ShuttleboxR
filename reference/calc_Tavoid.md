# Calculate avoidance temperatures

Calculates percentile boundaries of the selected core-temperature
distribution.

## Usage

``` r
calc_Tavoid(
  data,
  percentiles = c(0.05, 0.95),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE,
  exclude_gravitation = FALSE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- percentiles:

  Lower and upper percentiles.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the results.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Optional gravitation duration in hours, usually from
  [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md).

## Value

A two-element vector containing lower and upper avoidance temperatures.
