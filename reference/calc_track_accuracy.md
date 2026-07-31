# Calculate tracking accuracy

Calculates tracking accuracy within the selected analysis window.

## Usage

``` r
calc_track_accuracy(
  data,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `x_pos`.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the result.

## Value

The proportion of observations successfully tracked.
