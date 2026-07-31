# Calculate total distance moved

Calculates accumulated movement within the selected analysis window.

## Usage

``` r
calc_tot_distance(
  data,
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

  An organised shuttle-box data frame containing cumulative `distance`.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the result.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Optional gravitation duration in hours.

## Value

Total distance moved within the selected window.
