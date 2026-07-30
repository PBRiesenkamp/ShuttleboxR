# Calculate the time spent near the extremes

This function calculates the time the subject spent near the set minimum
and maximum temperature limits during the shuttle-box trial

## Usage

``` r
calc_extremes(
  data,
  threshold = 0.2 * (max(data$max_T) - max(data$min_T)),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = F,
  print_results = T
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- threshold:

  Definition of the extreme temperature range. Default is 20% of the
  temperature range: 0.2\*(max(data\$max_T)-max(data\$min_T))

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- exclude_acclimation:

  Exclude the acclimation period from variable calculation, default = F

- print_results:

  Print the results, default is TRUE

## Value

the time spent near each extreme
