# Calculate the tracking accuracy

This function calculates the proportion of time the subject was tracked
accurately during the trial

## Usage

``` r
calc_track_accuracy(
  data,
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

the tracking accuracy
