# Calculate the total distance

This function calculates the total distance covered during the trial

## Usage

``` r
calc_tot_distance(
  data,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE
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

the total distance covered during the trial
