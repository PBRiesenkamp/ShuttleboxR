# Calculate the avoidance temperatures

This function calculates the upper and lower avoidance temperature for
the trial

## Usage

``` r
calc_Tavoid(
  data,
  percentiles = c(0.05, 0.95),
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

- percentiles:

  The lower and upper percentile for lower and upper avoidance
  temperature calculation resp. Default is c(0.05, 0.95)

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

the upper and lower avoidance temperature
