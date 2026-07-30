# Calculate the temperature preference

This function calculates the temperature preference for the trial

## Usage

``` r
calc_Tpref(
  data,
  method = c("median", "mean", "mode"),
  exclude_acclimation = F,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  print_results = T
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- method:

  The method used for calculation of temperature preference ("median",
  "mean", "mode"). Default is "median".

- exclude_acclimation:

  Exclude the acclimation period from variable calculation, default = F

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- print_results:

  Print the results, default is TRUE

## Value

the temperature preference
