# Plot the core body temperature during the trial

This function plots the core body temperature during the trial, along
with temperature preference, avoidance, and gravitation time.

## Usage

``` r
plot_T_segmented(
  data,
  Tpref_method = "median",
  Tavoid_percentiles = c(0.05, 0.95),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = TRUE,
  overlay_chamber_temp = T
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- Tpref_method:

  The method used for calculation of temperature preference ("median",
  "mean", "mode"). Default is "median".

- Tavoid_percentiles:

  The lower and upper percentile for lower and upper avoidance
  temperature calculation resp. Default is c(0.05, 0.95)

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- exclude_acclimation:

  Exclude the acclimation period from variable calculation, default is
  TRUE

- overlay_chamber_temp:

  Boolean to determine whether or not to overlay chamber temperatures,
  default = TRUE

## Value

plot with the core body temperature and key shuttle-box metrics
