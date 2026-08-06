# Calculate shuttle-box metrics for all trials

Calculates project-level metrics and optionally separates transitional
and settled behaviour.

## Usage

``` r
calc_project_results(
  data_read,
  calculate_distance = FALSE,
  pixel_to_cm = TRUE,
  recalculate_core_T = FALSE,
  exclude_acclimation = FALSE,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  Tpref_method = "median",
  Tavoid_percentiles = c(0.05, 0.95),
  Tpercentile_range_percentiles = c(0.25, 0.75),
  textremes_threshold = expression(0.2 * (max(df$max_T, na.rm = TRUE) - max(df$min_T, na.rm = TRUE))),
  core_T_variance_type = "std_error",
  exclude_gravitation_thermal = FALSE,
  exclude_gravitation_activity = FALSE,
  gravitation_failure = c("warn", "error")
)
```

## Arguments

- data_read:

  A list of imported shuttle-box trials.

- calculate_distance:

  Calculate distance from coordinates.

- pixel_to_cm:

  Convert pixels to centimetres when calculating distance.

- recalculate_core_T:

  Recalculate core body temperature.

- exclude_acclimation:

  Use the dynamic period as the calculation origin.

- exclude_start_minutes:

  Minutes omitted from the selected origin.

- exclude_end_minutes:

  Minutes omitted from the end.

- Tpref_method:

  Method used by
  [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md).

- Tavoid_percentiles:

  Percentiles used by
  [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md).

- Tpercentile_range_percentiles:

  Percentiles used by
  [`calc_Tpercentile_range()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md).
  The default is `c(0.25, 0.75)`.

- textremes_threshold:

  Definition of the extreme-temperature range.

- core_T_variance_type:

  Method used by
  [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md).

- exclude_gravitation_thermal:

  Exclude gravitation from thermal distribution metrics.

- exclude_gravitation_activity:

  Exclude gravitation from distance, shuttles, and occupancy.

- gravitation_failure:

  Use `"warn"` to record NA for gravitation-dependent metrics or
  `"error"` to stop.

## Value

A data frame containing one row of metrics per trial, plus
gravitation-window metadata.

## Details

Gravitation is estimated once per fish and reused across dependent
metrics. Project output includes the distance-based `Tbreadth` and the
percentile-based `Tpercentile_range`.
