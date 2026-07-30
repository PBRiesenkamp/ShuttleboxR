# Calculate shuttle-box metrics for all trials

Calculates the standard ShuttleboxR metrics for every trial in a list,
such as the object returned by
[`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md).
The output now includes effective selected thermal breadth from
[`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md).

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
  Tbreadth_bin_size = 0.1,
  textremes_threshold = expression(0.2 * (max(df$max_T) - max(df$min_T))),
  core_T_variance_type = "std_error"
)
```

## Arguments

- data_read:

  A list containing imported shuttle-box trials.

- calculate_distance:

  Logical. Calculate distance from coordinates. Default is `FALSE`.

- pixel_to_cm:

  Logical. Convert pixels to centimetres when calculating distance.
  Default is `TRUE`.

- recalculate_core_T:

  Logical. Recalculate body temperature with
  [`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md).
  Default is `FALSE`, which uses the `core_T` already present in the
  ShuttleSoft files.

- exclude_acclimation:

  Logical. Exclude the acclimation period. Default is `FALSE`.

- exclude_start_minutes:

  Minutes excluded from the start of each recording. Default is 0.

- exclude_end_minutes:

  Minutes excluded from the end of each recording. Default is 0.

- Tpref_method:

  Method used by
  [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md):
  `"median"`, `"mean"`, or `"mode"`. Default is `"median"`.

- Tavoid_percentiles:

  Lower and upper percentiles used by
  [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md).
  Default is `c(0.05, 0.95)`.

- Tbreadth_bin_size:

  Bin width in degrees Celsius used by
  [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md).
  Default is 0.1.

- textremes_threshold:

  Definition of the extreme-temperature range.

- core_T_variance_type:

  Method used by
  [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md).

## Value

A data frame containing metrics for all trials.

## Details

ShuttleSoft files normally already contain `core_T`, so recalculation is
off by default. Set `recalculate_core_T = TRUE` only when calibrated
thermal-lag parameters are available for every trial.
