# Calculate a percentile-based thermal range

Calculates the difference between two percentiles of the selected
core-temperature distribution. With the default 25th and 75th
percentiles, this is the interquartile range (IQR): the width of the
central 50 percent of experienced temperatures.

## Usage

``` r
calc_Tpercentile_range(
  data,
  percentiles = c(0.25, 0.75),
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

  An organised shuttle-box data frame containing `core_T`.

- percentiles:

  Lower and upper percentiles. Default is `c(0.25, 0.75)`.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period. Default is 0.

- exclude_end_minutes:

  Minutes omitted from the end of the recording. Default is 0.

- exclude_acclimation:

  Logical. Use only the dynamic period. Default is `FALSE`.

- print_results:

  Logical. Print the percentile boundaries and their difference. Default
  is `TRUE`.

- exclude_gravitation:

  Logical. Exclude the transitional gravitation period. Default is
  `FALSE` for backwards compatibility.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset;
  when `exclude_gravitation = TRUE`, gravitation is estimated
  automatically.

## Value

A single non-negative percentile range in degrees Celsius.

## Details

This metric complements
[`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md).
`Tpercentile_range` describes the width between two explicit percentile
boundaries, whereas `Tbreadth` uses the complete distribution and
averages all pairwise temperature differences. It also differs from
`Tpref_range`, which is calculated from the avoidance percentiles
selected in
[`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md).

When `exclude_gravitation = TRUE`, gravitation is estimated
automatically unless `gravitation_time` is supplied as an advanced
override.

## See also

[`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
[`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md),
[`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
calc_Tpercentile_range(fish, exclude_gravitation = TRUE)
calc_Tpercentile_range(
  fish,
  percentiles = c(0.10, 0.90),
  exclude_gravitation = TRUE
)
} # }
```
