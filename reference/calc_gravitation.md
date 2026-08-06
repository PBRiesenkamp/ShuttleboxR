# Calculate gravitation time

Estimates a one-breakpoint segmented regression of core temperature
through time.

## Usage

``` r
calc_gravitation(
  data,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `time_sec` and
  `core_T`.

- exclude_start_minutes:

  Minutes omitted before fitting, measured from the selected origin.

- exclude_end_minutes:

  Minutes omitted from the end before fitting.

- exclude_acclimation:

  Use the dynamic-period start as the origin rather than the recording
  start.

- print_results:

  Print the estimated duration.

## Value

A gravitation duration in hours from the selected origin.

## Details

The returned duration can be reused in thermal calculations to ensure
that all metrics use the same checked breakpoint.

## See also

[`plot_T_segmented`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md),
[`calc_Tpref`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
[`calc_Tbreadth`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
[`calc_Tpercentile_range`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
calc_gravitation(fish)
} # }
```
