# Calculate selected thermal breadth

Measures the mean absolute difference between two independently selected
core-temperature observations.

## Usage

``` r
calc_Tbreadth(
  data,
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

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the result.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset;
  when `exclude_gravitation = TRUE`, gravitation is estimated
  automatically.

## Value

A non-negative selected thermal breadth in degrees Celsius.

## Details

Tbreadth uses both the frequency and separation of temperatures. It is
not centred on Tpref. When gravitation is excluded, it describes the
spread of the settled temperature distribution.

## See also

[`calc_Tpercentile_range`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md),
[`plot_coreT_histogram`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md),
[`calc_gravitation`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
calc_Tbreadth(fish, exclude_gravitation = TRUE)
} # }
```
