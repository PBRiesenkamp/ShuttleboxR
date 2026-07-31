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

  Optional gravitation duration in hours, usually from
  [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md).

## Value

A non-negative selected thermal breadth in degrees Celsius.

## Details

Tbreadth uses both the frequency and separation of temperatures. It is
not centred on Tpref. When gravitation is excluded, it describes the
spread of the settled temperature distribution.

## See also

[`plot_coreT_histogram`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md),
[`calc_gravitation`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)

## Examples

``` r
example_file <- system.file("extdata", "Fish_14_13_2.txt", package = "ShuttleboxR")
fish <- read_shuttlesoft(example_file)
calc_Tbreadth(fish, print_results = FALSE)
#> [1] 2.057776
```
