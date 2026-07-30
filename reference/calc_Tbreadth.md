# Calculate effective selected thermal breadth

Calculates an effective breadth from the frequency distribution of core
body temperatures. Temperatures are placed into equal-width bins, the
proportion of observations in each bin is calculated, and breadth is
defined as `bin_size / sum(p^2)`, where `p` is the proportion of
observations in each occupied bin.

## Usage

``` r
calc_Tbreadth(
  data,
  bin_size = 0.1,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- bin_size:

  Temperature-bin width in degrees Celsius. The default is 0.1. Use the
  same value for all animals being compared.

- exclude_start_minutes:

  Minutes to exclude from the beginning of the recording. Default is 0.

- exclude_end_minutes:

  Minutes to exclude from the end of the recording. Default is 0.

- exclude_acclimation:

  Logical. Exclude rows labelled `"acclimation"` in `trial_phase`.
  Default is `FALSE`.

- print_results:

  Logical. Print the result. Default is `TRUE`.

## Value

A single effective thermal breadth in degrees Celsius.

## Details

This is a Simpson/Hill-number effective breadth expressed in degrees
Celsius. It is small when observations are concentrated around one
temperature and larger when time is distributed broadly and evenly
across temperatures.

This metric describes selected or experienced temperatures during the
trial; it is not a measure of physiological thermal tolerance.

## See also

[`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md),
[`calc_Tpref`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
[`calc_Tavoid`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md)

## Examples

``` r
example_file <- system.file(
  "extdata", "Fish_8_13_3_example.csv",
  package = "ShuttleboxR"
)
fish <- read_shuttlesoft(example_file)
calc_Tbreadth(fish, print_results = FALSE)
#> [1] 1.664842
```
