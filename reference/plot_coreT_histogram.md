# Plot the distribution of core body temperatures

Plots the percentage of observations within fixed-width `core_T` bins.
The plot can show the midpoint of the most frequently occupied
temperature bin and the effective selected thermal breadth calculated
from the same bins.

## Usage

``` r
plot_coreT_histogram(
  data,
  bin_size = 0.1,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  show_Tpref = TRUE,
  show_Tbreadth = TRUE
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- bin_size:

  Width of the temperature bins in degrees Celsius. Default is 0.1. Use
  the same value when comparing animals.

- exclude_start_minutes:

  Minutes excluded from the beginning of the recording. Default is 0.

- exclude_end_minutes:

  Minutes excluded from the end of the recording. Default is 0.

- exclude_acclimation:

  Logical. Exclude rows labelled `"acclimation"`. Default is `FALSE`.

- show_Tpref:

  Logical. Show a dashed line at the midpoint of the most frequently
  occupied temperature bin. Default is `TRUE`.

- show_Tbreadth:

  Logical. Report effective selected thermal breadth in the plot
  subtitle. Default is `TRUE`.

## Value

Invisibly returns the `ggplot` object, allowing it to be saved or
further customised.

## See also

[`calc_Tbreadth`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
[`calc_Tpref`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md)

## Examples

``` r
example_file <- system.file(
  "extdata", "Fish_8_13_3_example.csv",
  package = "ShuttleboxR"
)
fish <- read_shuttlesoft(example_file)
plot_coreT_histogram(fish)
```
