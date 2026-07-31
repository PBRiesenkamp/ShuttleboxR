# Calculate selected thermal breadth

Measures how widely separated the temperatures experienced by a fish
were. Tbreadth is the mean absolute difference between the core
temperatures at two independently selected observations from the trial.

## Usage

``` r
calc_Tbreadth(
  data,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

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

A single non-negative selected thermal breadth in degrees Celsius.

## Details

This quantity is also known as the Gini mean difference. It uses the
complete distribution of `core_T`, including both the frequency of each
temperature and the distance between temperatures. It does not use
histogram bins and is not centred on `Tpref`.

A fish that remains at nearly one temperature has a Tbreadth close to
zero. A fish that regularly experiences temperatures far apart has a
larger Tbreadth. For example, a fish spending half its time at 10
degrees Celsius and half at 20 degrees Celsius has a Tbreadth of 5
degrees Celsius: half of all pairs have the same temperature and half
differ by 10 degrees Celsius.

Tbreadth describes overall spread but cannot, by itself, show whether
the histogram is symmetrical, skewed, or multimodal. Interpret it
together with
[`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md).
It is not a minimum-to-maximum range, does not define lower and upper
boundaries, and is not a physiological thermal-tolerance limit.

## See also

[`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md),
[`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
[`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md)

## Examples

``` r
example_file <- system.file(
  "extdata", "Fish_14_13_2.txt",
  package = "ShuttleboxR"
)
fish <- read_shuttlesoft(example_file)
calc_Tbreadth(fish, print_results = FALSE)
#> [1] 2.057776

# Simple examples
calc_Tbreadth(data.frame(core_T = rep(20, 100)), print_results = FALSE)
#> [1] 0
calc_Tbreadth(
  data.frame(core_T = c(rep(10, 50), rep(20, 50))),
  print_results = FALSE
)
#> [1] 5
```
