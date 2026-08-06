# ShuttleboxR

ShuttleboxR imports, summarises and inspects temperature shuttle-box
data. Please see the package vignette for a more detailed cescription
and full workflow!

The package supports two linked tasks:

1.  analysing one fish in detail; and
2.  comparing all fish in a project to identify trials that deserve
    closer inspection.

A project-level flag is not an automatic reason to exclude a fish. The
intended workflow is **flag → inspect the raw trial → decide and
document**.

## Installation

``` r

install.packages("remotes")  # only needed once
remotes::install_github(
  "PBRiesenkamp/ShuttleboxR",
  build_vignettes = TRUE
)
```

``` r

library(ShuttleboxR)
```

## Choose the correct starting point

| Starting data | Use | Result |
|----|----|----|
| One raw ShuttleSoft file | [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md) | `fish`: one time-series trial |
| A folder of raw files | [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md) | `all_fish`: a list of trials |
| An existing one-row-per-fish summary CSV | [`read_project_database()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_project_database.md) | `project_data`: a project summary table |

### One raw file

``` r

fish <- read_shuttlesoft(file.choose())
```

### A folder of raw files

``` r

all_fish <- read_shuttlesoft_project(directory = choose.dir())
project_data <- calc_project_results(all_fish)
```

The Windows folder-selection window displays folders rather than the
`.txt` and `.csv` files inside them. Select the folder containing all
raw trials.

### An existing project summary

``` r

project_data <- read_project_database(file.choose())
```

Do not use
[`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
for a one-row-per-fish summary table. It expects raw time-series
observations.

## Analyse one fish

### Import and inspect

``` r

fish <- read_shuttlesoft(file.choose())
inspect(fish)
```

When a recording contains an acclimation period, provide the time at
which the dynamic trial started:

``` r

fish <- read_shuttlesoft(file.choose(), trial_start = "13:30:00")
```

### View and calculate gravitation

The early dynamic period may describe the route from the starting
temperature to the fish’s settled thermal region. View the fitted
breakpoint before using post-gravitation metrics:

``` r

plot_T_segmented(fish)
calc_gravitation(fish)
```

The plot and calculation estimate gravitation internally. You do not
need to create a separate gravitation object for later functions.

### Calculate settled thermal metrics

Set `exclude_gravitation = TRUE`. Each function then estimates
gravitation under the hood and applies the correct cutoff.

``` r

calc_Tpref(fish, exclude_gravitation = TRUE)
calc_Tavoid(fish, exclude_gravitation = TRUE)
calc_Tbreadth(fish, exclude_gravitation = TRUE)
calc_Tpercentile_range(fish, exclude_gravitation = TRUE)
calc_extremes(fish, exclude_gravitation = TRUE)
calc_coreT_variance(
  fish,
  variance_type = "std_deviation",
  exclude_gravitation = TRUE
)
```

When `trial_start` was supplied and only the dynamic period should be
analysed, also use `exclude_acclimation = TRUE`:

``` r

calc_Tpref(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE
)
```

Activity can describe either the complete selected period or settled
thermoregulation:

``` r

calc_shuttles(fish)
calc_shuttles(fish, exclude_gravitation = TRUE)
```

### Percentile-based thermal range

[`calc_Tpercentile_range()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md)
calculates the difference between two percentiles of the
core-temperature distribution. The defaults are the 25th and 75th
percentiles, so the result is the interquartile range: the width of the
central 50% of experienced temperatures.

``` r

calc_Tpercentile_range(fish, exclude_gravitation = TRUE)
```

A larger value means that the middle half of the temperature
observations was spread across a wider interval. The percentiles can be
changed directly:

``` r

calc_Tpercentile_range(
  fish,
  percentiles = c(0.10, 0.90),
  exclude_gravitation = TRUE
)
```

The three spread measures answer different questions:

- `Tpref_range` is the distance between the avoidance percentiles used
  by
  [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md)
  (5th and 95th by default).
- `Tpercentile_range` is the distance between user-selected percentiles
  (25th and 75th by default) and focuses on the central part of the
  distribution.
- `Tbreadth` uses the complete distribution and averages the temperature
  difference across all pairs of observations.

None of these values shows whether the histogram is skewed or
multimodal, so they should be interpreted with
[`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md).

### What Tbreadth means

`Tbreadth` is the average absolute difference between the fish’s core
temperatures at two observations from the analysed period.

- A fish remaining near one temperature has a Tbreadth close to 0 °C.
- Equal time at 20 °C and 21 °C gives a Tbreadth of 0.5 °C.
- Equal time at 10 °C and 20 °C gives a Tbreadth of 5 °C.

It is not centred on Tpref and does not define a lower and upper
interval. Use it with the histogram to see whether the distribution is
narrow, broad, skewed or multimodal:

``` r

plot_coreT_histogram(fish, exclude_gravitation = TRUE)
```

### Useful single-trial summaries and plots

``` r

calc_shuttles(fish)
calc_tot_distance(fish)
calc_occupancy(fish)
calc_track_accuracy(fish)
```

``` r

plot_T_gradient(fish)
plot_T_segmented(fish)
plot_coreT_histogram(fish, exclude_gravitation = TRUE)
plot_distance(fish)
plot_interval(fish, column = "velocity", interval_minutes = 30)
plot_speed_coreT(fish)
plot_tracking(fish)
plot_heatmap(fish)
```

[`plot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance.md)
shows cumulative movement through the trial, whereas
[`plot_interval()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_interval.md)
can show how a selected measure, such as velocity, changes among time
blocks.
[`plot_speed_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_speed_coreT.md)
examines whether movement speed varies across experienced core
temperatures. These plots help separate biological variation from
temperature-control, tracking or behavioural problems.

Additional exploratory views are available with:

``` r

plot_histogram(fish, column = "velocity", binwidth = 0.5)
animate_movements(fish)
```

## Analyse a complete project

### Create project summaries from raw trials

``` r

all_fish <- read_shuttlesoft_project(directory = choose.dir())

project_data <- calc_project_results(
  all_fish,
  exclude_gravitation_thermal = TRUE
)
```

[`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
estimates gravitation once per fish and reuses it for all thermal
metrics, including `Tpercentile_range`. Activity metrics include
gravitation unless `exclude_gravitation_activity = TRUE` is also
specified.

### Screen the project

``` r

plot_histograms(project_data)
plot_upper_vs_lower_extremes(project_data)
plot_distance_vs_shuttles(project_data, highlight_cases = TRUE)
plot_limits_vs_distance(project_data, highlight_cases = TRUE)
plot_limits_vs_shuttles(project_data, highlight_cases = TRUE)
```

The limit-exposure plots use a default threshold of 10% of analysed
observations. Adjust it when the study design requires a different
value:

``` r

plot_upper_vs_lower_extremes(
  project_data,
  lower_limit_threshold = 5,
  upper_limit_threshold = 5
)
```

To retrieve the fish highlighted by a review plot:

``` r

review <- plot_distance_vs_shuttles(
  project_data,
  highlight_cases = TRUE,
  return_cases = TRUE
)
review$cases
```

### View pairwise relationships

``` r

correlation_matrix(
  project_data,
  columns = c(
    "Tpref", "Tpref_range", "grav_time",
    "tot_distance", "nr_shuttles", "t_near_limits"
  )
)
```

The lower panels show pairwise scatterplots; the upper panels show
Pearson correlations. A fish separated from the main cloud in one panel
may have an unusual combination of otherwise plausible values.

### PCA screening

Choose the variables explicitly so that adding a new numeric column does
not silently change the analysis:

``` r

project_pca <- pca(
  project_data,
  variables = c(
    "Tpref", "Tpref_range", "grav_time",
    "tot_distance", "nr_shuttles",
    "t_near_max", "t_near_min"
  ),
  print_labels = FALSE
)

project_pca$plots$screeplot
project_pca$plots$biplot
project_pca$outlier_details
```

The default PCA screen is deliberately conservative. It highlights fish
found by both Mahalanobis distance and DBSCAN. Sensitivity can be
changed with `mahalanobis_th`, `dbscan_th`, `dbscan_minPts` and
`flag_rule`.

### Return to the raw trial

``` r

fish_to_check <- all_fish[["Fish_17.txt"]]

plot_T_gradient(fish_to_check)
plot_tracking(fish_to_check)
plot_T_segmented(fish_to_check)
plot_coreT_histogram(fish_to_check, exclude_gravitation = TRUE)
plot_heatmap(fish_to_check)

calc_Tpref(fish_to_check, exclude_gravitation = TRUE)
calc_Tavoid(fish_to_check, exclude_gravitation = TRUE)
calc_Tbreadth(fish_to_check, exclude_gravitation = TRUE)
calc_Tpercentile_range(fish_to_check, exclude_gravitation = TRUE)
```

Exclude a fish only when inspection provides a documented technical or
biological reason. Statistical unusualness alone is not evidence that a
trial is invalid.

## Documentation

``` r

vignette("ShuttleboxR", package = "ShuttleboxR")
help(package = "ShuttleboxR")
```
