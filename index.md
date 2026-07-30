# ShuttleboxR

ShuttleboxR provides a practical workflow for importing, checking,
analysing, and plotting data from ShuttleSoft shuttle-box temperature
experiments.

## Installation

Install the current GitHub version with:

``` r

install.packages("remotes")
remotes::install_github("PBRiesenkamp/ShuttleboxR")
```

Then load the package:

``` r

library(ShuttleboxR)
```

When developing the package locally in RStudio, open `ShuttleboxR.Rproj`
and use:

``` r

devtools::load_all()
```

## Quick start: one file

Select either a ShuttleSoft `.txt` file or a comma-separated `.csv`
export:

``` r

fish <- read_shuttlesoft(file.choose())
```

[`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
prepares the data automatically. ShuttleSoft’s existing `core_T` values
are retained, so most users do not need to recalculate body temperature.

Calculate common metrics:

``` r

calc_Tpref(fish)
calc_Tavoid(fish)
calc_Tbreadth(fish)
calc_shuttles(fish)
calc_occupancy(fish)
calc_tot_distance(fish)
```

Plot the temperature distribution and effective thermal breadth:

``` r

plot_coreT_histogram(fish)
```

## Excluding acclimation

When the experimental trial began after an acclimation period, provide
the clock time at which the trial started:

``` r

fish <- read_shuttlesoft(
  file.choose(),
  trial_start = "13:30:00"
)

calc_Tpref(fish, exclude_acclimation = TRUE)
calc_Tbreadth(fish, exclude_acclimation = TRUE)
```

When `trial_start` is omitted, the whole recording is treated as trial
data.

## Effective selected thermal breadth

[`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md)
summarises both the range of temperatures used and how evenly time was
distributed among them. It is expressed in degrees Celsius and is
calculated from fixed-width temperature bins:

``` r

calc_Tbreadth(fish, bin_size = 0.1)
```

Use the same `bin_size` for every animal being compared. This metric
describes the breadth of selected or experienced temperatures; it is not
a measure of physiological thermal tolerance.

## Multiple files

Import all ShuttleSoft `.txt` and `.csv` files in a folder:

``` r

all_fish <- read_shuttlesoft_project(
  directory = choose.dir()
)

results <- calc_project_results(all_fish)
```

The resulting table includes `Tpref`, upper and lower `Tavoid`,
`Tbreadth`, shuttling, occupancy, movement, tracking accuracy, and other
summary metrics. A metadata table remains available for projects that
require different trial start times or thermal-model values for
individual files, but it is optional.

An existing project-results CSV can be imported directly:

``` r

project_data <- read_project_database(file.choose())

plot_histograms(project_data)
plot_distance_vs_shuttles(project_data, label_points = FALSE)

correlation_matrix(
  project_data,
  columns = c(
    "Tpref", "Tavoid_lower", "Tavoid_upper",
    "tot_distance", "nr_shuttles"
  )
)
```

[`read_project_database()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_project_database.md)
automatically standardises common column names from older ShuttleboxR
versions. The package vignette includes a complete runnable example
covering project distributions, correlations, scatter plots, outlier
screening, and PCA.

## Recalculating body temperature is optional

ShuttleSoft files normally already contain `core_T`. Only use
[`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
when you deliberately want to recalculate body temperature and have
appropriate calibrated coefficients:

``` r

fish <- calc_coreT(
  fish,
  mass = 12.4,
  a_value = 0.05,
  b_value = -0.25
)
```

The `a_value` and `b_value` coefficients cannot be inferred from the
ShuttleSoft file. They must come from an appropriate calibration or
published source.

## Full guide

After installation, open the package vignette with:

``` r

vignette("ShuttleboxR", package = "ShuttleboxR")
```
