# ShuttleboxR

## Quick start: one ShuttleSoft file

Open the ShuttleboxR project in RStudio and load the development version:

```r
devtools::load_all()
```

Select a ShuttleSoft text file using the normal Windows file browser:

```r
fish <- read_shuttlesoft(file.choose())
```

The file is prepared automatically and the `core_T` values already written by
ShuttleSoft are retained. Calculate effective selected thermal breadth with:

```r
calc_Tbreadth(fish)
```

To distinguish acclimation from the experimental trial, provide the clock time
at which the trial started:

```r
fish <- read_shuttlesoft(
  file.choose(),
  trial_start = "13:30:00"
)

calc_Tbreadth(fish, exclude_acclimation = TRUE)
```

The default thermal breadth uses 0.1 degrees Celsius bins. Use the same bin size
for every fish being compared:

```r
calc_Tbreadth(fish, bin_size = 0.1)
```

## Recalculating body temperature is optional

ShuttleSoft files normally already contain `core_T`, so most users should not
run `calc_coreT()`.

Recalculation requires body mass and calibrated `a_value` and `b_value`
coefficients. These coefficients must come from an appropriate calibration or
published source; ShuttleboxR cannot infer them from the data file.

```r
fish <- calc_coreT(
  fish,
  mass = 12.4,
  a_value = 0.05,
  b_value = -0.25
)
```

When `initial_T` is omitted, the first valid existing `core_T` value is used.

## Multiple files

A metadata table is still supported for projects containing many trials, but it
is no longer required:

```r
all_fish <- read_shuttlesoft_project(
  directory = choose.dir()
)

results <- calc_project_results(all_fish)
```

`results` includes a `Tbreadth` column.
