# ShuttleboxR

ShuttleboxR provides a structured workflow for moving from raw shuttle-box
recordings to a checked, interpretable dataset that is ready for statistical
analysis.

The package is organised around **three consecutive tasks**:

1. **Import and organise** the raw data.
2. **Calculate shuttle-box metrics** using consistent definitions.
3. **Inspect and troubleshoot** the data before formal analysis.

The same logic is applied at two complementary levels:

- the **single-trial branch**, used to calculate and inspect the results for one
  fish; and
- the **project branch**, used after all fish have been processed to compare
  metrics across the study and flag unusual trials for closer inspection.

```text
Raw shuttle-box recordings
        |
        +-- Single-trial branch
        |      1. Import and organise one trial
        |      2. Calculate thermal and behavioural metrics
        |      3. Inspect system performance and fish behaviour
        |
        +-- Project branch
               1. Import all trials or an existing results database
               2. Compile one row of metrics per fish
               3. Inspect distributions, relationships and multivariate patterns
                              |
                              v
                    Flag trials for review
                              |
                              v
                    Return to the single-trial plots
```

ShuttleboxR supports quality control and data exploration. It does **not**
automatically decide that a fish should be excluded, and it does not replace the
statistical analysis appropriate to a study. A trial that appears unusual at the
project level should be checked against its raw temperature, tracking and
behavioural records, and interpreted in the context of the species and
experimental design.

## Installation

Install the current GitHub version with:

```r
install.packages("remotes")  # only needed once

remotes::install_github(
  "PBRiesenkamp/ShuttleboxR",
  build_vignettes = TRUE
)
```

Then load the package:

```r
library(ShuttleboxR)
```

## Branch 1: analyse and inspect one fish

### 1. Import and organise

Select a ShuttleSoft `.txt` file or comma-separated `.csv` export:

```r
fish <- read_shuttlesoft(file.choose())
```

`read_shuttlesoft()` imports and prepares the recording automatically. It adds
elapsed time, labels trial phases and identifies shuttles between chambers. The
existing `core_T` values produced by ShuttleSoft are retained by default.

When the recording includes a separate acclimation period, provide the clock
time at which the dynamic trial began:

```r
fish <- read_shuttlesoft(
  file.choose(),
  trial_start = "13:30:00"
)
```

The structure can be checked with:

```r
inspect(fish)
```

### 2. Calculate shuttle-box metrics

```r
calc_Tpref(fish, exclude_acclimation = TRUE)
calc_Tavoid(fish, exclude_acclimation = TRUE)
calc_Tbreadth(fish, exclude_acclimation = TRUE)
calc_gravitation(fish)
calc_shuttles(fish, exclude_acclimation = TRUE)
calc_occupancy(fish, exclude_acclimation = TRUE)
calc_tot_distance(fish, exclude_acclimation = TRUE)
calc_extremes(fish, exclude_acclimation = TRUE)
calc_track_accuracy(fish)
```

The thermal metrics answer related but distinct questions:

- `Tpref` summarises the central selected temperature; the default is median
  `core_T`.
- `Tavoid_lower` and `Tavoid_upper` are lower and upper percentiles of the
  `core_T` distribution.
- `Tpref_range` is the difference between those avoidance temperatures.
- `Tbreadth` describes how far apart the temperatures experienced by the fish
  were. It is the average absolute difference between the temperatures at two
  randomly selected observations from the trial.

### What does Tbreadth mean?

Imagine choosing two moments from the trial and asking how different the fish's
core temperature was at those moments. Repeat that for every possible pair and
take the average. That average is `Tbreadth`.

- A fish that remains at nearly one temperature has a Tbreadth close to 0 °C.
- Equal time at 20 °C and 21 °C gives a Tbreadth of 0.5 °C.
- Equal time at 10 °C and 20 °C gives a Tbreadth of 5 °C.

The final example gives 5 °C, rather than 10 °C, because half of the possible
pairs compare two equal temperatures and half compare temperatures 10 °C apart.

`Tbreadth` is **not centred on Tpref**, does not define a lower and upper
boundary, and does not depend on histogram bin size. It summarises overall
spread, while `plot_coreT_histogram()` shows whether that spread is symmetrical,
skewed, or split into multiple peaks.

```r
calc_Tbreadth(fish)
plot_coreT_histogram(fish)
```

### 3. Inspect the trial

Single-trial plots help answer two different quality-control questions.

**Did the shuttle-box and tracking system work as intended?**

```r
plot_T_gradient(fish)
plot_tracking(fish)
plot_T_segmented(fish)
```

**Did the fish display interpretable behaviour?**

```r
plot_coreT_histogram(fish)
plot_heatmap(fish)
plot_distance(fish)
plot_interval(fish)
plot_speed_coreT(fish)
animate_movements(fish)
```

These plots can reveal interruptions in temperature control, poor tracking,
prolonged inactivity, extensive time near system limits, doorway use or other
patterns that deserve closer examination.

## Branch 2: inspect a complete project

### 1. Import all trials

Place the raw files for a project in one folder and import them as a named list:

```r
all_fish <- read_shuttlesoft_project(
  directory = choose.dir()
)
```

A metadata table is optional. It is useful when individual files have different
trial start times:

```r
metadata <- data.frame(
  file_name = c("Fish_1.txt", "Fish_2.txt"),
  trial_start = c("10:30:00", "14:15:00")
)

all_fish <- read_shuttlesoft_project(
  metadata = metadata,
  directory = choose.dir()
)
```

### 2. Calculate one row of results per fish

```r
project_results <- calc_project_results(
  all_fish,
  exclude_acclimation = TRUE,
  Tpref_method = "median",
  Tavoid_percentiles = c(0.05, 0.95)
)
```

This creates a project-level database containing thermal, activity, occupancy,
tracking and movement metrics for every trial.

An existing project-results CSV can instead be loaded with:

```r
project_results <- read_project_database(file.choose())
```

### 3. Flag unusual trials for individual review

Start with the distributions of key metrics:

```r
possible_outliers <- plot_histograms(project_results)
```

Then inspect relationships that can distinguish different kinds of unusual
behaviour:

```r
plot_distance_vs_shuttles(project_results, label_points = FALSE)
plot_limits_vs_distance(project_results, label_points = FALSE)
plot_limits_vs_shuttles(project_results, label_points = FALSE)
```

A correlation matrix or PCA can provide a multivariate overview:

```r
correlation_matrix(
  project_results,
  columns = c(
    "Tpref", "Tavoid_lower", "Tavoid_upper",
    "tot_distance", "nr_shuttles", "t_near_limits"
  )
)

project_pca <- pca(
  project_results[c(
    "fileID", "Tpref", "Tavoid_lower", "Tavoid_upper",
    "tot_distance", "nr_shuttles", "t_near_limits"
  )],
  print_labels = FALSE
)

project_pca$plots$biplot
project_pca$outliers
```

When the project database was generated with the current version of
`calc_project_results()`, `Tbreadth` can also be included in these project-level
comparisons. Older summary databases must be regenerated from the raw trial
files before `Tbreadth` is available.

The output identifies **candidates for review**, not automatic exclusions. The
recommended next step is to locate each flagged fish in `all_fish` and inspect
its raw trial:

```r
fish_to_check <- all_fish[["Fish_17.txt"]]

plot_T_gradient(fish_to_check)
plot_tracking(fish_to_check)
plot_coreT_histogram(fish_to_check)
plot_heatmap(fish_to_check)
```

A fish should only be excluded when there is a clear, documented reason that the
trial is technically flawed or does not provide a valid measure of the intended
behaviour. Unusual behaviour can also be genuine biological variation.

## Recalculating core temperature is optional

ShuttleSoft files normally already contain `core_T`. Only use `calc_coreT()`
when those values need to be replaced and appropriate thermal-lag coefficients
are available:

```r
fish <- calc_coreT(
  fish,
  mass = 12.4,
  a_value = 0.05,
  b_value = -0.25
)
```

The `a_value` and `b_value` coefficients cannot be inferred from the recording.
They must come from an appropriate calibration or published source.

## Full guide

After installation, open the worked vignette with:

```r
vignette("ShuttleboxR", package = "ShuttleboxR")
```

Individual function documentation is available in R, for example:

```r
?calc_Tbreadth
help(package = "ShuttleboxR")
```
